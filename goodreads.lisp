(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================



(define-condition unsupported-row-structure (error)
  ((text :initarg :text :reader text)))

(define-condition no-isbn-matched (error)
  ((text :initarg :text :reader text)))


(defun divide (sequence index)
  "Divides sequence to 2 sequences after element at index."
  (let ((index (1+ index)))
    (list
     (subseq sequence 0 index)
     (subseq sequence index))))


(defun create-url-with-parameters (url parameters)
  (apply #'concat url "?"
         (mapcar (lambda (alist)
                   (concat (car alist) "=" (cdr alist) "&"))
                 parameters)))


;; Unsused
(defun list-shelves ()
  (flexi-streams:octets-to-string
   (drakma:http-request "https://www.goodreads.com/shelf/list.xml"
                        :method :get
                        :parameters `(("key" . ,+user-key+)
                                      ("user_id" . ,+user-id+)))))


(defun all-raw-works-from-shelve (shelf-name &optional (per-page "200"))
  (labels ((raw-page-from-shelve (page)
             (flexi-streams:octets-to-string
              (drakma:http-request (create-url-with-parameters
                                    "https://www.goodreads.com/review/list"
                                    `(("v" . "2")
                                      ("per_page" . ,per-page)
                                      ("page" . ,(write-to-string page))
                                      ("shelf" . ,shelf-name)
                                      ("key" . ,+user-key+)
                                      ("id" . ,+user-id+)))
                                   :method :get)))
           (raw-works-from-page (page)
               (let ((dom (plump:parse page)))
                 (coerce ($ dom "reviews" "review") 'list))))
    (loop
      with page = 1
      with results = nil
      with res = nil
      do
         (setf res (raw-works-from-page (raw-page-from-shelve page)))
         (setf results (append results res))
         (incf page)
      when (not (= (length res) 200))
        return results)))


(defun work-extract-basic-data (work)
  `((title   . ,($ work "title_without_series" (node) (text)))
    (author  . ,($ work "authors" "name" (node) (text)))
    (id . ,(parse-integer ($ work "work" "id" (node) (text))))))


(defun all-works-from-shelve (&optional (shelf-name "to-read"))
  "Download all works in shelve via API and return them in alist."
  (mapcar #'work-extract-basic-data (all-raw-works-from-shelve shelf-name)))


(defun all-raw-editions (work-id per-page)
  "Manualy parse HTML, editions-api is not for public.
 Official per-page limit is 100, but it works :)"
  (let* ((url (format nil
                      "https://www.goodreads.com/work/editions/~a?per_page=~a"
                      work-id
                      per-page))
         (html (drakma:http-request url :method :get))
         (root (plump:parse html)))
    ($ root "div.editionData")))


(defun process-edition (edition)
  (handler-case
      (let* ((rows (edition-dispart-rows edition))
             (link ($ (inline (aget rows 'title))
                     "a.bookTitle" (attr "href") (node)))
             (data (edition-rows-to-strings rows)))
        (append
         `((title    . ,(parse-edition-title (aget data 'title )))
           (id       . , (parse-edition-id link))
           (language . ,(aget data 'language)))
         (list (assoc 'format (parse-edition-format (aget data 'format))))
         (parse-edition-isbns (aget data 'isbns)
                              (read-from-string (subseq (aget data 'isbn-title)
                                                        0
                                                        (- (length (aget data 'isbn-title)) 1))))))
    (unsupported-row-structure (condition)
      (print (text condition))
      nil)
    (no-isbn-matched (condition)
      (print (text condition))
      nil)))


(defun all-editions (work-id &key (per-page 999) (language nil))
  (let ((editions (remove-if #'null
                             (map 'list
                                  #'process-edition
                                  (all-raw-editions work-id per-page)))))
    (if language
        (remove-if (lambda (e) (string/= language (aget e 'language)))
                   editions)
        editions)))


(defun edition-dispart-details (details)
  (let* ((data-rows ($ details "div.dataRow"))
         (data-values ($ data-rows "div.dataValue")))
    (if (/= (length data-rows) 4)
        (error 'unsupported-row-structure))
    `((authors    . ,(elt data-values 0))
      (isbns      . ,(elt data-values 1))
      (isbn-title . ,($ (inline (elt data-rows 1)) "div.dataTitle" (node)))
      (language   . ,(elt data-values 2))
      (rating     . ,(elt data-values 3)))))


(defun edition-dispart-rows (raw-edition)
  (let* ((data-rows ($ raw-edition (children)
                      (filter (lambda (n) (string= (lquery-funcs:attr n "class")
                                                   "dataRow")))))
         (rows-count (length data-rows))
         (title (elt data-rows 0)))
    (if (not (member rows-count '(3 4)))
        (error 'unsupported-row-structure
               :text (format nil "ID: ~a | Unsupported edition structure."
                             (parse-edition-id ($ title "a.bookTitle"
                                                 (attr "href") (node))))))
    (append `((title     . ,title)
              (published . ,(if (= rows-count 4) (elt data-rows 1)))
              (format    . ,(elt data-rows (- rows-count 2))))
            (handler-case (edition-dispart-details
                           ($ raw-edition "div.hideDetails" (node)))
              (unsupported-row-structure ()
                (error 'unsupported-row-structure
                       :text (format nil
                                     "ID: ~a | Unsupported edition details structure."
                                     (parse-edition-id ($ title "a.bookTitle"
                                                         (attr "href") (node))))))))))


(defun edition-rows-to-strings (row-pairs)
  (labels ((row-to-string (row)
             (join " " (mapcar #'trim
                               (remove-if #'blankp
                                          (lines ($ row (text) (node))))))))
    (mapcar (lambda (row-pair)
              (destructuring-bind (name . row) row-pair
                `(,name . ,(row-to-string row))))
            row-pairs)))


(defun try-matches (item patterns)
  (loop for (id . p) in patterns
        with groups and match
        do
           (multiple-value-setq  (match groups) (cl-ppcre:scan-to-strings p item))
        if groups
          return (cons id groups)))


(defun split-edition (edition)
  (let* ((items (split ", " edition))
         (items-len (length items)))
    (if (> items-len 3)
        (destructuring-bind (edition format-pages)
            (divide items (- items-len 3))
          (cons (join ", " edition) format-pages))
        items)))


(defun parse-edition-title (title)
  "Strip edition title of series information in parentheses."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^(.*)(?:\\s\\(.*\\)){1}$" title)
    (if (null groups)
        (multiple-value-bind (match groups)
            (cl-ppcre:scan-to-strings "^(.*)$" title)
          (elt groups 0))
        (elt groups 0))))


(defun parse-edition-id (href)
  (multiple-value-bind (match result)
      (cl-ppcre:scan-to-strings "^/book/show/(\\d+).*$" href)
    (parse-integer (elt result 0))))


(defun parse-edition-format (format)
  "Parse string with edition, format and pages into alist with those keys."
  (let ((items (split-edition format))
        (patterns
          `((format . ,(concat
                        "(^Paperback$|^Hardcover$|^Kindle\\sEdition$|^ebook$|"
                        "^Audiobook$|^Mass\\sMarker\\sPaperback$|^Audio\\sCD$|"
                        "^nook$|^Library\\sBinding$|^Audio\\sCassette$|"
                        "^Audible\\sAudio$|^CD-ROM$|^MP3\\sCD$|^Board\\sbook$|"
                        "^Leather\\sBound$|^Unbound$|^Spiral-bound$|"
                        "^Unknown\\sBinding$)"))
            (pages  . "(\\d+(?=\\spages))")
            (edition . "(.*)"))))
    (mapcar (lambda (x)
              (destructuring-bind (pattern . item) (try-matches x patterns)
                (cons pattern (elt item 0))))
            items)))


;; (defun parse-edition-isbns (isbns isbn-title)
;;   "Parse string with isbn+isbn13/asin into alist with all those keys."
;;   (let* ((patterns '((isbn   . "^(\\w{10})\\s\\(ISBN13:\\s(\\d{13})\\)$")
;;                     (isbn13 . "^(\\d{13})$")
;;                      (asin   . "^(\\w{10})$")))
;;          (result (try-matches isbns patterns)))
;;     (if (null result)
;;         (error 'no-isbn-matched
;;                :text (format nil "ISBN ~a is not supported" isbns)))
;;     (destructuring-bind (type . groups) result
;;       (let ((res `(,(cons 'isbn nil) ,(cons 'isbn13 nil) ,(cons 'asin nil))))
;;         (cond ((eq type 'isbn) (progn
;;                                  (setf (aget res 'isbn) (elt groups 0))
;;                                  (setf (aget res 'isbn13) (elt groups 1))))
;;               ((eq type 'isbn13) (setf (aget res 'isbn13) (elt groups 0)))
;;               ((eq type 'asin) (setf (aget res 'asin) (elt groups 0))))
;;         res))))


(defun parse-edition-isbns (isbns isbn-title)
  (let* ((patterns '((isbn . ((isbn+isbn13 . "^(\\w{10})\\s\\(ISBN13:\\s(\\d{13})\\)$")
                               (isbn        . "^(\\w{10})$")))
                     (isbn13 . ((isbn13 . "^(\\d{13})$")))
                     (asin   . ((asin   . "^(\\w{10})$")))))
         (result (try-matches isbns (aget patterns isbn-title))))
    (if (null result)
        (error 'no-isbn-matched
               :text (format nil "ISBN ~a is not supported" isbns)))
    (destructuring-bind (type . groups) result
      (let ((res `(,(cons 'isbn nil) ,(cons 'isbn13 nil) ,(cons 'asin nil))))
        (cond ((eq type 'isbn+isbn13) (progn
                                        (setf (aget res 'isbn) (elt groups 0))
                                        (setf (aget res 'isbn13) (elt groups 1))))
              ((eq type 'isbn) (setf (aget res 'isbn) (elt groups 0)))
              ((eq type 'isbn13) (setf (aget res 'isbn13) (elt groups 0)))
              ((eq type 'asin) (setf (aget res 'asin) (elt groups 0))))
        res))))




