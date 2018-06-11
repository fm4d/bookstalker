(ql:quickload "drakma")
(ql:quickload "plump")
(ql:quickload "lquery")
(ql:quickload "str")
(ql:quickload "cl-ppcre")
(ql:quickload "assoc-utils")

(declaim (optimize (debug 3)))

;===========================================================

(defparameter +user-id+ "76812964")
(defparameter +user-key+ "OY2rDbQV8HRskYZNMEkUPA")

(define-condition unsupported-row-structure (error)
  ((text :initarg :text :reader text)))

(define-condition unmatched-isbn-asin (error)
  ((text :initarg :text :reader text)))


(defun divide (sequence index)
  "Divides sequence to 2 sequences after element at index."
  (let ((index (1+ index)))
    (list
     (subseq sequence 0 index)
     (subseq sequence index))))


(defun create-url-with-parameters (url parameters)
  (apply #'str:concat url "?"
         (mapcar (lambda (alist)
                   (str:concat (car alist) "=" (cdr alist) "&"))
                 parameters)))


(defun get-list-of-shelves ()
  (flexi-streams:octets-to-string
   (drakma:http-request "https://www.goodreads.com/shelf/list.xml"
                        :method :get
                        :parameters `(("key" . ,+user-key+)
                                      ("user_id" . ,+user-id+)))))


(defun get-raw-page-from-shelve (shelf-name &optional (page "1") (per-page "200"))
  (flexi-streams:octets-to-string
   (drakma:http-request (create-url-with-parameters
                         "https://www.goodreads.com/review/list"
                         `(("v" . "2")
                           ("per_page" . ,per-page)
                           ("page" . ,page)
                           ("shelf" . ,shelf-name)
                           ("key" . ,+user-key+)
                           ("id" . ,+user-id+)))
                        :method :get)))


(defun get-raw-books (page)
  (let ((dom (plump:parse page)))
    (coerce (lquery:$ dom "reviews" "review") 'list)))


(defun get-all-raw-books-from-shelve (&optional (shelf-name "to-read"))
  (loop
    with page = 1
    with results = nil
    with res = nil
    do
       (setf res (get-raw-books
                  (get-raw-page-from-shelve shelf-name
                                            (write-to-string page)
                                            "200")))
       (setf results (append results res))
       (incf page)
    when (not (= (length res) 200))
      return results))


(defun get-book-basic-data (book)
  `((title   . ,(lquery:$ book "title_without_series" (node) (text)))
    (author  . ,(lquery:$ book "authors" "name" (node) (text)))
    (work-id . ,(parse-integer (lquery:$ book "work" "id" (node) (text))))))


(defun get-all-books-from-shelve (&optional (shelf-name "to-read"))
  "Download all books in shelve via API and return them in alist."
  (mapcar #'get-book-basic-data (get-all-raw-books-from-shelve shelf-name)))




(defun get-all-raw-editions (work-id per-page)
  "Manualy parse HTML, editions-api is not for public.
 Official per-page limit is 100, but it works :)"
  (let* ((url (format nil
                      "https://www.goodreads.com/work/editions/~a?per_page=~a"
                      work-id
                      per-page))
         (html (drakma:http-request url :method :get))
         (root (plump:parse html)))
    (lquery:$ root "div.editionData")))


(defun process-edition (edition)
  (handler-case
      (let* ((rows (edition-dispart-rows edition))
             (book-id (lquery:$ (inline (cdr (assoc 'title rows)))
                        "a.bookTitle" (attr "href") (node)))
             (data (edition-rows-to-strings rows)))
        (append
         `((title    . ,(parse-edition-title (cdr (assoc 'title data ))))
           (book-id  . , (parse-edition-book-id book-id))
           (language . ,(cdr (assoc 'language data))))
         (list (assoc 'format (parse-edition-format (cdr (assoc 'format data)))))
         (parse-edition-isbn-asin (cdr (assoc 'isbn-asin data)))))
    (unsupported-row-structure () nil)
    (unmatched-isbn-asin () nil)))


(defun get-all-editions (work-id &key (per-page 999) (filter-language nil))
  (let ((editions (remove-if #'null
                             (map 'list
                                  #'process-edition
                                  (get-all-raw-editions work-id per-page)))))
    (if filter-language
        (remove-if (lambda (e) (string/= filter-language (cdr (assoc 'language e))))
                   editions)
        editions)))


(defun edition-dispart-details (details)
  (let ((data-rows (lquery:$ details "div.dataRow" "div.dataValue")))
    (if (/= (length data-rows) 4)
        (error 'unsupported-row-structure :text "Wrong amount of rows in details"))
    `((authors   . ,(elt data-rows 0))
      (isbn-asin . ,(elt data-rows 1))
      (language  . ,(elt data-rows 2))
      (rating    . ,(elt data-rows 3)))))


(defun edition-dispart-rows (raw-edition)
  (let* ((data-rows (lquery:$ raw-edition (children)
                      (filter (lambda (n) (string= (lquery-funcs:attr n "class")
                                                   "dataRow")))))
         (rows-count (length data-rows)))
    (if (not (member rows-count '(3 4)))
        (error 'unsupported-row-structure :text "Unsupported edition rows amount."))
    (append `((title     . ,(elt data-rows 0))
              (published . ,(if (= rows-count 4) (elt data-rows 1)))
              (format    . ,(elt data-rows (- rows-count 2))))
            (edition-dispart-details (lquery:$ raw-edition "div.hideDetails" (node))))))


(defun edition-rows-to-strings (row-pairs)
  (mapcar (lambda (row-pair)
            (destructuring-bind (name . row) row-pair
              `(,name . ,(str:join " " (mapcar #'str:trim
                                               (remove-if #'str:blankp
                                                          (str:lines
                                                           (lquery:$ row (text) (node)))))))))
          row-pairs))


(defun parse-edition-title (title)
  "Strip edition title of series information in parentheses."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^(.*)(?:\\s\\(.*\\)){1}$" title)
    (if (null groups)
        (multiple-value-bind (match groups)
            (cl-ppcre:scan-to-strings "^(.*)$" title)
          (elt groups 0))
        (elt groups 0))))


(defun parse-edition-book-id (href)
  (multiple-value-bind (match result)
      (cl-ppcre:scan-to-strings "^/book/show/(\\d+).*$" href)
    (parse-integer (elt result 0))))


(defun try-matches (item patterns)
  (loop for (id . p) in patterns
        with groups and match
        do
           (multiple-value-setq  (match groups) (cl-ppcre:scan-to-strings p item))
        if groups
          return (cons id groups)))


(defun split-edition (edition)
  (let* ((items (str:split ", " edition))
        (items-len (length items)))
    (if (> items-len 3)
        (destructuring-bind (edition format-pages)
            (divide items (- items-len 3))
          (cons (str:join ", " edition) format-pages))
        items)))


(defun parse-edition-format (format)
  "Parse string with edition, format and pages into alist with those keys."
  (let ((items (split-edition format))
        (patterns
          `((format . ,(str:concat
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


(defun parse-edition-isbn-asin (isbn-asin)
  "Parse string with isbn+isbn13/asin into alist with all those keys."
  (let* ((patterns '((isbn   . "^(\\w{10})\\s\\(ISBN13:\\s(\\d{13})\\)$")
                    (isbn13 . "^(\\d{13})$")
                     (asin   . "^(\\w{10})$")))
         (result (try-matches isbn-asin patterns)))
    (if (null result)
        (error 'unmatched-isbn-asin
               :text (format nil "~a is not supported" isbn-asin)))
    (destructuring-bind (type . groups) result
      (let ((res `(,(cons 'isbn nil) ,(cons 'isbn13 nil) ,(cons 'asin nil))))
        (cond ((eq type 'isbn) (progn
                                 (setf (assoc-utils:aget res 'isbn) (elt groups 0))
                                 (setf (assoc-utils:aget res 'isbn13) (elt groups 1))))
              ((eq type 'isbn13) (setf (assoc-utils:aget res 'isbn13) (elt groups 0)))
              ((eq type 'asin) (setf (assoc-utils:aget res 'asin) (elt groups 0))))
        res))))
