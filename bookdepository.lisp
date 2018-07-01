(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================


;;TODO more then one page
(defun search-by-isbns (isbns currency &optional (page 1))
  (let ((url (create-url-with-parameters "https://www.bookdepository.com/search"
                                          `(("searchTerm" . "")
                                            ("searchTitle" . "")
                                            ("searchAuthor" . "")
                                            ("searchPublisher" . "")
                                            ("searchIsbn" . ,(join "+AND+" isbns))
                                            ("searchLang" . "")
                                            ("selectCurrency" . ,currency)
                                            ("advanced" . "true")
                                            ("page" . ,(write-to-string page)))))
        (currencies '("USD" "GBP" "EUR" "CZK")))
    (if (not (member currency currencies :test #'string=))
        (error "Currency is not supported."))
    (let* ((raw-response (drakma:http-request url :method :get))
           (root (plump:parse raw-response)))
      root)))


(defun process-single-result (result)
  (let ((unavailable  ($ result "p.red-text.bold" (node)))
        (add-to-basket  ($ result "div.checkout-tools" "a.add-to-basket" (node)))
        (savings-splat ($ result "div.savings-splat" (node))))
    (if (and unavailable
             (string= ($ unavailable (node) (text)) "Currently unavailable"))
        nil
        `(((isbn     . ,($ add-to-basket (node) (attr "data-isbn")))
           (currency . ,($ add-to-basket (node) (attr "data-currency")))
           (price    . ,($ add-to-basket (node) (attr "data-price")))
           (discount . ,(if (null savings-splat) 0
                            (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings
                                                                 "^(\\d{1,3})%off$"
                                                                 ($ savings-splat (node) (text)))
                              (elt groups 0)))))))))


(defun process-book-item (item)
  (let ((basket ($ item "div.btn-wrap" "a.add-to-basket" (node)))
        (savings-splat ($ item "div.savings-splat" (node))))
    (if (null basket) nil
        `((isbn     . ,($ basket (node) (attr "data-isbn")))
          (currency . ,($ basket (node) (attr "data-currency")))
          (price    . ,($ basket (node) (attr "data-price")))
          (discount . ,(if (null savings-splat) 0
                           (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings
                                                                "^(\\d{1,3})%off$"
                                                                ($ savings-splat (node) (text)))
                             (elt groups 0))))))))


(defun route-results (raw-response)
  (let ((search-results ($ raw-response "div.main-content" "h1" (node)))
        (advanced-search ($ raw-response "div.content-wrap" "div.content" "h1" (node)))
        (single-item ($ raw-response "div.page-slide" "div.item-wrap" (node))))

    (cond ((and advanced-search (string= ($ advanced-search (node) (text))
                                         "Advanced Search"))
           'none)
          ((and search-results (string= (trim ($ search-results (node) (text))) "Search results"))
           'multiple)
          (single-item 'single)
          (t (error "Unable to route results.")))))


(defun find-all-isbns (isbns &optional (currency "USD") (chunk-size 90))
  (labels ((+-with-max (x y max)
             (let ((res (+ x y)))
               (if (> res max) max res))))
    (let ((chunks (loop with len = (length isbns)
                        for i from 0 to len by chunk-size
                        collect (subseq isbns i (+-with-max i chunk-size len)))))
      (apply #'append
             (mapcar (lambda (ch) (find-isbns ch currency)) chunks)))))


(defun find-isbns (isbns currency)
    (let* ((raw-response (search-by-isbns isbns currency))
           (route (route-results raw-response)))
      (cond ((eq route 'none) nil)
            ((eq route 'single) (process-single-result raw-response))
            ((eq route 'multiple)
             (loop append (remove-if #'null
                                     (mapcar #'process-book-item
                                             (get-books-from-all-pages raw-response isbns currency))))))))


(defun get-books-from-all-pages (raw-response isbns currency)
  (let ((page-count (ceiling (multiple-value-bind (match groups)
                                 (cl-ppcre:scan-to-strings
                                  "^Showing (\\d{1,3}) to (\\d{1,3}) of (\\d{1,5}) results$"
                                  (join " " (words ($ raw-response "div.search-info.left-content"
                                                 (node) (text)))))
                               (parse-integer (elt groups 2)))
                             30)))
        (loop for i from 1 to page-count
              append (if (= i 1) (coerce ($ raw-response "div.book-item") 'list)
                          (coerce ($ (inline (search-by-isbns isbns currency i)) "div.book-item")
                                  'list)))))
