(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================


;;TODO more then one page
(defun search-by-isbns (isbns currency)
  (let ((url (create-url-with-parameters "https://www.bookdepository.com/search"
                                          `(("searchTerm" . "")
                                            ("searchTitle" . "")
                                            ("searchAuthor" . "")
                                            ("searchPublisher" . "")
                                            ("searchIsbn" . ,(join "+AND+" isbns))
                                            ("searchLang" . "")
                                            ("selectCurrency" . ,currency)
                                            ("advanced" . "true"))))
        (currencies '("USD" "GBP" "EUR" "CZK")))
    (if (not (member currency currencies :test #'string=))
        (error "Currency is not supported."))
    (let* ((raw-response (drakma:http-request url :method :get))
           (root (plump:parse raw-response)))
      root)))


(defun process-single-result (result)
  (let ((unavailable  ($ result "p.red-text" (node))))
    (if (and unavailable (string= (trim ($ unavailable (text))) "Currently unavailable"))
        nil))
  (let ((add-to-basket  ($ result "div.copy-content" "a.add-to-basket" (node))))
    `((isbn     . ,($ add-to-basket (attr "data-isbn")))
      (currency . ,($ add-to-basket (attr "data-currency")))
      (price    . ,($ add-to-basket (attr "data-price"))))))


(defun process-multiple-results (results)
  (let ((add-to-baskets ($ results "div.btn-wrap" "a.add-to-basket")))
    (mapcar (lambda (basket)
              `((isbn     . ,($ basket (attr "data-isbn")))
                (currency . ,($ basket (attr "data-currency")))
                (price    . ,($ basket (attr "data-price")))))
            add-to-baskets)))


(defun route-results (results)
  (let ((search-results ($ results "div.main-content" "h1" (node)))
        (advanced-search ($ results "div.content-wrap" "content" "h1" (node)))
        (single-item ($ results "div.page-slide" "item-wrap" (node))))
    (if (string= (trim ($ advanced-search (text))) "Advanced search") 'none)
    (if (string= (trim ($ search-results (text))) "Search results") 'multiple)
    (if single-item 'single)
    (error "Unable to route results.")))



;; (defun process-book (book)
;;   (let ((price (words ($ book "p.price" (node)
;;                             #'(lambda (el) (if el (lquery-funcs:text el))))))
;;         (title ($ book "meta[itemprop=name]" (attr "content") (node)))
;;         (author ($ book "span[itemprop=author]" (attr "itemscope") (node)))
;;         (isbn ($ book "meta[itemprop=isbn]" (attr "content") (node)))
;;         (format (trim ($ book "p.format" (node) (text)))))
;;     `((price . ,(if price (divide price 1)))
;;       (title . ,title)
;;       (author . ,author)
;;       (format . ,format)
;;       (isbn . ,isbn))))



(defun book-suitable-p (book)
  "Only available books (with price) and only Paperbacks and Hardbacks, no CDs etc."
   (assoc 'price book))



; Fix search for one book
;; (defun find-all-isbns (isbns)
;;   (remove-if-not #'book-suitable-p
;;                  (map 'list #'process-book (search-by-isbns isbns))))


(defun find-all-isbns (isbns &optional (currency "USD"))
  (let* ((results (search-by-isbns isbns currency))
         (route (route-results results)))
    (cond ((eq route 'none) nil)
          ((eq route 'single) (process-single-result results))
          ((eq route 'multiple) (process-multiple-results results)))))
