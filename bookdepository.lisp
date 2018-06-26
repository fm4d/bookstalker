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
  (let ((unavailable  ($ result "p.red-text.bold" (node)))
        (add-to-basket  ($ result "div.checkout-tools" "a.add-to-basket" (node))))
    (if (and unavailable
             (string= ($ unavailable (node) (text)) "Currently unavailable"))
        nil
        `(((isbn     . ,($ add-to-basket (node) (attr "data-isbn")))
          (currency . ,($ add-to-basket (node) (attr "data-currency")))
          (price    . ,($ add-to-basket (node) (attr "data-price"))))))))


(defun process-multiple-results (results)
  (let ((add-to-baskets ($ results "div.btn-wrap" "a.add-to-basket")))
    (map 'list (lambda (basket)
                 `((isbn     . ,($ basket (node) (attr "data-isbn")))
                   (currency . ,($ basket (node) (attr "data-currency")))
                   (price    . ,($ basket (node) (attr "data-price")))))
         add-to-baskets)))


(defun route-results (results)
  (let ((search-results ($ results "div.main-content" "h1" (node)))
        (advanced-search ($ results "div.content-wrap" "div.content" "h1" (node)))
        (single-item ($ results "div.page-slide" "div.item-wrap" (node))))

    (cond ((and advanced-search (string= ($ advanced-search (node) (text))
                                         "Advanced Search"))
           'none)
          ((and search-results (string= (trim ($ search-results (node) (text))) "Search results"))
           'multiple)
          (single-item 'single)
          (t (error "Unable to route results.")))))



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



(defun find-all-isbns (isbns &optional (currency "USD"))
  (let* ((results (search-by-isbns isbns currency))
         (route (route-results results)))
    (cond ((eq route 'none) nil)
          ((eq route 'single) (process-single-result results))
          ((eq route 'multiple) (process-multiple-results results)))))
