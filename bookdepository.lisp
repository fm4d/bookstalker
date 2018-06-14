(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================


(defun create-search-url (title author)
  (let* ((url "https://www.bookdepository.com/search?searchTerm=")
        (words (append (words title) (words author)))
        (query (apply #'concat
                (loop
                  for w in words
                  and idx from 1
                  collect w
                  if (< idx (length words)) collect "+"))))
    (concat url query)))


;; TODO more then one page
(defun search-for-book (title author)
  (let* ((raw-response (drakma:http-request (create-search-url title author)
                                            :method :get))
         (root (plump:parse raw-response))
         (books ($ root "div.book-item")))
    books))


(defun process-book (book)
  (let ((price (words ($ book "p.price" (node)
                            #'(lambda (el) (if el (lquery-funcs:text el))))))
        (title ($ book "meta[itemprop=name]" (attr "content") (node)))
        (author ($ book "span[itemprop=author]" (attr "itemscope") (node)))
        (isbn ($ book "meta[itemprop=isbn]" (attr "content") (node)))
        (format (trim ($ book "p.format" (node) (text)))))
    `((price . ,(if price (divide price 1)))
      (title . ,title)
      (author . ,author)
      (format . ,format)
      (isbn . ,isbn))))


(defun suitablep (book)
  "Only available books (with price) and only Paperbacks and Hardbacks, no CDs etc."
  (and
   (assoc 'price book)
   (not (member (cdr (assoc 'format book)) '("CD-Audio") :test #'string=))))


(defun find-all-books (title author)
  (remove-if-not #'suitablep
                 (map 'list #'process-book (search-for-book title author))))
