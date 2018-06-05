(ql:quickload :drakma)
(ql:quickload :plump)
(ql:quickload :str)
(ql:quickload :lquery)

(declaim (optimize (debug 3)))

;===========================================================

(defun divide (sequence index)
  "Divides sequence to 2 sequences after element at index."
  (let ((index (1+ index)))
    (list
     (subseq sequence 0 index)
     (subseq sequence index))))

(defun create-search-url (title author)
  (let* ((url "https://www.bookdepository.com/search?searchTerm=")
        (words (append (str:words title) (str:words author)))
        (query (apply #'str:concat
                (loop
                  for w in words
                  and idx from 1
                  collect w
                  if (< idx (length words)) collect "+"))))
    (str:concat url query)))


;; TODO more then one page
(defun search-for-book (title author)
  (let* ((raw-response (drakma:http-request (create-search-url title author) :method :get))
         (root (plump:parse raw-response))
         (books (lquery:$ root "div.book-item")))
    books))


(defun process-book (book)
  (let ((price (str:words (lquery:$ book "p.price" (node)
                            #'(lambda (el) (if el (lquery-funcs:text el))))))
        (title (lquery:$ book "meta[itemprop=name]" (attr "content") (node)))
        (author (lquery:$ book "span[itemprop=author]" (attr "itemscope") (node)))
        (isbn (lquery:$ book "meta[itemprop=isbn]" (attr "content") (node)))
        (format (str:trim (lquery:$ book "p.format" (node) (text)))))
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
  (remove-if-not #'suitablep (map 'list #'process-book (search-for-book title author))))
