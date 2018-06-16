(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================


(define-condition work-already-exists (error)
  ((text :initarg :text :reader text)))

(define-condition edition-already-exists (error)
  ((text :initarg :text :reader text)))


(defconstant tables
  `((work . ,(concat "CREATE TABLE work ("
                     "    id int NOT NULL,"
                     "    title varchar(255) NOT NULL,"
                     "    author varchar(255) NOT NULL,"
                     "    PRIMARY KEY (id)"
                     ");"))
    (edition . ,(concat "CREATE TABLE edition ("
                        "    id int NOT NULL,"
                        "    work_id int NOT NULL,"
                        "    language varchar(100) NOT NULL,"
                        "    format varchar(100) NOT NULL,"
                        "    isbn varchar(10),"
                        "    isbn13 varchar(13),"
                        "    asin varchar(10),"
                        "    PRIMARY KEY (id),"
                        "    FOREIGN KEY (work_id) REFERENCES work(id)"
                        ");"))))



(defun execute (query)
  (dbi:with-connection (conn :sqlite3 :database-name "goodreads_db.db")
    (dbi:execute (dbi:prepare conn query))))


(defun check-nil (item)
  (if (null item) "" item))


(defun symbol-add-colon (symbol)
  (make-symbol (str:concat ":" (string symbol))))


(defun insert-work (work)
  (execute (format nil (concat "INSERT INTO work (id, title, author)"
                               "VALUES (\'~d\', \'~a\', \'~a\');")
                   (aget work 'id)
                   (aget work 'title)
                   (aget work 'author))))


(defun insert-edition (edition work-id)
  (execute
   (format nil
           (concat "INSERT INTO edition (id, work_id, language, format, isbn, isbn13, asin)"
                   "VALUES (\'~d\', \'~d\', \'~a\', \'~a\', \'~a\', \'~a\', \'~a\')")
           (aget edition 'id)
           work-id
           (aget edition 'language)
           (aget edition 'format)
           (check-nil (aget edition 'isbn))
           (check-nil (aget edition 'isbn13))
           (check-nil (aget edition 'asin)))))



