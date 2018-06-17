(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================


(define-condition work-not-found (error)
  ((text :initarg :text :reader text)))

(define-condition work-already-exists (error)
  ((text :initarg :text :reader text)))

(define-condition edition-already-exists (error)
  ((text :initarg :text :reader text)))


(defconstant tables
  `((work . ,(concat "CREATE TABLE work ("
                     "    id int NOT NULL,"
                     "    title varchar(255) NOT NULL,"
                     "    author varchar(255) NOT NULL,"
                     "    last_check int NOT NULL,"
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

(defun recreate-tables ()
  (loop for (name . query) in tables
        do (execute (format nil "DROP TABLE IF EXISTS ~a ;" (write-to-string name)))
           (execute query)))


(defun execute (query)
  (dbi:with-connection (conn :sqlite3 :database-name "goodreads_db.db")
    (let ((res (dbi:execute (dbi:prepare conn query))))
      (if res
          (mapcar #'plist-alist (dbi:fetch-all res))
          nil))))


(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
        collect (cons (read-from-string (string-downcase k)) v)))

(defun check-nil (item)
  (if (null item) "" item))


(defun insert-work (work)
  (execute (format nil (concat "INSERT INTO work (id, title, author, last_check) "
                               "VALUES (\'~d\', \'~a\', \'~a\', \'~d\');")
                   (aget work 'id)
                   (aget work 'title)
                   (aget work 'author)
                   (get-universal-time))))


(defun insert-edition (edition work-id)
  (execute
   (format nil
           (concat "INSERT INTO edition (id, work_id, language, format, isbn, isbn13, asin) "
                   "VALUES (\'~d\', \'~d\', \'~a\', \'~a\', \'~a\', \'~a\', \'~a\')")
           (aget edition 'id)
           work-id
           (aget edition 'language)
           (aget edition 'format)
           (check-nil (aget edition 'isbn))
           (check-nil (aget edition 'isbn13))
           (check-nil (aget edition 'asin)))))


(defun delete-all-editions (work-id)
  (execute
   (format nil
           "DELETE FROM edition WHERE work_id = ~d ;"
           work-id)))


(defun insert-work-with-editions (work editions)
  (insert-work work)
  (loop for e in editions
        do (insert-edition e (aget work 'id))))


(defun select-editions (work-id &optional
                                (language "English")
                                  (format '("Paperback" "Hardcover")))
  (execute
   (format nil
           (concat "SELECT edition.isbn, edition.isbn13, edition.asin FROM edition "
                   "JOIN work ON edition.work_id = work.id "
                   "AND work.id = ~d;")
           work-id)))


(defun select-work (work-id)
  (execute
   (format nil
           "SELECT * FROM work WHERE id = ~d ;"
           work-id)))

(defun load-new-work (work)
  (insert-work-with-editions work (all-editions (aget work 'id))))

(defun update-work (work)
  (let ((id (aget work 'id)))
    (delete-all-editions id)
    (loop for e in (all-editions id)
          do (insert-edition e id))
    (execute (format nil "UPDATE work SET last_check = ~d WHERE id = ~d"
                     (get-universal-time) id))))


(defun work-in-db-p (work)
  (if (select-work (aget work 'id))
      t
      nil))


(defun work-outdated-p (work)
  (let* ((id (aget work 'id))
         (res (first (select-work id))))
    (if (null res)
        nil
        (> (- (get-universal-time) (aget res 'last_check))
           +update-frequency+))))

