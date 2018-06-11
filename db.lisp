(ql:quickload "mito")
(ql:quickload "str")
(ql:quickload "alexandria")

(declaim (optimize (debug 3)))

;===========================================================

(mito:connect-toplevel :sqlite3 :database-name "/Users/kgil/goodreads.db")


(defun symbol-add-colon (symbol)
  (make-symbol (str:concat ":" (string symbol))))


(defclass edition ()
  ((title :col-type (:varchar 512)
         :initarg :title
          :accessor edition-title)
   (book_id :col-type (:integer)
            :initarg :book_id
            :accessor edition-book_id)
   (work_id :references (book work_id)
          :initarg :work_id
          :accessor edition-work_id)
   (language :col-type (:varchar 128)
             :initarg :language
             :accessor edition-language)
   (format :col-type (or (:varchar 128) :null)
           :initarg :format
           :accessor edition-format)
   (isbn :col-type (or (:integer 10) :null)
         :initarg :isbn
         :accessor edition-isbn)
   (isbn13 :col-type (or (:integer 13) :null)
           :initarg :isbn13
           :accessor edition-isbn13)
   (asin :col-type (or (:varchar 10) :null)
         :initarg :asin
         :accessor edition-asin))
  (:metaclass mito:dao-table-class))


(defclass book ()
  ((title :col-type (:varchar 512)
          :initarg :title
          :accessor book-title)
   (author :col-type (:varchar 128)
           :initarg :author
           :accessor book-author)
   (work_id :col-type (:integer)
          :initarg :work_id
            :accessor work_id))
  (:unique-keys work_id)
  (:metaclass mito:dao-table-class))


(defun ensure-tables (&rest classes)
  (mapc #'mito:ensure-table-exists classes))


(defun insert-edition (edition work-id)
  (let ((edition (cons `(work_id ,work-id) edition)))
    (apply #'mito:create-dao 'edition
           (loop for (name . val) in edition
                 collect (alexandria:make-keyword name)
                 collect val))))


(defun insert-book (book)
  (mito:create-dao 'book
                   :title (cdr (assoc 'title book))
                   :author (cdr (assoc 'author book))
                   :work_id (cdr (assoc 'work-id book))))

