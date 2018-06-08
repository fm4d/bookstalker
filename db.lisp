(ql:quickload "mito")
(ql:quickload "str")
(ql:quickload "alexandria")

(declaim (optimize (debug 3)))

;===========================================================

(mito:connect-toplevel :sqlite3 :database-name "/Users/kgil/goodreads.db")


(defun symbol-add-colon (symbol)
  (make-symbol (str:concat ":" (string symbol))))


(defclass edition ()
  ((title :col-type (:varchar 128)
         :initarg :title
         :accessor edition-title)
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


(defun insert-edition (alist)
  (mito:insert-dao
   (apply #'make-instance 'edition (loop for (name . val) in alist
                                         collect (alexandria:make-keyword name)
                                         collect val))))
