(asdf:defsystem "bookstalker"
  :depends-on ("lquery" "str" "drakma" "plump" "cl-ppcre" "assoc-utils" "cl-dbi"
               "alexandria")
  :serial T
  :components ((:file "package")
               (:file "bookdepository")
               (:file "goodreads")
               (:file "db")
               (:file "bookstalker")))
