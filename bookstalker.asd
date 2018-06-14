(asdf:defsystem "bookstalker"
  :depends-on ("lquery" "str" "drakma" "plump" "cl-ppcre" "assoc-utils")
  :serial T
  :components ((:file "package")
               (:file "bookdepository")
               (:file "goodreads")))
