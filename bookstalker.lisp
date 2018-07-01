(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================


(defun load-configuration (&optional (filename "config.lisp"))
  (handler-case (load filename)
      (sb-int:simple-file-error (c)
        (print "Unable to load configuration, file not found."))))


(defun load-shelve-in-db (&optional (shelf-name "to-read"))
  (let* ((works (all-works-from-shelve shelf-name))
         (outdated-works (remove-if-not #'work-outdated-p works))
         (new-works (remove-if #'work-in-db-p works)))
    (mapc #'load-new-work new-works)
    (mapc #'update-work outdated-works)))


(defun largest-bd-discount (&optional (shelf-name "to-read")
                              (language "English")
                              (formats '("Paperback" "Hardcover")))
  (labels ((isbn-or-isbn13 (book)
             (let ((isbn (aget book 'isbn))
                   (isbn13 (aget book 'isbn13)))
               (if isbn isbn (if isbn13 isbn13 nil)))))
    (remove-if #'null
               (mapcar #'isbn-or-isbn13
                       (apply #'append
                              (mapcar (lambda (id)
                                        (all-editions id :language language :formats formats))
                                      (mapcar (lambda (w)
                                                (aget w 'id)) (all-works-from-shelve shelf-name)))))))

