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


