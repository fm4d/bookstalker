(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================


(defun divide (sequence index)
  "Divides sequence to 2 sequences after element at index."
  (let ((index (1+ index)))
    (list
     (subseq sequence 0 index)
     (subseq sequence index))))


(defun create-url-with-parameters (url parameters)
  (apply #'concat url "?"
         (mapcar (lambda (alist)
                   (concat (car alist) "=" (cdr alist) "&"))
                 parameters)))


(defun plist-alist-sym (plist)
  (loop for (k v) on plist by #'cddr
        collect (cons (read-from-string (string-downcase k)) v)))
