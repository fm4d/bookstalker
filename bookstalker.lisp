(in-package :bookstalker)

(declaim (optimize (debug 3)))

;===========================================================

(defparameter +user-id+ "76812964")
(defparameter +user-key+ "OY2rDbQV8HRskYZNMEkUPA")
(defparameter +update-frequency+ 604800)


(defun load-shelve-in-db (&optional (shelf-name "to-read"))
  (let* ((works (all-works-from-shelve shelf-name))
         (outdated-works (remove-if-not #'work-outdated-p works))
         (new-works (remove-if #'work-in-db-p works)))
    (mapc #'load-new-work new-works)
    (mapc #'update-work outdated-works)))


;; (defun ensure-editions (work-id)
;;   (let ((work (select-work work-id)))
;;     (if (not work) 


;; (defun gather-prices (work-id &optional (format '("Paperback" "Hardcover")) (language "English"))
;;   ()


