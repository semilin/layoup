(in-package :layoup)

(defstruct layout
  name
  author
  link
  year
  formats)

(defun key-matrix (row-list)
  "Converts a list of rows into a 2d key matrix. Each row is a string with no spaces, e.g. \"asdfghjkl;\" "
  (mapcar (lambda (row) (coerce row 'list)) row-list))

(defun pos-to-key (pos keys)
  "Returns the key at POS on KEYS."
  (declare (type pos pos))
  (nth (pos-col pos)
       (nth (pos-row pos) keys)))

(defun keys-total (keys corpus)
  "Returns the total frequency count of all the keys on KEYS from the
CORPUS. Used for calculating metric percentages."
  (let ((total 0))
    (loop for row in keys do
      (loop for key in row do
	(incf total (gethash key (corpus-chars corpus)))))
    total))

;; (defun swap-keys (a b keys)
;;   "Destructively modifies original KEYS by swapping A and B."
;;   (declare (type pos a b))
;;   (rotatef (nth )))
