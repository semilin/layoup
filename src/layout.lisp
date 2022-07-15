(in-package :layoup)

(defstruct layout
  name
  author
  link
  year
  formats)

(defun key-matrix (row-list)
  "Converts a list of rows into a 2d key array. Each row is a string with no spaces, e.g. \"asdfghjkl;\" "
  (let ((l (mapcar (lambda (row) (coerce row 'list)) row-list)))
    (make-array (list (length l)
		      (length (first l)))
		:initial-contents l)))

(defun pos-to-key (pos keys)
  "Returns the key at POS on KEYS."
  (declare (type pos pos))
  (aref keys
	(pos-row pos)
	(pos-col pos)))

(defun keys-total (keys corpus)
  "Returns the total frequency count of all the keys on KEYS from the
CORPUS. Used for calculating metric percentages."
  (let ((total 0))
    (loop for y below (array-dimension keys 0) do
      (loop for x below (array-dimension keys 1) do
	(incf total (gethash (aref keys y x) (corpus-chars corpus)))))
    total))

(defun swap-keys (a b keys)
  "Destructively modifies original KEYS by swapping A and B."
  (declare (type pos a b))
  (rotatef (pos-to-key a keys)
	   (pos-to-key b keys))
  keys)
