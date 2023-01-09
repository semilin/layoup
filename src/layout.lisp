(in-package :layoup)

(defstruct layout
  (name "" :type string)
  matrix
  shift-matrix
  keyboard)

(defun* (key-matrix -> (simple-array character *))
    (&rest row-list)
  "Converts a list of rows into a 2d key array. Each row is a string with no spaces, e.g. \"asdfghjkl;\" "
  (let ((l (mapcar (lambda (row) (coerce row 'list)) row-list)))
    (make-array (list (length l)
		      (length (first l)))
		:initial-contents l
		:element-type 'character)))

(defun* (pos-to-key -> character)
    ((pos pos) (keys (simple-array character *)))
  "Returns the key at POS on KEYS."
  (declare (inline)
	   (optimize (speed 3) (safety 1)))
  (aref keys
	(pos-row pos)
	(pos-col pos)))

(defun keys-total (keys corpus)
  "Returns the total frequency count of all the keys on KEYS from the
CORPUS. Used for calculating metric percentages."
  (let ((total 0))
    (loop for y below (array-dimension keys 0) do
      (loop for x below (array-dimension keys 1) do
	(incf total (gethash (aref keys y x) (corpus-chars corpus) 0))))
    total))

(defun swap-keys (a b keys)
  "Destructively modifies original KEYS by swapping A and B."
  (declare (type pos a b)
	   (type (simple-array character *) keys))
  (rotatef (aref keys
		 (pos-row a)
		 (pos-col a))
	   (aref keys
		 (pos-row b)
		 (pos-col b)))
  keys)
