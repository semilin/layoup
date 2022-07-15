(in-package :layoup)

(defstruct corpus
  chars
  bigrams
  skipgrams
  trigrams)

;; private util function for add-to-corpus
(defun add-one (key table)
  (if (null (gethash key table))
      (setf (gethash key table) 1)
      (incf (gethash key table))))

(defun new-corpus ()
  "Creates an empty corpus to be added to."
  (let ((corpus (make-corpus
		 :chars (make-hash-table :size 100)
		 :bigrams (make-hash-table :test 'equal :size 1800)
		 :skipgrams (make-hash-table :test 'equal :size 3000)
		 :trigrams (make-hash-table :test 'equal :size 12000))))
    corpus))

(defun add-to-corpus (text corpus)
  "Adds the data from text to corpus."
  (loop for (a b c) on (coerce text 'list)
	while (and b c) do
	  (let ((a (char-downcase a))
		(b (char-downcase b))
		(c (char-downcase c)))
	    (add-one a (corpus-chars corpus))
	    (add-one (list a b) (corpus-bigrams corpus))
	    (add-one (list a b c) (corpus-trigrams corpus))
	    (add-one (list a c) (corpus-skipgrams corpus))))
  corpus)

(defun add-file-to-corpus (path corpus)
  "Reads a text file and adds its data to corpus."
  (with-open-file (file path)
    (loop for i from 0
	  for line = (read-line file nil nil)
	  while line
	  do (add-to-corpus line corpus)))
  corpus)
