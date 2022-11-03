(in-package :layoup)

(defstruct corpus
  chars
  bigrams
  skipgrams
  trigrams)

;; private util function for add-to-corpus
(defun add-one (key table)
  (declare (type hash-table table))
  (if (null (gethash key table))
      (setf (gethash key table) 1)
      (incf (gethash key table))))

(defun new-corpus ()
  "Creates an empty corpus to be added to."
  (make-corpus
   :chars (make-hash-table :size 100)
   :bigrams (make-hash-table :test 'equal :size 1800)
   :skipgrams (make-hash-table :test 'equal :size 3000)
   :trigrams (make-hash-table :test 'equal :size 12000)))

(defun add-to-corpus (text corpus)
  "Adds the data from text to corpus."
  (declare (type string text)
	   (type corpus corpus)
	   (optimize (speed 3)))
  (loop for (a b c) on (coerce text 'list) do
    (if a
	(let ((a (char-downcase a)))
	  (add-one a (corpus-chars corpus))
	  (if b
	      (let ((b (char-downcase b)))
		(add-one (list a b) (corpus-bigrams corpus))
		(if c
		    (let ((c (char-downcase c)))
		      (add-one (list a b c) (corpus-trigrams corpus))
		      (add-one (list a c) (corpus-skipgrams corpus)))))))))
  corpus)

(defun add-file-to-corpus (path corpus)
  "Reads a text file and adds its data to corpus."
  (declare (type pathname path)
	   (type corpus corpus)
	   (optimize (speed 3)))
  (with-open-file (file path)
    (loop for line = (read-line file nil nil)
	  while line
	  do (add-to-corpus line corpus)))
  corpus)
