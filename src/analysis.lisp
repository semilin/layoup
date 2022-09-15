(in-package :layoup)

(defstruct metric
  (name "" :type string)
  (fn nil :type function)
  ngram)

(defstruct metric-result
  (positions nil :type list)
  (result 0.0 :type single-float))

(defstruct metric-result-list
  table)

(defstruct metric-list
  bigraphs
  trigraphs)

(defun new-metric (name fn ngram)
  (declare (type string name)
	   (type function fn))
  (make-metric :name name :fn fn :ngram ngram))

(defun classify-ngram (metric positions)
  (declare (type metric metric))
  (let ((result (apply (metric-fn metric) positions)))
    (if result
	(make-metric-result :positions positions :result result))))

(defun calculate-metrics (pos-fn metric-list)
  (declare (type function pos-fn)
	   (type metric-list metric-list))
  (let* ((positions nil)
	 (tricombinations nil)
	 (combinations nil)
	 (results (make-metric-result-list :table (make-hash-table :test 'eq)))
	 (table (metric-result-list-table results))) 
    ;; create basic positions list
    (loop for row from 0 to 2 do
      (loop for col from 0 to 9 do
	(push (funcall pos-fn col row) positions)))

    ;; create combinations
    (loop for a in positions do
      (loop for b in positions do
	(push (list a b) combinations)
	(loop for c in positions do
	  (push (list a b c) tricombinations))))

    ;; calculate metrics
    (loop for comb in combinations do
      (loop for metric in (metric-list-bigraphs metric-list)
	    do (let ((result (classify-ngram metric comb)))
		 (if result (push result (gethash metric table))))))

    (loop for comb in tricombinations do
      (loop for metric in (metric-list-trigraphs metric-list)
	    do (let ((result (classify-ngram metric comb)))
		 (if result (push result (gethash metric table))))))
    results))

(defun ngraph-to-ngram (ngraph k)
  (declare (optimize (speed 3)
		     (safety 0))
	   (type (or list character) ngraph)
	   (type (array) k)
	   (inline))
  (mapcar (lambda (pos) (pos-to-key pos k)) ngraph))

(defun analyze-keys (corpus k metric-results)
  (declare (type corpus corpus)
	   (type array k)
	   (type metric-result-list metric-results))
  (declare (optimize (speed 3) (safety 3)))
  (let* ((table (metric-result-list-table metric-results))
	 (results (make-hash-table :size (hash-table-size table))))
    (loop for metric being the hash-keys of table
	    using (hash-value values) do
	      (let ((corpus-ngrams (ecase (metric-ngram metric)
				     (:bigram (corpus-bigrams corpus))
				     (:skipgram (corpus-skipgrams corpus))
				     (:trigram (corpus-trigrams corpus)))))
		(loop for value in values
		      do (let ((frequency (gethash (ngraph-to-ngram (metric-result-positions value) k) corpus-ngrams 0))
			       (metric-amount (metric-result-result value)))
			   (declare (type fixnum frequency))
			   (incf (the single-float (gethash metric results 0.0)) (the single-float (* metric-amount frequency)))))))
    results))

(defun layout-metric-ngrams (corpus k metric-results metric)
  (declare (type corpus corpus)
	   (type array k)
	   (type metric-result-list metric-results)
	   (type metric metric))
  (sort (loop for value in (gethash metric (metric-result-list-table metric-results))
	      collect (let ((ngram (ngraph-to-ngram (metric-result-positions value) k))
			    (corpus-ngrams (ecase (metric-ngram metric)
					     (:bigram (corpus-bigrams corpus))
					     (:skipgram (corpus-skipgrams corpus))
					     (:trigram (corpus-trigrams corpus)))))
			(list ngram
			      (* (gethash ngram corpus-ngrams 0.0)
				 (metric-result-result value)))))
	(lambda (a b) (> (second a)
		    (second b)))))
