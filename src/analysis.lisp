(in-package :layoup)

(defstruct metric
  name
  fn)

(defstruct metric-result
  metric
  positions
  result)

(defstruct metric-list
  bigraphs
  trigraphs)

(defun new-metric (name fn)
  (declare (type string name)
	   (type function fn))
  (make-metric :name name :fn fn))

(defun classify-bigraph (metric-list a b)
  (declare (type metric-list metric-list)
	   (type pos a b))
  (let ((results nil))
    (loop for metric in (metric-list-bigraphs metric-list) do
      (let ((result (funcall (metric-fn metric) a b)))
	(if (not (zerop result))
	    (push (make-metric-result :metric metric
				      :positions (list a b)
				      :result result)
		  results))))
    results))

(defun classify-trigraph (metric-list a b c)
  (declare (type metric-list metric-list)
	   (type pos a b c))
  (let ((results nil))
    (loop for metric in (metric-list-trigraphs metric-list) do
      (let ((result (funcall (metric-fn metric) a b c)))
	(if (not (zerop result))
	    (push (make-metric-result :metric metric
				      :positions (list a b c)
				      :result result)
		  results))))
    results))

(defun calculate-metrics (pos-fn metric-list)
  (declare (type function pos-fn)
	   (type metric-list metric-list))
  (let ((positions nil)
	(combinations nil)
	(results nil))    
    ;; create basic positions list
    (loop for row from 0 to 2 do
      (loop for col from 0 to 9 do
	(push (funcall pos-fn col row) positions)))

    ;; create combinations
    (loop for a in positions do
      (loop for b in positions do
	(push (list a b) combinations)))

    ;; calculate metrics
    (loop for comb in combinations do
      (let* ((a (first comb))
	     (b (second comb))
	     (result (classify-bigraph metric-list a b)))
	(if (not (null result))
	    (push result results))))
    results))

(defun ngraph-to-ngram (ngraph k)
  (mapcar (lambda (pos) (pos-to-key pos k)) ngraph))

(defun analyze-keys (corpus k metric-results)
  (declare (type corpus corpus)
	   (type array k)
	   (type list metric-results))
  (declare (optimize (speed 3) (safety 0)))
  (let ((results (make-hash-table)))
    (loop for rlist in metric-results do
      (loop for result in rlist do
	(let ((frequency (gethash (ngraph-to-ngram (metric-result-positions result) k)
				  (corpus-bigrams corpus) 0))
	      (metric-r (metric-result-metric result)))
	  (declare (type fixnum frequency))
	  ;; TODO figure out how to improve performance here (declare
	  ;; hash result to be fixnum)
	  (incf (gethash metric-r results 0) frequency))))
    results))
