(in-package :layoup/cli)

(defun format-stat (value layout)
  (format nil "~1,2f%~%"
	  (* 100
	     (/ value (layoup:keys-total (layout-matrix layout)
					 (defaults-corpus *defaults*))))))

(defun rank (metric)
  (declare (type metric metric))
  (let ((metric-results (layoup:calculate-metrics (defaults-keyboard *defaults*)
						  *metrics*)))    
    (-> (loop for k being the hash-keys in *layouts* using (hash-value v)
	      collect (let ((result (layoup:analyze-keys (defaults-corpus *defaults*)
							 (layout-matrix v)
							 metric-results)))
			(list v
			      (gethash metric result))))
	(sort (lambda (a b) (> (second a)
			  (second b)))))))

(defun most (metric)
  (mapcar (lambda (result) (format T "~a ~a"
			      (cyan (layout-name (first result)))
			      (format-stat (second result) (first result))))
	  (subseq (rank metric) 0 10)))

(defun least (metric)
  (mapcar (lambda (result) (format T "~a ~a" 
			      (cyan (layout-name (first result)))
			      (format-stat (second result) (first result))))
	  (reverse (subseq (reverse (rank metric)) 0 10))))

(defun analyze (layout)
  (let* ((metric-results (layoup:calculate-metrics (defaults-keyboard *defaults*)
						   *metrics*))
	 (results (layoup:analyze-keys (defaults-corpus *defaults*)
				       (layout-matrix layout)
				       metric-results)))    
    (loop for k being the hash-keys in results using (hash-value v) do
      (format t "~a: ~a" (cyan (metric-name k))
	      (format-stat v layout)))))
