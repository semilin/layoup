(in-package :layoup/cli)

(defun random-pos ()
  (make-pos :finger LP
	    :col (random 10)
	    :row (random 3)))

(defun constraint-anneal (comparator1 optimized comparator2 constrained-metric percentage)
  (declare (type metric constrained-metric)
	   (type number percentage)
	   (type metric optimized)
	   (type function comparator1)
	   (type function comparator2))
  (let* ((keys (alexandria:copy-array (layout-matrix (gethash "qwerty" *layouts*))))
	 (corpus (defaults-corpus *defaults*))
	 (baseline (round (/ (* percentage (layoup:keys-total keys corpus)) 100)))
	 (metric-results (layoup:calculate-metrics (defaults-keyboard *defaults*)
						   ;; reduces *metrics* to only the metrics needed
						   (make-metric-list :bigraphs (remove-if-not (lambda (m) (or (eq m constrained-metric)
													 (eq m optimized)))
											      (metric-list-bigraphs *metrics*))
								     :trigraphs (remove-if-not (lambda (m) (or (eq m constrained-metric)
													  (eq m optimized)))
											       (metric-list-trigraphs *metrics*)))))
	 (current-stats (layoup:analyze-keys corpus keys metric-results))
	 (fitting-constraint (funcall comparator2 (gethash constrained-metric current-stats) baseline)))
    (loop for temp downfrom 100 to -2
	  do (progn
	       (format t "~C~a    " #\return temp)
	       (finish-output)
	       (dotimes (i (* 5 (- 100 temp)))
		 (let ((a (random-pos))
		       (b (random-pos)))
		   (swap-keys a b keys)
		   (let ((new-stats (layoup:analyze-keys corpus keys metric-results)))
		     (if (and (if fitting-constraint
				  (funcall comparator2 (gethash constrained-metric new-stats) baseline)
				  (let ((improved (funcall comparator2
							   (gethash constrained-metric new-stats)
							   (gethash constrained-metric current-stats))))
				    (if (and improved (funcall comparator2 (gethash constrained-metric new-stats) baseline))
					(setf fitting-constraint t))
				    improved))
			      (funcall comparator1
				       (gethash optimized new-stats)
				       (gethash optimized current-stats)))
			 (setf current-stats new-stats)
			 (if (> temp (random 100))
			     (setf current-stats new-stats
				   fitting-constraint (funcall comparator2 (gethash constrained-metric new-stats) baseline))
			     (swap-keys a b keys))))))))
    (format t "~%")
    (print-matrix keys)
    (analyze (make-layout :name "Generated" :matrix keys :keyboard (defaults-keyboard *defaults*)))))
