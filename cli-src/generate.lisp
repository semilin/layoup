(in-package :layoup/cli)

(defun random-pos ()
  (make-pos :finger LP
	    :col (random 10)
	    :row (random 3)))

(defun constraint-deviance (stats constraints max-threshold)
  (reduce #'+ (mapcar (lambda (c)
			(if (funcall (ecase (constraint-goal c)
				       (:less '<=)
				       (:more '>=))
				     (gethash (constraint-metric c) stats)
				     (constraint-threshold c))
			    0
			    (/ (* (/ max-threshold (constraint-threshold c))
				  (abs (- (constraint-threshold c)
					  (gethash (constraint-metric c) stats))))
			       (* 2 (length constraints)))))
		      constraints)))

(defun sum-deviance-optimized (deviance opt-comparator optimized stats)
  (if (eq opt-comparator #'<=)
      (+ deviance (gethash optimized stats))
      (- deviance (gethash optimized stats))))

(defun constraint-anneal (opt-comparator optimized profile)
  (declare (type metric optimized)
	   (type function opt-comparator)
	   (type constraint-profile profile))
  (let* ((keys (alexandria:copy-array (layout-matrix (gethash "qwerty" *layouts*))))
	 (corpus (defaults-corpus *defaults*))
	 (constraints (mapcar (lambda (c) (make-constraint :metric (constraint-metric c)
						      :goal (constraint-goal c)
						      :threshold (* (keys-total keys corpus)
								    (/ (constraint-threshold c) 100))))
			      (constraint-profile-constraints profile)))
	 (max-threshold (apply #'max (mapcar (lambda (c) (constraint-threshold c))
					     constraints)))
	 (metric-results (layoup:calculate-metrics
			  (defaults-keyboard *defaults*)
			  ;; reduces *metrics* to only the metrics needed
			  (make-metric-list :bigraphs (remove-if-not
						       (lambda (m) (or (eq m optimized)
								  (remove-if-not (lambda (c)
										   (equal (constraint-metric c)
											  m))
										 constraints)))
						       (metric-list-bigraphs *metrics*))
					    :trigraphs (remove-if-not
							(lambda (m) (or (eq m optimized)
								   (remove-if-not (lambda (c)
										    (equal (constraint-metric c)
											   m))
										  constraints)))
							(metric-list-trigraphs *metrics*)))))
	 (current-stats (layoup:analyze-keys corpus keys metric-results))
	 (current-deviance (constraint-deviance current-stats constraints max-threshold))
	 (current-total (sum-deviance-optimized current-deviance opt-comparator optimized current-stats)))
    (loop for temp downfrom 100 to -10
	  do (progn
	       (format t "~C~a%    " #\return temp)
	       ;;(format t "~a%: ~a + ~a = ~a~%" temp current-deviance (gethash optimized current-stats) current-total)
	       (finish-output)
	       (dotimes (i (* 10 (- 100 temp)))
		 (let ((a (random-pos))
		       (b (random-pos)))
		   (swap-keys a b keys)
		   (let* ((new-stats (layoup:analyze-keys corpus keys metric-results))
			  (new-deviance (constraint-deviance new-stats constraints max-threshold))
			  (new-total (sum-deviance-optimized new-deviance opt-comparator optimized new-stats)))
		     (if (<= new-total current-total)
			 (setf current-stats new-stats
			       current-deviance new-deviance
			       current-total new-total)			 
			 (if (> temp (random 100))
			     (setf current-stats new-stats
				   current-deviance new-deviance
				   current-total new-total)
			     (swap-keys a b keys))))))))
    (format t "~%")
    (print-matrix keys)
    (analyze (make-layout :name "Generated" :matrix keys :keyboard (defaults-keyboard *defaults*)))))
