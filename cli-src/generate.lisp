(in-package :layoup/cli)

(defun random-layout ()
  (declare (optimize (debug 3)))
  (let ((chars (coerce "abcdefghijklmnopqrstuvwxyz,./'" 'list))
	(matrix ()))
    (dotimes (row 3)
      (push () matrix)
      (dotimes (col 10)
	(let* ((n (random (length chars)))
	       (c (nth n chars)))
	  (setf chars (remove c chars))	
	  (push c (first matrix)))))
    (make-array '(3 10) :initial-contents matrix
			:initial-element #\SPACE
			:element-type 'character)))

(defun random-pos ()
  (make-pos :finger LP
	    :col (random 10)
	    :row (random 3)))

(defun constraint-deviance (stats constraints)
  (reduce #'* (mapcar (lambda (c)
			(if (funcall (ecase (constraint-goal c)
				       (:less '<=)
				       (:more '>=))
				     (gethash (constraint-metric c) stats)
				     (constraint-threshold c))
			    1
			    (+ 1 (expt (/ (- (gethash (constraint-metric c) stats)
					     (constraint-threshold c))
					  (* 0.4
					     (constraint-leniency c)
					     (constraint-threshold c)))
				       1.5))))
		      constraints)))

(defun sum-deviance-optimized (deviance opt-comparator optimized stats)
  (if (eq opt-comparator #'<=)
      (* deviance (gethash optimized stats))
      (/ (gethash optimized stats) deviance)))

(defun first-matching (profile)
  (declare (type constraint-profile profile)
	   (optimize (debug 3)))
  (let* ((keys (random-layout))
	 (corpus (defaults-corpus *defaults*))
	 (constraints (mapcar (lambda (c) (make-constraint :metric (constraint-metric c)
						      :goal (constraint-goal c)
						      :threshold (* (keys-total keys corpus)
								    (/ (constraint-threshold c) 100))))
			      (constraint-profile-constraints profile)))
	 (metric-results (layoup:calculate-metrics
			  (defaults-keyboard *defaults*)
			  (make-metric-list :bigraphs (remove-if-not
						       (lambda (m) (remove-if-not (lambda (c)
									       (equal (constraint-metric c)
										      m))
									     constraints))
						       (metric-list-bigraphs *metrics*))
					    :trigraphs (remove-if-not
							(lambda (m) (remove-if-not (lambda (c)
										(equal (constraint-metric c)
										       m))
									      constraints))
							(metric-list-trigraphs *metrics*)))))
	 (current-stats (layoup:analyze-keys corpus keys metric-results))
	 (current-deviance (constraint-deviance current-stats constraints))
	 (matching nil))
    (loop for temp downfrom 100
	  while (not matching)	  
	  do (dotimes (i 100)
	       (print current-deviance)
	       (progn (let ((a (random-pos))
			    (b (random-pos)))
			(swap-keys a b keys)
			(let* ((new-stats (layoup:analyze-keys corpus keys metric-results))
			       (new-deviance (constraint-deviance new-stats constraints)))
			  (if (<= new-deviance 1)
			      (setf matching t))
			  (if (<= new-deviance current-deviance)
			      (setf current-stats new-stats
				    current-deviance new-deviance)
			      (if (<= (* 100 (/ (abs (- new-deviance current-deviance)) current-deviance))
				      temp)
				  (setf current-stats new-stats
					current-deviance new-deviance)
				  (swap-keys a b keys))))))))
    (format t "~%")
    (print-matrix keys)
    (analyze (make-layout :name "Generated" :matrix keys :keyboard (defaults-keyboard *defaults*)))))

(defun constraint-anneal (opt-comparator optimized profile)
  (declare (type metric optimized)
	   (type function opt-comparator)
	   (type constraint-profile profile))
  (let* ((keys (random-layout))
	 (corpus (defaults-corpus *defaults*))
	 (constraints (mapcar (lambda (c) (make-constraint :metric (constraint-metric c)
						      :goal (constraint-goal c)
						      :threshold (* (keys-total keys corpus)
								    (/ (constraint-threshold c) 100))))
			      (constraint-profile-constraints profile)))
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
	 (current-deviance (constraint-deviance current-stats constraints))
	 (current-total (sum-deviance-optimized current-deviance opt-comparator optimized current-stats)))
    (loop for temp downfrom 100 to -2
	  do (progn
	       ;;(format t "~C~a%    " #\return temp)
	       (format t "~a%: ~a * ~a = ~a~%" temp current-deviance (gethash optimized current-stats) current-total)
	       (finish-output)
	       (dotimes (i (round (expt (* 3 (- 100 temp)) 1.3)))
		 (let ((a (random-pos))
		       (b (random-pos)))
		   (swap-keys a b keys)
		   (let* ((new-stats (layoup:analyze-keys corpus keys metric-results))
			  (new-deviance (constraint-deviance new-stats constraints))
			  (new-total (sum-deviance-optimized new-deviance opt-comparator optimized new-stats)))
		     (if (<= new-total current-total)
			 (setf current-stats new-stats
			       current-deviance new-deviance
			       current-total new-total)			 
			 (if (<= (* 100 (/ (abs (- new-total current-total)) current-total))
				 temp)
			     (setf current-stats new-stats
				   current-deviance new-deviance
				   current-total new-total)
			     (swap-keys a b keys))))))))
    (format t "~%")
    (print-matrix keys)
    (analyze (make-layout :name "Generated" :matrix keys :keyboard (defaults-keyboard *defaults*)))
    (loop for c in constraints
	  do (if (> (gethash (constraint-metric c) current-stats)
		    (constraint-threshold c))
		 (format t "~a constraint failed~%" (metric-name (constraint-metric c)))))
    keys))

(defun constraint-evolve (opt-comparator optimized profile)
  (let* ((metric-results (layoup:calculate-metrics
			  (defaults-keyboard *defaults*)
			  *metrics*))
	 (corpus (defaults-corpus *defaults*))
	 (constraints (mapcar (lambda (c)
				(make-constraint :metric (constraint-metric c)
						 :goal (constraint-goal c)
						 :threshold (* (keys-total (layout-matrix (gethash "qwerty" *layouts*))
									   corpus)
							       (/ (constraint-threshold c) 100))))
			      (constraint-profile-constraints profile)))
	 (threads (loop for i from 1 to 50
			collect (bordeaux-threads:make-thread (lambda ()
								(constraint-anneal opt-comparator optimized profile))))))))

(defun constraint-multithreaded-anneal (opt-comparator optimized profile)
  (let* ((metric-results (layoup:calculate-metrics
			  (defaults-keyboard *defaults*)
			  *metrics*))
	 (corpus (defaults-corpus *defaults*))
	 (constraints (mapcar (lambda (c)
				(make-constraint :metric (constraint-metric c)
						 :goal (constraint-goal c)
						 :threshold (* (keys-total (layout-matrix (gethash "qwerty" *layouts*))
									   corpus)
							       (/ (constraint-threshold c) 100))))
			      (constraint-profile-constraints profile)))
	 (threads (loop for i from 1 to 50
			collect (bordeaux-threads:make-thread (lambda ()
								(constraint-anneal opt-comparator optimized profile))))))
    (let ((keys (first
		 (first
		  (sort (mapcar (lambda (k) (let ((stats (analyze-keys corpus k metric-results)))
					 (list k (sum-deviance-optimized
						  (constraint-deviance stats constraints)
						  opt-comparator
						  optimized
						  stats))))
				(loop for thr in threads
				      collect (bordeaux-threads:join-thread thr)))
			(lambda (a b) (< (second a) (second b))))))))
      (format t "~%~%")
      (print-matrix keys)
      (analyze (make-layout :name "Generated" :matrix keys :keyboard (defaults-keyboard *defaults*))))))
