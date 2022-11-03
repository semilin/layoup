(defpackage layoup/cli
  (:use :cl :layoup :arrows)
  (:export :main)) 
(in-package :layoup/cli)

(defvar *metrics* nil)
(defvar *layouts* (make-hash-table :test #'equal))
(defvar *corpora* (make-hash-table :test #'equal))

(defun load-metrics ()
  (load "./data/functions.lisp")
  (with-open-file (in #P"./data/metric-list.lisp")
    (with-standard-io-syntax
      (setf *metrics* (eval (read in))))))

(defun load-layouts ()
  (with-open-file (in #P"./data/layouts.lisp")
    (with-standard-io-syntax
      (loop for l in (eval (read in))
	    do (setf (gethash (->> l
				   layout-name
				   string-downcase
				   (substitute #\_ #\ ))
			      *layouts*)
		     l)))))

(defun analyze-layout (layout corpus &optional keyboard)
  ())

(defun load-corpora ()
  (setf *corpora* (cl-store:restore "./data/corpora.out")))

(defun save-corpora ()
  (cl-store:store *corpora* "./data/corpora.out"))

(defun add-corpus (path name)
  (let ((corpus (new-corpus)))
    (add-file-to-corpus path corpus)
    (setf (gethash name *corpora*) corpus)))

(defun main ()
  (load-metrics)
  (load-layouts)
  (load-corpora)
  
  (let ((metric-results (layoup:calculate-metrics #'layoup:ansi-pos *metrics*))
	(gutenberg (gethash "gutenberg" *corpora*))
	(colemak (layout-matrix (gethash "colemak" *layouts*))))
    (time (loop repeat 1000 do
      (layoup:analyze-keys gutenberg colemak metric-results)))
    
    ;; (tracer:with-tracing ("LAYOUP" ecase gethash incf) (loop repeat 3 do
    ;;   (layoup:analyze-keys gutenberg semimak metric-results)))
    ;; (tracer:save-report "report.json")
    
    ;; (let ((threads nil))
    ;;   (time (loop repeat 100 do
    ;; 	(push (bt:make-thread (lambda () (loop repeat 1000
    ;; 					  do (layoup:analyze-keys gutenberg semimak metric-results)))) threads)))
    ;;   (time (mapcar #'bt:join-thread threads)))
    (let ((results (layoup:analyze-keys gutenberg colemak metric-results)))
      (format T "~a~%" (layoup::layout-metric-ngrams gutenberg colemak metric-results (fourth (metric-list-bigraphs *metrics*))))
      (loop for k being the hash-keys in results using (hash-value v) do
	(format T "~a: ~f%~%" (layoup:metric-name k) (* 100 (/ v (layoup:keys-total colemak gutenberg))))))))
