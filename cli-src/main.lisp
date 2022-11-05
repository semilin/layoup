(defpackage layoup/cli
  (:use :cl :layoup :arrows :cl-ansi-text)
  (:export :main)) 
(in-package :layoup/cli)

(defstruct defaults
  corpus
  keyboard)

(defvar *metrics* nil)
(defvar *layouts* (make-hash-table :test #'equal))
(defvar *corpora* (make-hash-table :test #'equal))
(defvar *keyboards* (make-hash-table :test #'equal))
(defvar *defaults* nil)

(defun load-metrics ()
  (load "./data/functions.lisp")
  (with-open-file (in #P"./data/metric-list.lisp")
    (with-standard-io-syntax
      (setf *metrics* (eval (read in))))))

(defun load-layouts ()
  (loop for f in (uiop:directory-files "./data/layouts/")
	do (with-open-file (in f)
	     (with-standard-io-syntax
	       (let ((l (eval (read in))))
		 (setf (gethash (->> l
				     layout-name
				     string-downcase
				     (substitute #\_ #\SPACE))
				*layouts*)
		       l))
	       ))))

(defun analyze (layout)
  (let* ((metric-results (layoup:calculate-metrics (defaults-keyboard *defaults*)
						   *metrics*))
	 (results (layoup:analyze-keys (defaults-corpus *defaults*)
				       (layout-matrix layout)
				       metric-results)))    
    (loop for k being the hash-keys in results using (hash-value v) do
      (format T "~a: ~1,2f%~%" (cyan (layoup:metric-name k))
	      (* 100
		 (/ v (layoup:keys-total (layout-matrix layout) (defaults-corpus *defaults*))))))))

(defun load-defaults ()
  (setf *defaults* (if (probe-file "./data/defaults.out")
		       (cl-store:restore "./data/defaults.out")
		       (make-defaults :corpus (gethash "monkeytype" *corpora*)
				      :keyboard #'matrix-pos))))

(defun save-defaults ()
  (cl-store:store *defaults* "./data/defaults.out"))

(defun default-keyboard (keyboard)
  (setf (defaults-keyboard *defaults*) keyboard))

(defun default-corpus (corpus)
  (setf (defaults-corpus *defaults*) corpus))

(defun load-corpora ()
  (setf *corpora* (cl-store:restore "./data/corpora.out")))

(defun list-corpora ()
  (format t "corpora:~%")
  (maphash (lambda (name c)
	     (declare (ignore c))
	     (format t "~t- ~a~%" name))
	   *corpora*))

(defun save-corpora ()
  (cl-store:store *corpora* "./data/corpora.out"))

(defun add-corpus (path name)
  (let ((corpus (new-corpus)))
    (add-file-to-corpus path corpus)
    (setf (gethash name *corpora*) corpus)))

(defun remove-corpus (corpus)
  (maphash (lambda (key c) (if (eq corpus c)
			  (remhash key *corpora* )))
	   *corpora*))

(defun help ()
  (format t "layoup is used by typing commands into the prompt.~%list of commands:~%")
  (mapcar (lambda (c) (format t "~t~a (~a): ~a~%"
			 (cyan (command-name c))
			 (str:join ", " (mapcar (lambda (a) (yellow a)) (command-aliases c)))
			 (command-description c)))
	  *commands*))

(defun prepare-quit ()
  (save-corpora)
  (save-defaults))

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort ()
       (prepare-quit)
       (uiop:quit))))

(define-condition user-error (error) ())

(define-condition argument-error (user-error) ()
  (:report "There was an error with the supplied arguments."))

(defstruct argument
  name
  type)

(defstruct command
  (name "" :type string)
  (aliases () :type list)
  (description "" :type string)
  (arguments () :type list)
  function)

(defun command-usage (command)
  (format nil "~a ~a"
	  (command-name command)
	  (str:join " " (mapcar (lambda (a) (yellow (str:upcase (argument-name a))))
				(command-arguments command)))))

(defun print-command-help (command)
  (format t "~a: ~a~%"
	  (cyan (command-name command))
	  (command-description command))
  (format t "~a: ~a~%"
	  (cyan "usage")
	  (command-usage command))
  (format t "~a: ~a~%"
	  (cyan "aliases")
	  (command-aliases command)))

(defun parse-argument (user-arg argument)
  (ecase (argument-type argument)
    (:string user-arg)
    (:path (let ((path (probe-file user-arg)))
	     (if path
		 path
		 (error 'argument-error :message (format t "File ~a does not exist.~%" (yellow user-arg))))))
    (:layout (let ((layout (gethash user-arg *layouts*)))
	       (if layout
		   layout
		   (error 'argument-error :message (format t "Layout ~a does not exist.~%" (yellow user-arg))))))
    (:corpus (let ((corpus (gethash user-arg *corpora*)))
	       (if corpus
		   corpus
		   (error 'argument-error :message (format t "Corpus ~a does not exist.~%" (yellow user-arg))))))
    (:keyboard (alexandria:switch (user-arg :test #'equal)
		 ("matrix" #'layoup:matrix-pos)
		 ("ansi" #'layoup:ansi-pos)
		 (otherwise (error 'argument-error :message (format t "Keyboard ~a does not exist.~%" (yellow user-arg))))))))

(defun parse-command (command args)
  (if (not (command-arguments command))
      (funcall (command-function command))
      (if (/= (length args)
	      (length (command-arguments command)))
	  (print-command-help command)
	  (apply (command-function command)
		 (loop for arg in args
		       for carg in (command-arguments command)
		       collect (parse-argument arg carg))))))

(defun get-command (args)
  (mapcar (lambda (command) (if (or (string-equal (first args)
					     (command-name command))
			       (remove-if-not (lambda (a) (string-equal (first args) a))
					      (command-aliases command)))
			   (parse-command command (rest args))))
	  *commands*))

(defun main ()
  (load-metrics)
  (load-layouts)
  (load-corpora)
  (load-defaults)

  #+sb-ext (sb-ext:disable-debugger)
  (exit-on-ctrl-c
   (loop do (progn
	      (format t "> ")
	      (finish-output)
	      (let ((args (str:words (read-line))))
		(if args (handler-case (get-command args)
			   (user-error (e) (format t "~a: ~a~%" (red "error") e)))))))))
