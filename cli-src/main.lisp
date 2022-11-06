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
(defvar *profiles* (make-hash-table :test #'equal))
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
		       l))))))

(defun get-metric (s)
  (declare (type string s))
  (let ((result (remove-if-not (lambda (m) (string-equal (metric-name m) s))
			       (append (metric-list-bigraphs *metrics*)
				       (metric-list-trigraphs *metrics*)))))
    (if result
	(first result)
	nil)))

(defun load-profiles ()
  (loop for f in (uiop:directory-files "./data/constraint-profiles/")
	do (with-open-file (in f)
	     (with-standard-io-syntax
	       (in-package :layoup/cli)
	       (let ((p (eval (read in))))
		 (setf (gethash (->> p
				     constraint-profile-name
				     string-downcase
				     (substitute #\_ #\SPACE))
				*profiles*)
		       p))))))

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

(defun list-metrics ()
  (format t "metrics:~%")
  (mapcar (lambda (m) (format t "~t- ~a~%" (metric-name m)))
	  (append (metric-list-bigraphs *metrics*)
		  (metric-list-trigraphs *metrics*))))

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
  (examples () :type list)
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
  (format t "~a: ~{~a~^~%~}~%"
	  (cyan "examples")
	  (command-examples command))
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
		 (otherwise (error 'argument-error :message (format t "Keyboard ~a does not exist.~%" (yellow user-arg))))))
    (:metric (let ((bg (remove-if-not (lambda (m)
					(equal user-arg (metric-name m)))
				      (metric-list-bigraphs *metrics*)))
		   (tg (remove-if-not (lambda (m)
					(equal user-arg (metric-name m)))
				      (metric-list-trigraphs *metrics*))))
	       (if tg
		   (car tg)
		   (if bg
		       (car bg)
		       (error 'argument-error :message (format t "Metric ~a does not exist.~%" (yellow user-arg)))))))
    (:comparator (alexandria:switch (user-arg :test #'equal)
		   ("less" #'<=)
		   ("more" #'>=)
		   (otherwise (error 'argument-error :message (format t "~a is not a valid comparator. Valid options are (~a, ~a).~%"
								      (yellow user-arg)
								      (cyan "less")
								      (cyan "more"))))))
    (:profile (let ((p (gethash user-arg *profiles*)))
		(if p
		    p
		    (error 'argument-error :message (format t "Constraint profile ~a does not exist.~%"
							    (yellow user-arg))))))
    (:number (with-input-from-string (in user-arg)
	       (let ((v (read in)))
		 (if (numberp v)
		     v
		     (error 'argument-error :message (format t "~a is not a number.~%"
							     (yellow user-arg)))))))))

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
  (load-profiles)

  (setf *random-state* (make-random-state t))

  #+sb-ext (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (loop do (progn
	       (format t "> ")
	       (finish-output)
	       (let ((args (str:words (read-line))))
		 (if args (handler-case (get-command args)
			    (user-error (e) (format t "~a: ~a~%" (red "error") e)))))))))
