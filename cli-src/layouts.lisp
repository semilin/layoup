(in-package :layoup/cli)


(defun convert-row (row)
  (-<>> row
	str:trim
	(str:replace-all " " "")
	(subseq <> 0 10)
	(str:replace-all "\"" "\\\"")))

(defun convert-genkey (path)
  (loop for f in (uiop:directory-files path)
	do (if (not (string-equal (file-namestring f) "_generate"))
	       (let ((in (open f)))
		 (let ((g (loop for line = (read-line in nil nil)
				while line
				collect line)))
		   (with-open-file (file (merge-pathnames (str:concat (file-namestring f) ".lisp")
							  (merge-pathnames "data/layouts/"
									   (uiop:getcwd)))
					 :direction :output
					 :if-exists :overwrite
					 :if-does-not-exist :create)
		     (format t "~a~%" (first g))
		     (format file "(make-layout :name \"~a\"~%:matrix (key-matrix \"~a\"~%            \"~a\"~%            \"~a\"))"
			     (str:trim (first g))
			     (convert-row (second g))
			     (convert-row (third g))
			     (convert-row (fourth g)))))))))
