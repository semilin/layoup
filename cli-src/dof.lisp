(in-package :layoup/cli)

(defun dof-key-matrix (l)
  (mapcar (lambda (row)
	    (-<>> row
		  (coerce <> 'list)
		  (remove #\SPACE)
		  (subseq <> 0 10)
		  (coerce <> 'string)))
	  l))

(defun dof-sync ()
  (let* ((dof-url "https://raw.githubusercontent.com/ClemenPine/DOF/main/layouts/")
	 (names (-> (str:concat dof-url "_list.json")
		    dex:get
		    yason:parse)))
    (loop for name in names
	  for table = (-> (str:concat dof-url
				      name
				      ".json")
			  dex:get
			  yason:parse)
	  for layout = `(make-layout :name ,(gethash "name" table)
				     :matrix (apply #'key-matrix ',(ignore-errors
								    (->> table
									 (gethash "layers")
									 (gethash "main")
									 dof-key-matrix)))
				     :shift-matrix nil
				     :keyboard ,(gethash "keyboard" table))
	  do (with-open-file (file (merge-pathnames (str:concat (gethash "name" table) ".lisp")
						    (merge-pathnames "layouts/" *data-dir*))
				   :if-does-not-exist :create
				   :if-exists :supersede
				   :direction :output)
	       (format t "~a~%" name)
	       (pprint layout file)))))
