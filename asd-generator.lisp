;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ASD-GENERATOR
;;;; Michał "phoe" Herda - phoe@openmailbox.org
;;;; GPLv3
(in-package #:asd-generator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-unix-time ()
  (- (get-universal-time) 2208988800))

(defun tree-depth (tree)
  (if (consp tree)
      (1+ (apply #'max (mapcar #'tree-depth tree)))
      0))

(defun all-pathnames (directory &optional type)
  (flatten (iterate (for entry in (cl-fad:list-directory directory))
	     (when (cl-fad:directory-pathname-p entry)
	       (collect (all-pathnames entry type)))
	     (collect entry))))

(defun mapc-directory-tree (directory &optional type)
  (remove-if-not (lambda (x) (equal (pathname-type x) type))
		 (all-pathnames directory type)))

(defun get-relative-pathnames (directory &optional (type "lisp"))
  (assert (cl-fad:pathname-absolute-p (pathname directory))
	  (directory) "You must provide an absolute pathname.")
  (flet ((filter (x) (append (nthcdr (list-length (pathname-directory (pathname directory)))
				     (pathname-directory x))
			     (when type (list (pathname-name x))))))
    (mapcar #'filter (mapc-directory-tree directory type))))

(defun path-to-strings (path)
  (list :file (format nil "~{~A~^/~}" path)))

(defun symbol->string (symbol)
  (string-downcase (string symbol)))

(defun split (data value n)
  (let (list rest)
    (iterate (for element in data)
      (if (string= (nth n element) value)
	  (push element list)
	  (push element rest)))
    (values (nreverse list) (nreverse rest))))

(defun process (data terms n)
  (if (consp terms) 
      (iter (with data = (copy-tree data)) 
	(with processed-data = nil) 
	(for term in terms)
	(cond ((equal term '(:rest))
	       (push :rest processed-data))
	      ((consp term)
	       (multiple-value-bind (list rest)
		   (split data (symbol->string (car term)) n)
		 (push list processed-data)
		 (setf data rest))))
	(finally
	 (return (nsubst data :rest (nreverse processed-data)))))
      data))

(defun process-recur (pathnames data &optional (n 0))
  (if data
      (let ((sequences (process pathnames data n)))
	(iter (for sequence in sequences)
	  (for data-piece in data)
	  (collect (process-recur sequence (cdr data-piece) (1+ n))))) 
      pathnames))

(defun traverse-stringify (tree)
  (cond ((null tree)
	 nil)
	((and (consp tree) (stringp (car tree)))
	 (path-to-strings tree))
	(t
	 (mapcan #'traverse-stringify tree))))

(defun generate-structure (list)
  (when list
    (cons (list (first list) (second list))
	  (generate-structure (cddr list)))))

(defun generate (directory generator-data)
  (let ((pathnames (get-relative-pathnames directory)))
    (generate-structure (traverse-stringify (process-recur pathnames generator-data)))))

(defun generate-file (system)
  (let* ((system-dir (asdf:component-pathname (asdf:find-system system)))
	 (file-pathname (cl-fad:merge-pathnames-as-file
			 system-dir
			 (concatenate 'string (string-downcase (string system)) ".asd")))
	 (asd-gen-pathname (cl-fad:merge-pathnames-as-file
			    system-dir
			    "asd-generator-data.asd"))
	 (generator-data
	   (progn
	     (assert (file-exists-p asd-gen-pathname) (asd-gen-pathname)
		     "The file asd-generator-data.asd was not found.")
	     (with-open-file (stream asd-gen-pathname) (read stream)))))
    (with-open-file (stream file-pathname)
      (let ((data (read stream)))
	(setf (getf data :components) (generate system-dir generator-data))
	data))))

(defun fix (file)
  (replace (format nil "~S" (subst :asdfdefsystem 'asdf:defsystem file)) "ASDF:" :start1 1))

(defun write-asd (system &key (im-sure nil))
  (let* ((file (fix (generate-file system)))
	 (system-dir (asdf:component-pathname (asdf:find-system system)))
	 (file-pathname (cl-fad:merge-pathnames-as-file
			 system-dir
			 (concatenate 'string (string-downcase (string system)) ".asd")))
	 (renamed-pathname (cl-fad:merge-pathnames-as-file
			    system-dir
			    (concatenate 'string "."
					 (string-downcase (string system)) "."
					 (format nil "~D" (get-unix-time)) ".asd-backup")))) 
    (unless (file-exists-p file-pathname)
      (error "Original ASD file NOT found at ~A.~%" file-pathname)) 
    (format t "### This will write a new file at:~%~A~%" file-pathname)
    (format t "### and store the backup of the original at:~%~A~%" renamed-pathname)
    (unless im-sure
      (format t "### Hit Enter to continue.~%")
      (read-line))
    (rename-file file-pathname renamed-pathname)
    (format t "### Renamed file ~%~A~%### to ~%~A.~%" file-pathname renamed-pathname)
    (with-open-file (stream file-pathname :direction :output)
      (format stream ";;;; Autogenerated ASD file for system ~A~%"
	      (string-upcase (string system)))
      (princ file stream)
      (terpri stream)
      (force-output stream))
    (format t "### Wrote file ~%~A.~%### Finished.~%" file-pathname)
    file-pathname))

(defun regen (&key (im-sure nil))
  (write-asd (intern (package-name *package*) (find-package :keyword)) :im-sure im-sure))
