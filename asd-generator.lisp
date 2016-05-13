;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ASD-GENERATOR
;;;; Michał "phoe" Herda - phoe@openmailbox.org
;;;; GPLv3
(in-package #:asd-generator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ensure-system (place)
  `(setf ,place (asdf:find-system ,place)))

(defun get-unix-time ()
  (- (get-universal-time) 2208988800))

(defun backup-pathname (pathname)
  (cl-fad:merge-pathnames-as-file
   (cl-fad:pathname-directory-pathname pathname)
   (concatenate 'string "."
                (string-downcase (pathname-name pathname)) "."
                (format nil "~D" (get-unix-time)) ".backup")))

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

(defun generate-components (system generator-data)
  (let ((pathnames (get-relative-pathnames (asdf:component-pathname system))))
    (generate-structure (traverse-stringify (process-recur pathnames generator-data)))))

;;; main functions

(defun read-asd (asd-pathname)
  (with-open-file (stream asd-pathname)
    (do ((form (read stream) (read stream)))
        ((progn
           (let ((*print-level* 1)
                 (*print-length* 1))
             (format t "Searching defsystem form... : ~S~%" form))
           (string= 'asdf:defsystem (symbol-name (car form))))
         (format t "defsystem form found!~%")
         form)
      )))

(defun generate-asd (system)
  (ensure-system system)
  (let* ((asd-pathname (asdf:system-source-file system))
	 (asd-gendata-pathname (cl-fad:merge-pathnames-as-file
                                (asdf:component-pathname system)
                                "asd-generator-data.asd"))
	 (generator-data
          (progn
            (assert (file-exists-p asd-gendata-pathname) (asd-gendata-pathname)
                    "The file ~a was not found." asd-gendata-pathname)
            (with-open-file (stream asd-gendata-pathname) (read stream)))))
    (let ((data (read-asd asd-pathname)))
      (setf (getf data :components) (generate-components system generator-data))
      (values data
              asd-pathname
              (backup-pathname asd-pathname)))))

(defun write-asd (system &key (im-sure nil))
  (ensure-system system)
  (multiple-value-bind (asdf-definition pathname) (generate-asd system)
    (let ((backup (backup-pathname pathname)))
      (format t "### This will write a new file at:~%~3t~S~%" pathname)
      (format t "### and store the backup of the original at:~%~3t~S~%" backup)
      (unless im-sure
        (tagbody
          :start
          (format t "### Hit Enter, Y or y to continue. Hit N or n to stop.~%")
          (case (aref (read-line) 0)
            ((#\return #\newline #\y #\Y)
             (format t "### Alright.~%"))
            ((#\N #\n)
             (format t "### Aborted.~%")
             (return-from write-asd))
            (otherwise
             (format t "### Unrecognized char.~%")
             (go :start)))))
      (restart-case
          (progn
            (rename-file pathname backup)
            (format t "### Renamed file ~%~3t~S~%### to ~%~3t~S.~%" pathname backup)
            (with-open-file (stream pathname :direction :output)
              (format stream ";;;; Autogenerated ASD file for system ~S~%"
                      (string-upcase (asdf:component-name system)))
              (princ asdf-definition stream)
              (terpri stream)
              (force-output stream))
            (format t "### Wrote file ~%~3t~S .~%### Finished.~%" pathname))
        (retrieve ()
          :report "Recover original file from the backup"
          (format t "### Recovering...~%")
          (when (probe-file pathname)
            (delete-file pathname))
          (rename-file backup pathname)))
      pathname)))

(defun regen (&key (im-sure nil))
  (write-asd (intern (package-name *package*) (find-package :keyword)) :im-sure im-sure))
