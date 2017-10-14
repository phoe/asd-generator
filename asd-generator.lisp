;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ASD-GENERATOR
;;;; Michał "phoe" Herda - phoe@openmailbox.org
;;;; GPLv3
(in-package #:asd-generator)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pathnames, utils

(defmacro ensure-system (place)
  "Ensure the PLACE is a system"
  `(setf ,place (asdf:find-system ,place)))

(defun get-unix-time ()
  (- (get-universal-time) 2208988800))

(defun backup-pathname (pathname)
  (cl-fad:merge-pathnames-as-file
   (cl-fad:pathname-directory-pathname pathname)
   (concatenate 'string "."
                (string-downcase (pathname-name pathname)) "."
                (format nil "~D" (get-unix-time)) ".backup")))

(defun to-string (symbol)
  (string-downcase (string symbol)))

;;; directory traversal

(defun all-pathnames (directory)
  (flatten (iterate (for entry in (cl-fad:list-directory directory))
	     (when (cl-fad:directory-pathname-p entry)
	       (collect (all-pathnames entry)))
	     (collect entry))))

;;; data

(defun generate-components (system data)
  "Generate a form for the :COMPONENT section of the ASDF definition of SYSTEM, from the given DATA"
  (expand data (asdf:component-pathname system))) ;absolute

(defun expand (data dir)                ;dir is an absolute pathname string
  (let (paths)                          ;paths are also absolute
    (values (mappend
             #'funcall
             ;; Perform a lazy evaluation, so that :rest can correctly capture the already
             ;; included components
             (iter (for component in data)
                   (collecting
                    (match component
                      
                      ((list* :rest keywords)
                       (lambda ()
                         ;; (print dir)
                         ;; (print paths)
                         ;; (terpri)
                         (multiple-value-bind (expanded subpaths) (apply #'process-rest-form dir paths keywords)
                           (appendf paths subpaths)
                           expanded)))

                      ;; should be later than :rest because (:rest) is parsed incorrectly
                      ((list x)                     ;single element is an abbreviation of (:file x)
                       (push (merge-pathnames (to-string x) dir) paths)
                       (constantly `((:file ,(to-string x)))))
                      
                      ((list* :dir x rest)
                       (when (null rest)
                         (setf rest '((:rest)))) 
                       (multiple-value-bind (expanded subpaths)
                           (expand rest (merge-pathnames (make-pathname :directory `(:relative ,(to-string x))) dir))
                         (appendf paths subpaths)
                         (constantly `((:module ,(to-string x) :components ,expanded)))))
                      
                      ((list* type x options)
                       (push (merge-pathnames (to-string x) dir) paths)
                       (constantly `((,type ,(to-string x) ,@options))))

                      #+(or)
                      (_
                       ;; non-component options, e.g. :serial and t
                       (constantly `(,component)))))))
            
            ;; pass the included paths to the upper level
            paths)))

(defun process-rest-form (dir paths &key (as :file) (recursive t) (type "lisp"))
  (flet ((typeless (x) (make-pathname :type nil :defaults x)))
    (let ((included-paths (set-difference
                           (mapcar #'typeless
                                   (remove type
                                           (if recursive
                                               (all-pathnames dir)
                                               (remove-if #'cl-fad:directory-pathname-p
                                                          (cl-fad:list-directory dir)))
                                           :test (complement #'equal)
                                           :key #'pathname-type))
                           paths
                           :test #'equal))
          (excluded-paths (set-difference
                           (mapcar #'typeless
                                   (remove type
                                           (all-pathnames dir)
                                           :test (complement #'equal)
                                           :key #'pathname-type))
                           paths
                           :test #'equal)))
      (values
       (sort (mapcar (lambda (x) `(,as ,(enough-namestring x dir)))
                     included-paths)
             #'string<
             :key #'second)
       excluded-paths))))

;;; main functions

(defun read-asd (asd-pathname)
  "Read a definition file and find the asdf definition.
Handle older ASDF files which contain multiple forms.
The recent ASDF assumes one system per file."
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

(defun generate-asd (system &optional data)
  (ensure-system system)
  (unless data
    (setf data (asdf:system-relative-pathname system "asd-generator-data.asd")))
  (let* ((asd-pathname (asdf:system-source-file system))
	 (generator-data
          (progn
            (assert (file-exists-p data) (data)
                    "The file ~a was not found." data)
            (with-open-file (stream data) (read stream)))))
    (let ((data (read-asd asd-pathname)))
      (setf (getf data :components) (generate-components system generator-data))
      (values data
              asd-pathname
              (backup-pathname asd-pathname)))))

(defun write-asd (system &key (im-sure nil) data)
  (ensure-system system)
  (multiple-value-bind (asdf-definition pathname) (generate-asd system data)
    (let ((backup (backup-pathname pathname)))
      (format t "### This will write a new file at:~%~3t~S~%" pathname)
      (format t "### and store the backup of the original at:~%~3t~S~%" backup)
      (unless im-sure
        (if (y-or-n-p "### Continue?")
            (format t "### Alright.~%")
            (return-from write-asd
              (format t "### Aborted.~%"))))
      (restart-case
          (progn
            (rename-file pathname backup)
            (format t "### Renamed file ~%~3t~S~%### to ~%~3t~S.~%" pathname backup)
            (with-open-file (stream pathname :direction :output)
              (format stream ";;;; Autogenerated ASD file for system ~S~%"
                      (string-upcase (asdf:component-name system)))
              (format stream ";;;; In order to regenerate it, run update-asdf~%")
	      (format stream ";;;; from shell (see https://github.com/phoe-krk/asd-generator)~%")
              (format stream ";;;; For those who do not have update-asdf,~%")
	      (format stream ";;;; run `ros install asd-generator` (if you have roswell installed)~%")
              (format stream ";;;; There are also an interface available from lisp:~%")
	      (format stream ";;;; (asd-generator:regen &key im-sure)~%")
              (pprint-asd asdf-definition stream)
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

(defun pprint-asd (asdf-definition &optional (*standard-output* *standard-output*))
  (let ((*print-case* :downcase))
    (pprint-logical-block (*standard-output* asdf-definition :prefix "(" :suffix ")")
      (loop
        (pprint-exit-if-list-exhausted)
        (let ((key (pprint-pop)))
          (prin1 key)
          (write-char #\Space)
          (case key
            (:components
             (pprint-logical-block (*standard-output* (pprint-pop) :prefix "(" :suffix ")")
               (loop
                 (pprint-exit-if-list-exhausted)
                 (pprint-asd (pprint-pop) *standard-output*)
                 (pprint-exit-if-list-exhausted)
                 (pprint-newline :mandatory))))
	    (:depends-on
             (pprint-logical-block (*standard-output* (pprint-pop) :prefix "(" :suffix ")")
               (loop
                 (pprint-exit-if-list-exhausted)
                 (pprint-asd (pprint-pop) *standard-output*)
                 (pprint-exit-if-list-exhausted)
                 (pprint-newline :mandatory))))
            (otherwise
             (pprint-fill *standard-output* (pprint-pop)))))
        (pprint-exit-if-list-exhausted)
        (pprint-newline :mandatory)))))

(defun regen (&key (im-sure nil))
  (write-asd (intern (package-name *package*) (find-package :keyword)) :im-sure im-sure))
