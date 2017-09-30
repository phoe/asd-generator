(("package")        ;single element list (<PATH>) is an abbreviation of (:file <PATH>)
 (:file "constants")
 (:file "constants2" :depends-on ("constants")) ; you have to specify :file when you use other features
 (:cffi-grovel-file "grovel") ; works well with asdf extensions
 ;;
 (:dir :src                             ; abbreviation of :module + :components.
       ("a")
       ("b")
       (:dir "sub" ("a"))
       
       (:rest)
       ;; Traverse the current directory recursively and expands to the list of
       ;; files that are not included by any other directives.
       ;; 
       ;; This means that the expansion is affected by the components not just
       ;; before, but also after (:rest).  Thus the files "a", "b", "sub/a", "c",
       ;; "rest", all files below "more-grovels", all files below "non-recursive",
       ;; all files below "sub2" excluded.
       ;; 
       ;; This could be somewhat similar to the behavior of
       ;; (call-next-method).
       
       ;; (:rest :as :file)    ; You can specify the component type (:file by default).

       ("c")
       
       ;; A file rest.lisp should be included by (:file "rest")
       (:file "rest")
       
       (:dir "more-grovels"
             (:rest :as :cffi-grovel-file)) ; Specifying the component type.
    
       (:dir "non-recursive"
             ;; You can disable the recursive traversal.  Note that while the
             ;; expansion includes only the files immediately below
             ;; "non-recursive" and does not include the files in the
             ;; subdirectory "non-recursive/sub", the files below
             ;; "non-recursive/sub" are also excluded from the above (:rest)
             ;; directive. In other words, all files below "non-recursive" are
             ;; treated as if they are already included.
             (:rest :recursive nil))
         
       (:dir "sub2")                  ; directory without subcomponents imply (:rest).
       ;; (:dir "sub2" (:rest))       ; eqivalent definition
       ))
