(("package")        ;single element list (<PATH>) is an abbreviation of (:file <PATH>)
 (:file "constants")
 (:file "constants2" :depends-on ("constants")) ; you have to specify :file when you use other features
 (:cffi-grovel-file "grovel")           ; works well with asdf extensions
 ;;
 (:dir :src                             ; abbreviation of :module + :components.
       ("a")
       ("b")
       (:dir "sub"
             ("a"))
       (:rest)                          ; Descends into subdirectories and
                                        ; expands to the rest of the files not
                                        ; already included, as :file components.
                                        ; Thus sub/a and all files below more-grovels are not included here.
       ;; (:rest :as :file)             ; You can specify the component type (:file by default).
       ;; 
       ;; A file rest.lisp should be included by (:file "rest")
       (:file "rest")
       (:dir "more-grovels"
             (:rest :as :cffi-grovel-file)) ; Specifying the component type.
    
       (:dir "non-recursive"
             (:rest :recursive nil))  ; you can disable the recursive includes. 
         
       (:dir "sub2")                  ; directory without subcomponents imply (:rest)
       ;; (:dir "sub2" (:rest))       ; eqivalent definition
       ("c")))
