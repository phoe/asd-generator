# asd-generator
Automatic generator for ASDF's .asd files.

This is still in alpha, but I use it to build my own projects, if it means anything. `:P`


This autoregenerates an .asd file with a customizable `:COMPONENTS` field, based on the file structure you provide.

The project must have a valid `.asd` file containing a single `asdf:defsystem` expression.

The project must have a `asd-generator-data.asd` file with the structure outlined below.


## Installation

Currently, not available from quicklisp.

With Roswell, you can:

    $ ros install phoe/asd-generator

## Lisp API
### write-asd
Syntax: `(write-asd system-designator &key im-sure)`

This writes the ASD file for a provided ASDF-loadable system.

You need to press Enter before anything happens, unless the `im-sure` key is set to non-nil.

### regen
Syntax: `(regen &key im-sure)`

This calls write-asd on the *current* package, as stated in the **`*package*`** variable.

You need to press Enter before anything happens, unless the `im-sure` key is set to non-nil.

## Command line API

    update-asdf [-y] [-C pathname] [-f data] [[-s SYSTEM]*]

Mostly corresponds to calling `regen` and `write-asd`.

* -y : Do not confirm; equivalent to `:im-sure t`.
* -C pathname : Specify the system pathname. Similar to `make -C path`
* -s SYSTEM : Specify which asdf system definition to update.
              This is because the current directory could contain multiple asdf definitions.
              By default, it selects the first system in the dictionary order of the names.
              Can be specified multiple times, in which case all definitions are updated.
              Do not include `.asd` in the system name.
* -f data : Specify the asd-generator-data file relative to the system pathname,
              or an absolute pathname. Defaulted to `asd-generator-data.asd`.
              Similar to `Makefile -f makefile.mk` .

## Known bugs/TODO
Save *all* of your files in Emacs before running this, as it will pull files like `.#file.lisp` into the `:COMPONENTS` tree along with all others.

## Example of usage:

Original asdf file:

```
(defsystem #:asd-generator-test
 :description "system for testing asd-generator"
 :author "Masataro Asai"
 :depends-on (#:cl-fad
              #:iterate
              #:alexandria)
 :serial t
 :components ())
```

asd-generator-data.asd

```common-lisp
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
       ;; all files below "sub2" and "c" will be excluded.
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
```

Result asdf file:

``` common-lisp
(defsystem #:asd-generator-test
 :description "system for testing asd-generator"
 :author "Masataro Asai"
 :depends-on (#:cl-fad
              #:iterate
              #:alexandria)
 :serial t
 :components ((:file "package")
              (:file "constants")
              (:file "constants2"
               :depends-on ("constants"))
              (:cffi-grovel-file "grovel")
              (:module "src"
               :components ((:file "a")
                            (:file "b")
                            (:module "sub"
                             :components ((:file "a")))
                            (:file "not-specified-anywhere/a")
                            (:file "not-specified-anywhere/b")
                            (:file "not-specified-anywhere/c")
                            (:file "c")
                            (:file "rest")
                            (:module "more-grovels"
                             :components ((:cffi-grovel-file "a")
                                          (:cffi-grovel-file "b")
                                          (:cffi-grovel-file "c")))
                            (:module "non-recursive"
                             :components ((:file "a")
                                          (:file "b")
                                          (:file "c")))
                            (:module "sub2"
                             :components ((:file "a")
                                          (:file "b")
                                          (:file "c")))))))
```
