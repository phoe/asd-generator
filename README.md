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

```common-lisp
(("package")        ;single element list (<PATH>) is an abbreviation of (:file <PATH>)
 (:file "constants")
 (:file "constants2" :depends-on ("constants")) ; you have to specify :file when you use other features
 (:cffi-grovel-file "grovel")           ; works well with asdf extensions
 ;;
 (:dir :src                             ; abbreviation of :module + :components.
       ("a")
       ("b")
       (:dir "sub"
             ("sub-a"))
       (:rest)                          ; Descends into subdirectories and
                                        ; expands to the rest of the files not
                                        ; already included, as :file components.
                                        ; Thus sub/sub-a and all files below more-grovels are not included here.
       ;; (:rest :as :file)             ; You can specify the component type (:file by default).
       (:dir "more-grovels"
             (:rest :as :cffi-grovel-file)) ; Specifying the component type.
    
       (:dir "non-recursive"
             (:rest :recursive nil))  ; you can disable the recursive includes. 
         
       (:dir "sub2")                  ; directory without subcomponents imply :rest
       ;; (:dir "sub2" (:rest))       ; eqivalent definition
       ("c")))
```

