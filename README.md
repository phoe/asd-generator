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

    update-asdf [-y] [-C pathname] [[-s SYSTEM]*]

Mostly corresponds to calling `regen` and `write-asd`.

* -y : equivalent to `:im-sure t`.
* -C pathname : specify the repository pathname. Similar to `make -C pathname`
* -s SYSTEM : Specify which asdf system definitions to update.

## Known bugs/TODO
Save *all* of your files in Emacs before running this, as it will pull files like `.#file.lisp` into the `:COMPONENTS` tree along with all others.

## Example of usage:

```common-lisp
ASD-GENERATOR> *data* 
;; an example of what should the asd-generator-data.asd contain
((#:package)
 (#:constants)
 (#:utils (#:macros (#:massert (#:massert)
                               (#:definitions))
                    (:rest))
          (#:functions))
 (#:arch (:rest)
         (#:server (:rest)
                   (#:shard)
                   (#:gem)
                   (#:jewel)
                   (#:crown)))
 (#:impl (:rest)
         (#:server (:rest)
                   (#:shard)
                   (#:gem)
                   (#:jewel)
                   (#:crown)))
 (:rest))

ASD-GENERATOR> (mapc-directory-tree "/home/phoe/quicklisp/local-projects/lispfurc-new/" "lisp")
;; an example file structure
(#P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/connection.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/crown/crown.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/crown/listener.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/gem/gem.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/jewel/jewel.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/shard/chat.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/shard/message.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/shard/persona.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/shard/shard.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/arch/server/shard/world.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/constants.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/logger.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/connection.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/crown/crown.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/crown/listener.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/gem/gem-loop.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/gem/gem.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/jewel/jewel.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/shard/chat.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/shard/message.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/shard/persona.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/shard/shard.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/impl/server/shard/world.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/lispfurc-new.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/package.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/utils/functions/array/array.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/utils/functions/constructors.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/utils/functions/varia.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/utils/macros/logger.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/utils/macros/massert/definitions.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/utils/macros/massert/massert.lisp"
 #P"/home/phoe/quicklisp/local-projects/lispfurc-new/utils/macros/varia.lisp")

ASD-GENERATOR> (generate "/home/phoe/quicklisp/local-projects/lispfurc-new/" *data*)
;; an internal function that generates the :COMPONENTS part of an .asd file
((:FILE "package")
 (:FILE "constants")
 (:FILE "utils/macros/massert/massert")
 (:FILE "utils/macros/massert/definitions")
 (:FILE "utils/macros/logger")
 (:FILE "utils/macros/varia")
 (:FILE "utils/functions/array/array")
 (:FILE "utils/functions/constructors")
 (:FILE "utils/functions/varia")
 (:FILE "arch/server/connection")
 (:FILE "arch/server/shard/chat")
 (:FILE "arch/server/shard/message")
 (:FILE "arch/server/shard/persona")
 (:FILE "arch/server/shard/shard")
 (:FILE "arch/server/shard/world")
 (:FILE "arch/server/gem/gem")
 (:FILE "arch/server/jewel/jewel")
 (:FILE "arch/server/crown/crown")
 (:FILE "arch/server/crown/listener")
 (:FILE "impl/logger")
 (:FILE "impl/server/connection")
 (:FILE "impl/server/shard/chat")
 (:FILE "impl/server/shard/message")
 (:FILE "impl/server/shard/persona")
 (:FILE "impl/server/shard/shard")
 (:FILE "impl/server/shard/world")
 (:FILE "impl/server/gem/gem-loop")
 (:FILE "impl/server/gem/gem")
 (:FILE "impl/server/jewel/jewel")
 (:FILE "impl/server/crown/crown")
 (:FILE "impl/server/crown/listener")
 (:FILE "lispfurc-new"))
```
