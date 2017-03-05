# lisp

This directory contains my own Emacs lisp functions which are not yet packaged or are too small or too specific (i.e. only useful for me) for packaging.

Somewhat general-purpose code is located in `my-*.el` files which are thematically organized.  I use `;;;###autoload` and provide/require mechanism to lazy-load the functions.

Feel free to browse and steal, but be warned this is quite a huge ball of hairy code :)

## List of files

* `my-advices.el` - advices for functions in Emacs proper
* `my-defuns-buffer.el` - functions related to buffers or buffer management
* `my-defuns-edit.el` - editing functions
* `my-defuns.el` - everything else that is a function
* `my-redef.el` - redefinitions of functions where advice would not suffice; these are usually quite specific hacks and tweaks to make things work just the way *I* want.
* `vendor.el` - file with `use-package` definitions for the `special/` "packages".
