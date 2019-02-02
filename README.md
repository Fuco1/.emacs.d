# .emacs.d [![Build Status](https://travis-ci.org/Fuco1/.emacs.d.svg?branch=master)](https://travis-ci.org/Fuco1/.emacs.d)

The pre-git history is at: https://bitbucket.org/Fuco/.emacs.d/

# Installation

First, install cask

``` shell
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
```

Then from `.emacs.d` directory run

``` shell
cask install
make
```

This should get you reasonably close to a runnable installation.

# Layout

* etc: extra configuration files (not elisp)
* files: configuration for various packages (elisp)
* projects: directory with git submodules of the packages I maintain (I live on the bleeding edge :)
* site-lisp: extra elisp functions not yet bundled in a package
* tests: unit/integration tests for changes my configuration makes
* themes: holds my theme (modified tango)

# Tests

Emacs configuration **is** a program so it deserves to be tested.  Show some love to your config and add tests!
