# tests

Tests for my Emacs config.  I use the excellent [buttercup](https://github.com/jorgenschaefer/emacs-buttercup) to write the test cases.

# Usage

Run with

    make test

It is possible to specify additional buttercup options with environment variable `BUTTERCUP_OPTIONS`, e.g.

    BUTTERCUP_OPTIONS="-p refile" make test

to only run tests containing `refile` somewhere in the description.
