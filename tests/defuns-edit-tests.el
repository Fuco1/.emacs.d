;; -*- lexical-binding: t -*-

(require 'my-test-helper)
(require 'my-defuns-edit)

(describe "Edit functions"


  (describe "my-forward-whitespace"

    (it "should skip whitespace before a symbol"
      (my-test-with-temp-elisp-buffer "|   foo bar"
        (my-forward-whitespace)
        (expect (point) :to-be 4)))

    (it "should not skip a symbol if directly at its start."
      (my-test-with-temp-elisp-buffer "foo   |bar   baz"
        (my-forward-whitespace)
        (expect (point) :to-be 7))))


  (describe "my-kill-whitespace"

    (it "should kill whitespace before the point"
      (my-test-with-temp-elisp-buffer "foo    |  bar"
        (my-kill-whitespace)
        (my-buffer-equals "foo|  bar")))))
