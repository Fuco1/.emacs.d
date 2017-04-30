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
        (my-buffer-equals "foo|  bar"))))


  (describe "my-newline"

    (it "should insert new line when at the end of line."
      (my-test-with-temp-elisp-buffer "foo|"
        (my-newline)
        (my-buffer-equals "foo
|")))

    (it "should indent according to mode."
      (my-test-with-temp-elisp-buffer "(foo|)"
        (my-newline)
        (my-buffer-equals "(foo
 |)")))

    (it "should run `my-newline-hook' after inserting the newline."
      (my-test-with-temp-elisp-buffer "(foo|)"
        (add-hook 'my-newline-hook (lambda (&optional arg) (insert "bar")) nil :local)
        (my-newline)
        (my-buffer-equals "(foo
 bar|)"))))


  (describe "my--back-to-indentation"

    (it "should go back to indent when `visual-line-mode' is off."
      (spy-on 'beginning-of-visual-line)

      (my-test-with-temp-elisp-buffer "  foo|"
        (my--indentation-position)
        (expect 'beginning-of-visual-line :not :to-have-been-called)))

    (it "should go back to visual indent when `visual-line-mode' is on."
      (spy-on 'beginning-of-visual-line)

      (my-test-with-temp-elisp-buffer "  foo|"
        (visual-line-mode 1)
        (my--indentation-position)
        (expect 'beginning-of-visual-line :to-have-been-called)))))
