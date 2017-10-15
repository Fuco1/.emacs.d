;; -*- lexical-binding: t -*-

(require 'my-test-helper)

(require 'my-defuns-edit)

(describe "Edit function"


  (describe "my-forward-whitespace"

    (it "should skip whitespace before a symbol."
      (my-test-with-temp-elisp-buffer "|   foo bar"
        (my-forward-whitespace)
        (expect (point) :to-be 4)))

    (it "should not skip a symbol if directly at its start."
      (my-test-with-temp-elisp-buffer "foo   |bar   baz"
        (my-forward-whitespace)
        (expect (point) :to-be 7))))


  (describe "my-kill-whitespace"

    (it "should kill whitespace before the point."
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


  (describe "my-open-line"

    (it "should open new line above if the point is before indentation."
      (my-test-with-temp-elisp-buffer "(progn
|  (foo)
  (bar))"
        (my-open-line)
        (my-buffer-equals "(progn
  |
  (foo)
  (bar))")))

    (it "should open new line above if the point is at indentation."
      (my-test-with-temp-elisp-buffer "(progn
  |(foo)
  (bar))"
        (my-open-line)
        (my-buffer-equals "(progn
  |
  (foo)
  (bar))")))

    (it "should open new line below if the point is after indentation."
      (my-test-with-temp-elisp-buffer "(progn
  (f|oo)
  (bar))"
        (my-open-line)
        (my-buffer-equals "(progn
  (foo)
  |
  (bar))")))

    (it "should open new line at point if called with C-u."
      (my-test-with-temp-elisp-buffer "(progn
  (f|oo)
  (bar))"
        (my-open-line '(4))
        (my-buffer-equals "(progn
  (f|
   oo)
  (bar))"))))


  (describe "my-forward-line-and-indent"

    (it "should move a line forward, indent the line and go back to indentation."
      (my-test-with-temp-elisp-buffer "(pro|gn
 (foo))"
        (my-forward-line-and-indent)
        (my-buffer-equals "(progn
  |(foo))"))))


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
        (expect 'beginning-of-visual-line :to-have-been-called))))


  (describe "my-back-to-indentation-or-beginning"


    (describe "when not in special mode"

      (it "should go back to indentation if the point is after the indentation."
        (my-test-with-temp-elisp-buffer "  fo|o"
          (my-back-to-indentation-or-beginning)
          (my-buffer-equals "  |foo")))

      (it "should go back to indentation if the point is before the indentation."
        (my-test-with-temp-elisp-buffer " | foo"
          (my-back-to-indentation-or-beginning)
          (my-buffer-equals "  |foo")))

      (it "should go back to indentation if the point is at the beginning of line."
        (my-test-with-temp-elisp-buffer " | foo"
          (my-back-to-indentation-or-beginning)
          (my-buffer-equals "  |foo")))

      (it "should go back to beginning of line if the point is at the indentation."
        (my-test-with-temp-elisp-buffer "  |foo"
          (my-back-to-indentation-or-beginning)
          (my-buffer-equals "|  foo")))

      (it "should go back to comment beginning if inside the comment body."
        (my-test-with-temp-buffer "/*
 * Some rather bo|ring comment
 */
int foo();"
            (progn
              (c++-mode)
              (save-excursion (syntax-ppss 1)))
          (expect (my--point-in-comment) :to-be-truthy)
          (my-back-to-indentation-or-beginning)
          (my-buffer-equals "/*
 * |Some rather boring comment
 */
int foo();")))

      (it "should go back to indent if at comment beginning."
        (my-test-with-temp-buffer "/*
 * |Some rather boring comment
 */"
            (c-mode)
          (my-back-to-indentation-or-beginning)
          (my-buffer-equals "/*
 |* Some rather boring comment
 */")))

      (it "should go back to beginning of line if at indent before a comment."
        (my-test-with-temp-buffer "/*
 |* Some rather boring comment
 */"
            (c-mode)
          (my-back-to-indentation-or-beginning)
          (my-buffer-equals "/*
| * Some rather boring comment
 */"))))


    (describe "when in an org table"

      (it "should go to the beginning of current cell if not there."
        (my-test-with-temp-buffer "|| foo | bar | baz |
|   1 |   2 | X 3 |
"
            (org-mode)
          (re-search-forward "X")
          (replace-match " ")
          (my-back-to-indentation-or-beginning)
          (expect (point) :to-be 37)))

      (it "should go to the beginning of first cell if at the beginning of current cell."
        (my-test-with-temp-buffer "|| foo | bar | baz |
|   1 |   2 |  X3 |
"
            (org-mode)
          (re-search-forward "X")
          (replace-match " ")
          (my-back-to-indentation-or-beginning)
          (expect (point) :to-be 25)))

      (it "should go to the beginning of line if at the beginning of first cell."
        (my-test-with-temp-buffer "|| foo | bar | baz |
|  X1 |   2 |   3 |
"
            (org-mode)
          (re-search-forward "X")
          (replace-match " ")
          (my-back-to-indentation-or-beginning)
          (expect (point) :to-be 21)))

      (it "should go to the beginning of first cell if at the beginning of current line."
        (my-test-with-temp-buffer "|| foo | bar | baz |
X|   1 |   2 |   3 |
"
            (org-mode)
          (re-search-forward "X")
          (replace-match "")
          (my-back-to-indentation-or-beginning)
          (expect (point) :to-be 25))))


    (describe "when in dired"

      (it "should go to the beginning of the file name if not there."
        (dired ".")
        (dired-filter-mode -1)
        (goto-char (point-min))
        (forward-line 3)
        (forward-char 20)
        (let ((expected (save-excursion
                          (search-forward "..")
                          (backward-char 2)
                          (current-column))))
          (my-back-to-indentation-or-beginning)
          (expect (current-column) :to-be expected)))

      (it "should go to the beginning of permissions if at file name."
        (dired ".")
        (dired-filter-mode -1)
        (goto-char (point-min))
        (search-forward "..")
        (backward-char 2)
        (my-back-to-indentation-or-beginning)
        (expect (- (point) (line-beginning-position)) :to-be 2))))

  )
