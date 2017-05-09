;; -*- lexical-binding: t -*-

(require 'my-test-helper)

(require 'my-macros)

(describe "Macros"


  (describe "my-with-temporary-hook"

    (it "should add a hook for the duration of body and then remove it"
      (my-test-with-temp-buffer "" (fundamental-mode)
        (my-with-temporary-hook 'post-self-insert-hook
          (lambda () (insert "x"))
          (execute-kbd-macro "aaa"))
        (my-buffer-equals "axaxax|"))
      (my-test-with-temp-buffer "" (fundamental-mode)
        (execute-kbd-macro "aaa")
        (my-buffer-equals "aaa|")))

    (it "should remove the hook in case of error"
      (my-test-with-temp-buffer "" (fundamental-mode)
        (ignore-errors
          (my-with-temporary-hook 'post-self-insert-hook
            (lambda () (insert "y"))
            (execute-kbd-macro "b")
            (error "error")))
        (execute-kbd-macro "bb")
        (my-buffer-equals "bybb|")))))
