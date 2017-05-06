;; -*- lexical-binding: t -*-

(require 'my-test-helper)

(require 'dired-defs)

(require 'assess)

(describe "Dired"

  (it "should build imenu for dired"
    (shut-up
      (assess-with-filesystem '("first-dir/foo"
                                "another-dir/bar")
        (dired ".")
        (goto-char (point-min))
        (search-forward "first-dir")
        (call-interactively 'dired-maybe-insert-subdir)
        (goto-char (point-min))
        (search-forward "another-dir")
        (call-interactively 'dired-maybe-insert-subdir)
        (let ((alist (imenu--make-index-alist)))
          (expect (assoc "first-dir" alist) :to-be-truthy)
          (expect (assoc "another-dir" alist) :to-be-truthy)
          (expect (assoc (f-base default-directory) alist) :to-be-truthy)))))

  (it "should revert the buffer after `dired-create-directory'."
    (shut-up
      (assess-with-filesystem '("bbbbb")
        (dired ".")
        (goto-char (point-max))
        (dired-create-directory "aaaaa")
        (goto-char (point-min))
        (expect (< (save-excursion
                     (search-forward "aaaaa"))
                   (save-excursion
                     (search-forward "bbbbb")))
                :to-be-truthy))))

  (it "should go to newly created directory after `dired-create-directory'."
    (shut-up
      (assess-with-filesystem '("bbbbb")
        (dired ".")
        (goto-char (point-max))
        (dired-create-directory "aaaaa")
        (expect (looking-at "aaaaa") :to-be-truthy)))))
