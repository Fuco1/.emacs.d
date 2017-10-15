;; -*- lexical-binding: t -*-

(require 'my-test-helper)

(require 'org-defs)
(require 'org-defs-tangled)

(require 'assess)

(defmacro my-with-temp-org-file (initial &rest body)
  "Setup a temporary org file with INITIAL content then execute BODY."
  (declare (indent 1))
  `(let ((file (make-temp-file "tests")))
     (unwind-protect
         (my-test-with-temp-buffer ,initial
             (org-mode)
           (shut-up
             (write-region nil nil file)
             (set-visited-file-name file)
             ,@body))
       (ignore-errors (delete-file file)))))

(describe "Org"

  (describe "refile"

    (it "should remove inherited tags from the entry after refile."
      (my-with-temp-org-file "* |refile :math:
* A :foo:
* B :bar:
** Sub-B1
** Sub-B2 :math:
"
        (org-refile nil nil (list "Sub-B2" (buffer-file-name) "" 47))
        (my-buffer-equals "|* A :foo:
* B :bar:
** Sub-B1
** Sub-B2 :math:
*** refile
")))

    (it "should keep non-inherited tags on the entry after refile."
      (my-with-temp-org-file "* |refile :new:
* A :foo:
* B :bar:
** Sub-B1
** Sub-B2 :math:
"
        (org-refile nil nil (list "Sub-B2" (buffer-file-name) "" 46))
        (my-buffer-equals "|* A :foo:
* B :bar:
** Sub-B1
** Sub-B2 :math:
*** refile :new:
")))

    (it "should resolve multiple tags after refile."
      (my-with-temp-org-file "* |refile :math:new:bar:
* A :foo:
* B :bar:
** Sub-B1
** Sub-B2 :math:
"
        (org-refile nil nil (list "Sub-B2" (buffer-file-name) "" 55))
        (my-buffer-equals "|* A :foo:
* B :bar:
** Sub-B1
** Sub-B2 :math:
*** refile :new:
"))))


  (describe "agenda"

    (it "should remove duplicate habits from the agenda."
      (my-with-temp-org-file "* TODO Latin
  SCHEDULED: <2017-06-04 Sun +2d>
  :PROPERTIES:
  :STYLE:    habit
  :END:
  SCHEDULED: <2017-06-02 Fri +2d>
  SCHEDULED: <2017-06-03 Sat +2d>
"
        (let ((org-agenda-files (list (buffer-file-name)))
              (org-agenda-span 'day)
              (org-agenda-finalize-hook
               (list 'my-org-agenda-remove-duplicate-habits
                     'my-org-agenda-remove-empty-lists)))
          (org-agenda-list nil (time-to-days
                                (org-time-string-to-time "<2017-06-04>")))
          (goto-char (point-max))
          (expect (line-number-at-pos) :to-be 4))))


    (it "should not remove duplicate clocked entries from the agenda."
      (my-with-temp-org-file "* TODO Latin
  SCHEDULED: <2017-06-04 Sun +2d>
  :PROPERTIES:
  :STYLE:    habit
  :END:
  :CLOCK:
  CLOCK: [2017-06-04 Sun 21:30]--[2017-06-04 Sun 21:33] => 0:03
  CLOCK: [2017-06-04 Sun 21:24]--[2017-06-04 Sun 21:29] => 0:05
  :END:
"
        (let ((org-agenda-files (list (buffer-file-name)))
              (org-agenda-span 'day)
              (org-agenda-finalize-hook
               (list 'my-org-agenda-remove-duplicate-habits
                     'my-org-agenda-remove-empty-lists)))
          (org-agenda-list nil (time-to-days
                                (org-time-string-to-time "<2017-06-04>")))
          (org-agenda-log-mode 1)
          (goto-char (point-min))
          (save-excursion (expect (search-forward "(0:03)") :to-be-truthy))
          (save-excursion (expect (search-forward "(0:05)") :to-be-truthy)))))))
