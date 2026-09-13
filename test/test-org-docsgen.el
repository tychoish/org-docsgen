;;; test-org-docsgen.el --- Tests for org-docsgen -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>

;;; Code:

(require 'ert)
(require 'org)
(require 'org-docsgen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. File discovery tests

(ert-deftest org-docsgen-test-find-el-files ()
  "Test `org-docsgen--find-el-files' filtering and sorting."
  (let ((tmp-dir (make-temp-file "org-docsgen-test-" t)))
    (unwind-protect
        (let ((file-sample (expand-file-name "sample.org" tmp-dir))
              (good-files '("alpha.el" "beta.el" "sub-module.el"))
              (bad-files '("alpha-test.el" "test-beta.el" "sample-pkg.el" "sample-autoloads.el" "notes.txt")))
          (write-region "" nil file-sample)
          ;; Create good files
          (dolist (f good-files)
            (write-region "" nil (expand-file-name f tmp-dir)))
          ;; Create excluded files
          (dolist (f bad-files)
            (write-region "" nil (expand-file-name f tmp-dir)))
          ;; Create test/ subdir with file
          (make-directory (expand-file-name "test" tmp-dir))
          (write-region "" nil (expand-file-name "test/nested.el" tmp-dir))

          (with-current-buffer (find-file-noselect file-sample)
            (let ((found (org-docsgen--find-el-files)))
              (should (equal (mapcar #'file-name-nondirectory found)
                             '("alpha.el" "beta.el" "sub-module.el"))))))
      (delete-directory tmp-dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Doc extraction and formatting tests

(ert-deftest org-docsgen-test-defkind ()
  "Test `org-docsgen--defkind' kind classification."
  (should (eq (org-docsgen--defkind "defun") 'function))
  (should (eq (org-docsgen--defkind "defmacro") 'function))
  (should (eq (org-docsgen--defkind "defsubst") 'function))
  (should (eq (org-docsgen--defkind "cl-defun") 'function))
  (should (eq (org-docsgen--defkind "define-minor-mode") 'function))
  (should (eq (org-docsgen--defkind "defvar") 'variable))
  (should (eq (org-docsgen--defkind "defconst") 'variable))
  (should (eq (org-docsgen--defkind "defvar-local") 'variable))
  (should (eq (org-docsgen--defkind "defcustom") 'custom))
  (should (eq (org-docsgen--defkind "defface") 'face))
  (should (eq (org-docsgen--defkind "defgroup") 'group))
  (should (null (org-docsgen--defkind "setq"))))

(ert-deftest org-docsgen-test-fn-arg-names ()
  "Test `org-docsgen--fn-arg-names' parsing of arglist signatures."
  (should (equal (org-docsgen--fn-arg-names "(fn FOO BAR &optional BAZ &rest REST)")
                 '("FOO" "BAR" "BAZ" "REST")))
  (should (null (org-docsgen--fn-arg-names nil)))
  (should (null (org-docsgen--fn-arg-names "Just a regular paragraph."))))

(ert-deftest org-docsgen-test-quote-symbol-refs ()
  "Test `org-docsgen--quote-symbol-refs' renders bound symbols as code spans."
  (let ((text "Use ‘car’ and ‘nonexistent-fake-symbol-xyz’ here."))
    ;; `car' is a bound function, `nonexistent-fake-symbol-xyz' is not
    (should (string-match-p "~car~" (org-docsgen--quote-symbol-refs text)))
    (should (string-match-p "‘nonexistent-fake-symbol-xyz’" (org-docsgen--quote-symbol-refs text)))))

(ert-deftest org-docsgen-test-emphasize-args ()
  "Test `org-docsgen--emphasize-args' emphasizes formal argument words."
  (let ((text "Return FOO when BAR is non-nil.")
        (args '("FOO" "BAR")))
    (should (equal (org-docsgen--emphasize-args text args)
                   "Return /FOO/ when /BAR/ is non-nil."))))

(ert-deftest org-docsgen-test-format-doc ()
  "Test `org-docsgen--format-doc' formats docstring and separates signature."
  (let* ((raw-doc "First line of doc for ARG1.\nSecond line.\n\n(fn ARG1 &optional ARG2)\n\nAnother paragraph.")
         (formatted (org-docsgen--format-doc raw-doc :default)))
    (should (string-match-p "^: (fn ARG1 &optional ARG2)" formatted))
    (should (string-match-p "First line of doc for /ARG1/\\. Second line\\." formatted))
    (should (string-match-p "/ARG1/" formatted))))

(ert-deftest org-docsgen-test-include-p ()
  "Test `org-docsgen--include-p' scope and kind filters."
  ;; Public function
  (should (org-docsgen--include-p "my-func" 'function nil nil 'exported '(variables customs) "my"))
  ;; Internal function (-- in name)
  (should-not (org-docsgen--include-p "my--internal" 'function nil nil 'exported '(variables customs) "my"))
  ;; Autoloaded scope
  (should (org-docsgen--include-p "my-func" 'function t nil 'autoloaded '(variables customs) "my"))
  (should-not (org-docsgen--include-p "my-func" 'function nil nil 'autoloaded '(variables customs) "my"))
  ;; Foreign variable without namespace
  (should-not (org-docsgen--include-p "foreign-var" 'variable nil t 'exported '(variables) "my"))
  ;; Namespace matching variable
  (should (org-docsgen--include-p "my-var" 'variable nil t 'exported '(variables) "my")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Org heading generation and table/symbol creation tests

(ert-deftest org-docsgen-test-heading ()
  "Test `org-docsgen--heading' star prefix."
  (should (equal (org-docsgen--heading 1) "* "))
  (should (equal (org-docsgen--heading 2) "** "))
  (should (equal (org-docsgen--heading 3) "*** ")))

(ert-deftest org-docsgen-test-section-name ()
  "Test `org-docsgen--section-name' converts comments to title case."
  (should (equal (org-docsgen--section-name "helpers and utilities -- internal")
                 "Helpers And Utilities"))
  (should (equal (org-docsgen--section-name "core-api -- primary entry points")
                 "Core Api")))

(ert-deftest org-docsgen-test-format-sym ()
  "Test `org-docsgen--format-sym' heading, properties, and custom IDs."
  (defun org-docsgen-test--dummy-fn (x)
    "A dummy function taking X for testing."
    x)
  (let ((out (org-docsgen--format-sym "org-docsgen-test--dummy-fn" 'function "** " nil
                                      "/path/to/test.el" 10 t 'org)))
    (should (string-match-p "\\`\\*\\* \\[\\[file:.*test\\.el::10\\]\\[org-docsgen-test--dummy-fn\\]\\]" out))
    (should (string-match-p ":CUSTOM_ID: org-docsgen-test--dummy-fn" out))
    (should (string-match-p "A dummy function taking X for testing\\." out))))

(ert-deftest org-docsgen-test-partition-by-groups ()
  "Test `org-docsgen--partition-by-groups' grouping logic."
  (let ((files '("/path/foo-bar.el" "/path/foo-baz.el" "/path/quux.el")))
    ;; Group by file
    (let ((by-file (org-docsgen--partition-by-groups files 'file)))
      (should (equal (mapcar #'car by-file) '("foo-bar" "foo-baz" "quux"))))
    ;; Group by explicit prefixes
    (let ((by-spec (org-docsgen--partition-by-groups files '("foo" "quux"))))
      (should (equal (car (nth 0 by-spec)) "foo"))
      (should (= (length (cdr (nth 0 by-spec))) 2))
      (should (equal (car (nth 1 by-spec)) "quux"))
      (should (= (length (cdr (nth 1 by-spec))) 1)))))

(ert-deftest org-docsgen-test-clear-subtree ()
  "Test `org-docsgen--clear-subtree' removes nested headings and content."
  (with-temp-buffer
    (org-mode)
    (insert "* Root\nContent under root\n** Child 1\nChild 1 content\n** Child 2\nChild 2 content\n* Next Root\nNext content\n")
    (goto-char (point-min))
    (org-docsgen--clear-subtree)
    (should (equal (buffer-string) "* Root\nContent under root\n\n* Next Root\nNext content\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Regeneration logic tests

(ert-deftest org-docsgen-test-buffer-and-file-has-run-p ()
  "Test `org-docsgen--buffer-has-run-p' and `org-docsgen--file-has-run-p'."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN_SRC emacs-lisp\n(org-docsgen-run)\n#+END_SRC\n")
    (should (org-docsgen--buffer-has-run-p)))
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN_SRC emacs-lisp\n(message \"hello\")\n#+END_SRC\n")
    (should-not (org-docsgen--buffer-has-run-p)))
  (let ((tmp (make-temp-file "docsgen-test-" nil ".org")))
    (unwind-protect
        (progn
          (write-region "#+BEGIN_SRC emacs-lisp\n(org-docsgen-run)\n#+END_SRC\n" nil tmp)
          (should (org-docsgen--file-has-run-p tmp)))
      (delete-file tmp))))

(ert-deftest org-docsgen-test-execute-run-block-and-regenerate ()
  "Test `org-docsgen-regenerate-file' regenerates documentation."
  (let ((tmp-dir (make-temp-file "org-docsgen-reg-" t)))
    (unwind-protect
        (let* ((el-file (expand-file-name "demo.el" tmp-dir))
               (org-file (expand-file-name "README.org" tmp-dir))
               (el-code ";;; demo.el --- Demo package -*- lexical-binding: t; -*-\n\n;;;###autoload\n(defun demo-hello (name)\n  \"Say hello to NAME.\n\n(fn NAME)\"\n  (message \"Hello %s\" name))\n\n(provide 'demo)\n;;; demo.el ends here\n")
               (org-code (concat "* API Reference\n:PROPERTIES:\n:CUSTOM_ID: api-reference\n:END:\n\n"
                                 "#+BEGIN_SRC emacs-lisp :results output raw :exports none\n"
                                 "(org-docsgen-run :el-files '(\"" el-file "\"))\n"
                                 "#+END_SRC\n\n"
                                 "#+RESULTS:\n")))
          (write-region el-code nil el-file)
          (write-region org-code nil org-file)

          ;; Regenerate the file
          (org-docsgen-regenerate-file org-file)

          (with-temp-buffer
            (insert-file-contents org-file)
            (let ((content (buffer-string)))
              (should (string-match-p ":CUSTOM_ID: demo-hello" content))
              (should (string-match-p "Say hello to /NAME/\\." content)))))
      (delete-directory tmp-dir t))))

(provide 'test-org-docsgen)
;;; test-org-docsgen.el ends here
