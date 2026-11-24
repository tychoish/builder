;;; test-builder-elisp-tools.el --- Tests for builder's Elisp package tools -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Covers `builder-elisp-check', `builder-scaffold', `builder-docs', the
;; worktree-aware package name and the MCP tools built on them.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'builder)
(require 'builder-elisp-check)
(require 'builder-scaffold)
(require 'builder-docs)

(defmacro builder-tools-test--with-dir (var &rest body)
  "Run BODY with VAR bound to a fresh temporary directory, deleted afterwards."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory (make-temp-file "builder-tools-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun builder-tools-test--write (file text)
  "Write TEXT to FILE, creating parent directories."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun builder-tools-test--package (root name &optional body)
  "Create package NAME under ROOT with BODY as its code; return the file."
  (let ((file (expand-file-name (concat name ".el") root)))
    (builder-tools-test--write
     file
     (format ";;; %1$s.el --- Demo package -*- lexical-binding: t; -*-\n\n;; Version: 0.1\n\n;;; Code:\n\n%2$s\n(provide '%1$s)\n;;; %1$s.el ends here\n"
             name (or body "(defun demo-fn () \"Return one.\" 1)")))
    file))

(defun builder-tools-test--tools (result tool)
  "Return the findings of TOOL in check RESULT."
  (seq-filter (lambda (f) (equal (plist-get f :tool) tool))
              (append (plist-get result :findings) nil)))

;;;; Package name in worktrees

(ert-deftest builder-test/package-name-plain-directory ()
  "A plain project directory names the package."
  (builder-tools-test--with-dir tmp
    (should (equal (builder-elisp-package--name (expand-file-name "demo" tmp)) "demo"))
    (should (equal (builder-elisp-package--name (expand-file-name "demo.el/" tmp)) "demo"))))

(ert-deftest builder-test/package-name-in-dot-wt-worktree ()
  "A .wt/<topic> worktree is named after its repository."
  (builder-tools-test--with-dir tmp
    (let ((wt (expand-file-name "demo/.wt/topic/" tmp)))
      (builder-tools-test--package wt "demo")
      (should (equal (builder-elisp-package--name wt) "demo"))
      (should (equal (cadr (builder-elisp-package--resolve wt)) "demo"))
      (should (builder-elisp-package-p wt)))))

(ert-deftest builder-test/package-name-worktree-keeps-matching-directory ()
  "A worktree directory that has its own <name>.el keeps that name."
  (builder-tools-test--with-dir tmp
    (let ((wt (expand-file-name "demo/.wt/other/" tmp)))
      (builder-tools-test--package wt "other")
      (should (equal (builder-elisp-package--name wt) "other")))))

;;;; builder-elisp-check

(ert-deftest builder-test/check-clean-package-passes ()
  "A clean package returns ok with no errors and leaves no .elc behind."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo")
    (let* ((builder-elisp-check-use-cache nil)
           (res (builder-elisp-check-package tmp :tools '(parens compile checkdoc))))
      (should (equal (plist-get res :status) "ok"))
      (should (= (plist-get res :errors) 0))
      (should (= (plist-get res :files) 1))
      (should (vectorp (plist-get res :findings)))
      (should-not (file-expand-wildcards (expand-file-name "*.elc" tmp))))))

(ert-deftest builder-test/check-unbalanced-parens-skip-other-checks ()
  "Unbalanced parens are the only finding reported for that file."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo" "(defun demo-fn () (+ 1 2)")
    (let* ((builder-elisp-check-use-cache nil)
           (res (builder-elisp-check-package tmp :tools '(parens compile checkdoc)))
           (findings (append (plist-get res :findings) nil)))
      (should (equal (plist-get res :status) "failed"))
      (should (seq-every-p (lambda (f) (equal (plist-get f :tool) "parens")) findings))
      (should (> (plist-get (car findings) :line) 0)))))

(ert-deftest builder-test/check-reports-compile-warnings-with-lines ()
  "Byte-compile warnings carry file, line and severity."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo" "(defun demo-fn () (demo-undefined-fn))")
    (let* ((builder-elisp-check-use-cache nil)
           (res (builder-elisp-check-package tmp :tools '(compile)))
           (finding (car (builder-tools-test--tools res "compile"))))
      (should finding)
      (should (string-match-p "demo-undefined-fn" (plist-get finding :message)))
      (should (equal (plist-get finding :severity) "warning"))
      (should (> (plist-get finding :line) 0))
      (should (equal (plist-get res :status) "ok")))))

(ert-deftest builder-test/check-cache-reuses-and-invalidates ()
  "Unchanged files are answered from the cache; edits invalidate it."
  (builder-tools-test--with-dir tmp
    (let ((file (builder-tools-test--package tmp "demo"))
          (builder-elisp-check-use-cache t)
          (builder-elisp-check--cache (make-hash-table :test #'equal)))
      (should (= (plist-get (builder-elisp-check-package tmp :tools '(parens checkdoc)) :cached) 0))
      (should (= (plist-get (builder-elisp-check-package tmp :tools '(parens checkdoc)) :cached) 2))
      (with-temp-file file
        (insert ";;; demo.el --- Demo -*- lexical-binding: t; -*-\n(defun demo-fn () (+ 1\n"))
      (let ((res (builder-elisp-check-package tmp :tools '(parens checkdoc))))
        (should (= (plist-get res :cached) 0))
        (should (equal (plist-get res :status) "failed"))))))

(ert-deftest builder-test/check-cache-does-not-alias-results ()
  "Cached findings survive callers that modify the returned vectors."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo" "(defun demo-fn () (demo-undefined-fn))")
    (let ((builder-elisp-check-use-cache t)
          (builder-elisp-check--cache (make-hash-table :test #'equal)))
      (let ((first (builder-elisp-check-package tmp :tools '(compile))))
        (aset (plist-get first :findings) 0 nil))
      (should (builder-tools-test--tools
               (builder-elisp-check-package tmp :tools '(compile)) "compile")))))

(ert-deftest builder-test/check-skips-unavailable-tools ()
  "Tools that cannot run are listed under :skipped, not failed."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo")
    (cl-letf (((symbol-function 'builder-elisp-check--unavailable)
               (lambda (tool) (when (eq tool 'relint) "relint not installed")))
              (builder-elisp-check-use-cache nil))
      (let ((res (builder-elisp-check-package tmp :tools '(parens relint))))
        (should (equal (plist-get res :status) "ok"))
        (should (equal (append (plist-get res :skipped) nil)
                       '("relint: relint not installed")))))))

(ert-deftest builder-test/check-honors-files-and-skips-tests ()
  "Discovery leaves out test files; :files overrides discovery."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo")
    (builder-tools-test--write (expand-file-name "test-demo.el" tmp) "(this is (broken")
    (let ((builder-elisp-check-use-cache nil))
      (should (equal (plist-get (builder-elisp-check-package tmp :tools '(parens)) :status) "ok"))
      (should (equal (plist-get (builder-elisp-check-package
                                 tmp :tools '(parens) :files '("test-demo.el"))
                                :status)
                     "failed")))))

(ert-deftest builder-test/static-findings-shared-by-lint-paths ()
  "`builder-elisp-static-findings' reports long docstring lines per file."
  (builder-tools-test--with-dir tmp
    (let ((file (builder-tools-test--package
                 tmp "demo"
                 (format "(defun demo-fn ()\n  %S\n  1)" (make-string 100 ?x)))))
      (let ((findings (builder-elisp-static-findings tmp (list file))))
        (should (= (length findings) 1))
        (should (equal (plist-get (car findings) :file) file))
        (should (string-match-p "exceeds" (builder-elisp--finding-text (car findings))))))))

;;;; builder-scaffold

(ert-deftest builder-test/scaffold-ci-workflow-toggles-steps ()
  "The workflow includes only the requested steps and dependencies."
  (let ((full (builder-scaffold-ci-workflow :dependencies '(dash "f"))))
    (should (string-match-p "dependencies: \"dash f\"" full))
    (should (string-match-p "/lint@main" full))
    (should (string-match-p "/coverage@main" full))
    (should (string-match-p "codecov-action" full))
    (should (string-match-p "\"29.4\"" full)))
  (let ((bare (builder-scaffold-ci-workflow :with-lint nil :with-coverage nil)))
    (should-not (string-match-p "lint" bare))
    (should-not (string-match-p "coverage" bare))
    (should-not (string-match-p "dependencies" bare)))
  (should-not (string-match-p "codecov-action"
                              (builder-scaffold-ci-workflow :with-codecov nil))))

(ert-deftest builder-test/scaffold-ci-workflow-uses-customization ()
  "Action repository, ref and Emacs versions are customizable."
  (let ((builder-scaffold-ci-action "me/ci")
        (builder-scaffold-ci-action-ref "v1")
        (builder-scaffold-ci-emacs-versions '("31.1")))
    (let ((text (builder-scaffold-ci-workflow)))
      (should (string-match-p "uses: me/ci/lint@v1" text))
      (should (string-match-p "uses: me/ci@v1" text))
      (should (string-match-p "\"31.1\"" text))
      (should-not (string-match-p "29.4" text)))))

(ert-deftest builder-test/scaffold-package-dry-run-writes-nothing ()
  "Dry run reports the files it would create and writes none."
  (builder-tools-test--with-dir tmp
    (let* ((dir (expand-file-name "demo" tmp))
           (res (builder-scaffold-package "demo" :directory dir :dry-run t)))
      (should (eq (plist-get res :dry_run) t))
      (should (= (length (plist-get res :created)) 4))
      (should-not (file-exists-p dir)))))

(ert-deftest builder-test/scaffold-package-writes-and-never-overwrites ()
  "A real run writes the four files; a second run leaves them untouched."
  (builder-tools-test--with-dir tmp
    (let* ((dir (expand-file-name "demo" tmp))
           (builder-scaffold-author "Ada Lovelace")
           (builder-scaffold-email "ada@example.org")
           (builder-scaffold-github-owner "ada")
           (res (builder-scaffold-package "demo" :directory dir :summary "A demo"
                                          :dependencies '(dash))))
      (should (= (length (plist-get res :created)) 4))
      (should (file-exists-p (expand-file-name ".gitignore" dir)))
      (should (file-exists-p (expand-file-name "test/test-demo.el" dir)))
      (let ((main (builder-scaffold--read-file (expand-file-name "demo.el" dir))))
        (should (string-match-p "--- A demo" main))
        (should (string-match-p "Author: Ada Lovelace <ada@example.org>" main))
        (should (string-match-p "URL: https://github.com/ada/demo" main)))
      (should (string-match-p "dependencies: \"dash\""
                              (builder-scaffold--read-file
                               (expand-file-name ".github/workflows/ci.yml" dir))))
      (builder-tools-test--write (expand-file-name "demo.el" dir) "mine")
      (let ((again (builder-scaffold-package "demo" :directory dir)))
        (should (= (length (plist-get again :created)) 0))
        (should (= (length (plist-get again :existing)) 4))
        (should (equal (builder-scaffold--read-file (expand-file-name "demo.el" dir)) "mine"))))))

(ert-deftest builder-test/scaffold-package-the-starter-package-passes-the-check ()
  "The scaffolded package compiles and lints clean."
  (builder-tools-test--with-dir tmp
    (let* ((dir (expand-file-name "demo" tmp))
           (builder-scaffold-author "Ada")
           (builder-scaffold-email "ada@example.org"))
      (builder-scaffold-package "demo" :directory dir)
      (let ((builder-elisp-check-use-cache nil))
        (should (equal (plist-get (builder-elisp-check-package
                                   dir :tools '(parens compile checkdoc))
                                  :status)
                       "ok"))))))

(ert-deftest builder-test/scaffold-ci-infers-dependencies-and-guards-overwrite ()
  "CI setup reads Package-Requires, skips built-ins and respects overwrite."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--write
     (expand-file-name "demo.el" tmp)
     ";;; demo.el --- Demo -*- lexical-binding: t; -*-\n;; Package-Requires: ((emacs \"29.1\") (seq \"2.0\") (dash \"2.19\")\n;;   (f \"0.20\"))\n")
    (let ((dry (builder-scaffold-ci tmp :dry-run t)))
      (should (equal (plist-get dry :status) "dry-run"))
      (should (equal (append (plist-get dry :dependencies) nil) '("dash" "f")))
      (should-not (file-exists-p (plist-get dry :path))))
    (should (equal (plist-get (builder-scaffold-ci tmp) :status) "written"))
    (should (equal (plist-get (builder-scaffold-ci tmp) :status) "exists"))
    (let ((res (builder-scaffold-ci tmp :overwrite t :with-lint nil)))
      (should (equal (plist-get res :status) "written"))
      (should (stringp (plist-get res :existing))))))

;;;; builder-docs

(ert-deftest builder-test/docs-validate-finds-broken-links ()
  "Broken files, anchors, headings and line numbers are reported with lines."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--write (expand-file-name "docs/api.org" tmp)
                               "* Heading\n:PROPERTIES:\n:CUSTOM_ID: known\n:END:\ntext\n")
    (builder-tools-test--write
     (expand-file-name "README.org" tmp)
     (concat "* Intro\n"
             "ok [[file:docs/api.org::#known][known]]\n"
             "bad id [[file:docs/api.org::#missing][x]]\n"
             "bad file [[file:docs/none.org][y]]\n"
             "bad line [[file:docs/api.org::99][z]]\n"
             "bad heading [[file:docs/api.org::*Nope][h]]\n"
             "ok heading [[file:docs/api.org::*Heading]]\n"
             "ok anchor [[#intro]]\n"
             "bad anchor [[#nowhere]]\n"
             "web [[https://example.org][site]]\n"))
    (let* ((res (builder-docs-validate tmp))
           (errors (append (plist-get res :errors) nil)))
      (should (equal (plist-get res :status) "failed"))
      (should (= (plist-get res :count) 5))
      (should (equal (mapcar (lambda (e) (plist-get e :line)) errors) '(3 4 5 6 9)))
      (should (string-match-p "'#missing' not found" (plist-get (nth 0 errors) :message)))
      (should (string-match-p "does not exist" (plist-get (nth 1 errors) :message)))
      (should (string-match-p "out of range" (plist-get (nth 2 errors) :message))))))

(ert-deftest builder-test/docs-validate-clean-directory-is-ok ()
  "Valid links produce an ok result."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--write (expand-file-name "README.org" tmp) "* A\n[[#a]]\n")
    (let ((res (builder-docs-validate tmp)))
      (should (equal (plist-get res :status) "ok"))
      (should (= (plist-get res :count) 0))
      (should (equal (plist-get res :errors) [])))))

(ert-deftest builder-test/docs-generate-api-dry-run-reports-action ()
  "Dry run picks scaffold or regenerate without needing org-docsgen."
  (builder-tools-test--with-dir tmp
    (should (equal (plist-get (builder-docs-generate-api tmp t) :action) "scaffold"))
    (builder-tools-test--write (expand-file-name "docs/api.org" tmp) "")
    (should (equal (plist-get (builder-docs-generate-api tmp t) :action) "regenerate"))))

(ert-deftest builder-test/docs-api-source-lists-package-files ()
  "The scaffolded api.org covers package files and not tests."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo")
    (builder-tools-test--write (expand-file-name "test-demo.el" tmp) "")
    (should (equal (builder-docs--package-files tmp) '("demo.el")))
    (let ((text (builder-docs--api-source "demo" '("demo.el"))))
      (should (string-match-p "#\\+TITLE: demo API Reference" text))
      (should (string-match-p "\"demo.el\"" text))
      (should-not (string-match-p "test-demo" text)))))

;;;; MCP tools

(ert-deftest builder-test/mcp-elisp-preflight-tool ()
  "`elisp_preflight' returns structured findings, honoring cache=false."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--package tmp "demo" "(defun demo-fn () (demo-undefined-fn))")
    (let ((res (builder-mcp-call-tool
                'elisp_preflight
                (list :directory tmp :tools ["compile"] :cache :json-false))))
      (should (equal (plist-get res :status) "ok"))
      (should (= (plist-get res :cached) 0))
      (should (= (plist-get res :warnings) 1)))))

(ert-deftest builder-test/mcp-scaffold-tools-default-to-dry-run ()
  "`elisp_package_spinout' and `elisp_ci_setup' write only with dry_run=false."
  (builder-tools-test--with-dir tmp
    (let ((dir (expand-file-name "demo" tmp)))
      (should (eq (plist-get (builder-mcp-call-tool
                              'elisp_package_spinout (list :package "demo" :target_dir dir))
                             :dry_run)
                  t))
      (should-not (file-exists-p dir))
      (builder-mcp-call-tool 'elisp_package_spinout
                             (list :package "demo" :target_dir dir :dry_run :json-false))
      (should (file-exists-p (expand-file-name "demo.el" dir)))
      (should (equal (plist-get (builder-mcp-call-tool 'elisp_ci_setup (list :directory dir))
                                :status)
                     "dry-run"))
      (should (equal (plist-get (builder-mcp-call-tool
                                 'elisp_ci_setup (list :directory dir :dry_run :json-false))
                                :status)
                     "exists"))
      (let ((res (builder-mcp-call-tool
                  'elisp_ci_setup (list :directory dir :overwrite t :with_lint :json-false
                                        :dry_run :json-false))))
        (should (equal (plist-get res :status) "written"))
        (should-not (string-match-p "lint" (plist-get res :workflow)))))))

(ert-deftest builder-test/mcp-doc-tools ()
  "`doc_validate' and `doc_generate_api' (dry run) are wired up."
  (builder-tools-test--with-dir tmp
    (builder-tools-test--write (expand-file-name "README.org" tmp) "[[file:gone.org]]\n")
    (should (= (plist-get (builder-mcp-call-tool 'doc_validate (list :directory tmp)) :count) 1))
    (should (equal (plist-get (builder-mcp-call-tool 'doc_generate_api
                                                     (list :directory tmp :dry_run t))
                              :action)
                   "scaffold"))
    (should-error (builder-mcp-call-tool 'doc_validate nil))))

(ert-deftest builder-test/mcp-package-build-clean-action ()
  "`builder_package_build' with action=clean removes artifacts and rejects bad actions."
  (builder-tools-test--with-dir base
    (let ((tmp (expand-file-name "demo/" base)))
    (builder-tools-test--package tmp "demo")
    (builder-tools-test--write (expand-file-name "build/packages/x.tar" tmp) "x")
    (builder-tools-test--write (expand-file-name "demo.elc" tmp) "x")
    (let ((res (builder-mcp-call-tool 'builder_package_build
                                      (list :directory tmp :action "clean"))))
      (should (equal (plist-get res :status) "ok"))
      (should-not (file-exists-p (expand-file-name "build" tmp)))
      (should-not (file-exists-p (expand-file-name "demo.elc" tmp))))
    (should-error (builder-mcp-call-tool 'builder_package_build
                                         (list :directory tmp :action "nope"))))))

(ert-deftest builder-test/mcp-run-tests-reports-failures-from-subprocess ()
  "`builder_run_tests' with a root runs the package tests in a subprocess."
  (builder-tools-test--with-dir base
    (let ((tmp (expand-file-name "demo/" base)))
    (builder-tools-test--package tmp "demo")
    (builder-tools-test--write
     (expand-file-name "test/test-demo.el" tmp)
     (concat "(require 'ert)\n(require 'demo)\n"
             "(ert-deftest demo-test/passes () (should (= (demo-fn) 1)))\n"
             "(ert-deftest demo-test/fails () (should (= (demo-fn) 2)))\n"
             "(ert-deftest other-test/passes () (should t))\n"))
    (let ((res (builder-mcp-call-tool 'builder_run_tests (list :root tmp))))
      (should (equal (plist-get res :status) "failed"))
      (should (eq (plist-get res :success) :json-false))
      (should (= (plist-get res :total) 3))
      (should (= (plist-get res :passed) 2))
      (should (= (plist-get res :failed) 1))
      (should (equal (plist-get (aref (plist-get res :failing) 0) :name) "demo-test/fails")))
    (let ((res (builder-mcp-call-tool 'builder_run_tests
                                      (list :directory tmp :prefix "demo-test/passes"))))
      (should (equal (plist-get res :status) "ok"))
      (should (eq (plist-get res :success) t))
      (should (= (plist-get res :total) 1)))
    (let ((res (builder-mcp-call-tool 'builder_run_tests
                                      (list :root tmp :glob "other-*"))))
      (should (= (plist-get res :total) 1)))
    (should (equal (plist-get (builder-mcp-call-tool 'builder_run_tests
                                                     (list :root tmp :selector "nothing-matches"))
                              :status)
                   "failed")))))

(ert-deftest builder-startup-profile-report-test ()
  "The report lists summary lines and the slowest ops, slowest first."
  (require 'builder-isolate-elisp)
  (let ((report (builder-emacs-conf--startup-profile-report
                 (string-join '("[op]: <sprite> [startup] fast: 0.001000s"
                                "[op]: <sprite> [startup] slow: 0.075000s"
                                "[emacs]: <probe> init time 0.03"
                                "unrelated message")
                              "\n"))))
    (should (string-match-p "init time 0.03" report))
    (should (< (string-match "slow:\\|\\[startup\\] slow" report)
               (string-match "\\[startup\\] fast" report)))
    (should-not (string-match-p "unrelated" report))))

(ert-deftest builder-feature-file-table-test ()
  "Each entry maps a loaded feature's name to its providing file."
  (require 'builder-isolate-elisp)
  (let ((table (builder-emacs-conf--feature-file-table)))
    (should (stringp (map-elt table "builder-core")))
    (should (string-match-p "builder-core" (map-elt table "builder-core")))
    (should (equal (seq-map #'car table) (sort (seq-map #'car table) #'string<)))))

(provide 'test-builder-elisp-tools)
;;; test-builder-elisp-tools.el ends here
