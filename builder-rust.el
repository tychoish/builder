;;; builder-rust.el --- Rust project management, test, and compilation -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (f "0.20.0") (xtdlib "0.1.0"))
;; Keywords: tools, rust, cargo

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tools for developing, testing, checking, and compiling Rust projects and
;; workspaces with Cargo.  Integrates with `builder' compilation buffers.

;;; Code:

(require 'compile)
(require 'seq)
(require 'subr-x)
(require 'f)
(require 'xtd-f)
(require 'xtd-project)
(require 'builder-core)

(defgroup builder-rust nil
  "Rust and Cargo development tools for builder."
  :group 'builder
  :prefix "builder-rust-")

(defcustom builder-rust-cargo-binary "cargo"
  "Path to the cargo executable."
  :type 'string
  :group 'builder-rust)

;;; Project and workspace discovery

(defun builder-rust-root (&optional dir)
  "Find the directory containing the root Cargo.toml for DIR.
Defaults to `default-directory'."
  (let ((search-dir (or dir default-directory)))
    (locate-dominating-file search-dir "Cargo.toml")))

(defun builder-rust-workspace-p (&optional dir)
  "Return non-nil if the Rust project for DIR is a Cargo workspace."
  (when-let* ((root (builder-rust-root dir))
              (manifest (f-join root "Cargo.toml")))
    (when (f-exists-p manifest)
      (with-temp-buffer
        (insert-file-contents manifest)
        (goto-char (point-min))
        (re-search-forward "^\\[workspace\\]" nil t)))))

(defun builder-rust-packages (&optional dir)
  "Return a list of package names in the Rust workspace or project for DIR."
  (when-let* ((root (builder-rust-root dir)))
    (let (packages)
      (dolist (cargo-file (directory-files-recursively root "\\`Cargo\\.toml\\'"))
        (with-temp-buffer
          (insert-file-contents cargo-file)
          (goto-char (point-min))
          (when (re-search-forward "^\\[package\\]" nil t)
            (when (re-search-forward "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" nil t)
              (push (match-string 1) packages)))))
      (nreverse (delete-dups packages)))))

(defun builder-rust-binaries (&optional dir)
  "Return a list of binary names in the Rust project for DIR."
  (when-let* ((root (builder-rust-root dir)))
    (let (binaries)
      ;; 1. Check [[bin]] in Cargo.toml files
      (dolist (cargo-file (directory-files-recursively root "\\`Cargo\\.toml\\'"))
        (with-temp-buffer
          (insert-file-contents cargo-file)
          (goto-char (point-min))
          (while (re-search-forward "^\\[\\[bin\\]\\]" nil t)
            (when (re-search-forward "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" nil t)
              (push (match-string 1) binaries)))))
      ;; 2. Check src/bin/*.rs and src/main.rs
      (dolist (rs-file (directory-files-recursively root "\\`main\\.rs\\'"))
        (let* ((src-dir (f-parent rs-file))
               (crate-dir (f-parent src-dir))
               (manifest (f-join crate-dir "Cargo.toml")))
          (when (f-exists-p manifest)
            (with-temp-buffer
              (insert-file-contents manifest)
              (goto-char (point-min))
              (when (re-search-forward "^name[ \t]*=[ \t]*\"\\([^\"]+\\)\"" nil t)
                (push (match-string 1) binaries))))))
      (nreverse (delete-dups binaries)))))

;;; Execution and compilation commands

(defun builder-rust--run (command &optional name dir)
  "Run Cargo COMMAND via `builder-compile-project' in DIR."
  (let ((default-directory (or dir (builder-rust-root) default-directory)))
    (builder-compile-project (or name "cargo") command)))

;;;###autoload
(defun builder-rust-build (&optional release package)
  "Run cargo build for the current project or workspace.
When RELEASE is non-nil, pass --release.  When PACKAGE is non-nil,
build that package specifically."
  (interactive "P")
  (let* ((root (builder-rust-root))
         (is-ws (builder-rust-workspace-p root))
         (target (cond (package (format "-p %s" package))
                       (is-ws "--workspace")
                       (t "")))
         (flags (string-join (seq-filter (lambda (s) (not (string-empty-p s)))
                                         (list target (when release "--release")))
                             " "))
         (cmd (string-trim (format "%s build %s" builder-rust-cargo-binary flags))))
    (builder-rust--run cmd "cargo-build" root)))

;;;###autoload
(defun builder-rust-test (&optional package test-filter)
  "Run cargo test for the current project or workspace.
When PACKAGE is non-nil, test that package.  When TEST-FILTER is
non-nil, only run matching tests."
  (interactive)
  (let* ((root (builder-rust-root))
         (is-ws (builder-rust-workspace-p root))
         (pkg-flag (cond (package (format "-p %s" package))
                         (is-ws "--workspace")
                         (t "")))
         (filter-flag (if test-filter (format "-- %s" test-filter) ""))
         (cmd (string-trim (format "%s test %s %s"
                                   builder-rust-cargo-binary
                                   pkg-flag
                                   filter-flag))))
    (builder-rust--run cmd "cargo-test" root)))

;;;###autoload
(defun builder-rust-check (&optional package)
  "Run cargo check for the current project or workspace."
  (interactive)
  (let* ((root (builder-rust-root))
         (is-ws (builder-rust-workspace-p root))
         (target (cond (package (format "-p %s" package))
                       (is-ws "--workspace")
                       (t "")))
         (cmd (string-trim (format "%s check %s" builder-rust-cargo-binary target))))
    (builder-rust--run cmd "cargo-check" root)))

;;;###autoload
(defun builder-rust-clippy (&optional fix package)
  "Run cargo clippy for the current project or workspace.
When FIX is non-nil, pass --fix --allow-dirty."
  (interactive "P")
  (let* ((root (builder-rust-root))
         (is-ws (builder-rust-workspace-p root))
         (target (cond (package (format "-p %s" package))
                       (is-ws "--workspace")
                       (t "")))
         (fix-flag (if fix "--fix --allow-dirty" ""))
         (cmd (string-trim (format "%s clippy %s %s"
                                   builder-rust-cargo-binary
                                   target
                                   fix-flag))))
    (builder-rust--run cmd "cargo-clippy" root)))

;;;###autoload
(defun builder-rust-run (&optional binary args)
  "Run cargo run for BINARY with optional ARGS."
  (interactive)
  (let* ((root (builder-rust-root))
         (bins (builder-rust-binaries root))
         (choice (or binary
                     (if (and bins (> (length bins) 1))
                         (completing-read "Select binary: " bins nil t)
                       (car bins))))
         (bin-flag (if choice (format "--bin %s" choice) ""))
         (args-flag (if (and args (not (string-empty-p args))) (format "-- %s" args) ""))
         (cmd (string-trim (format "%s run %s %s"
                                   builder-rust-cargo-binary
                                   bin-flag
                                   args-flag))))
    (builder-rust--run cmd (if choice (format "cargo-run-%s" choice) "cargo-run") root)))

;;; Batch runner for automation / CI scripts

(defun builder-rust-batch (action &optional dir)
  "Run ACTION (:build, :test, :check, :clippy) synchronously in DIR.
Returns 0 on success or non-zero exit code on failure, streaming output."
  (let* ((root (or dir (builder-rust-root) default-directory))
         (default-directory root)
         (is-ws (builder-rust-workspace-p root))
         (ws-flag (if is-ws " --workspace" ""))
         (cmd (pcase action
                (:build (format "%s build%s" builder-rust-cargo-binary ws-flag))
                (:build-release (format "%s build%s --release" builder-rust-cargo-binary ws-flag))
                (:test (format "%s test%s" builder-rust-cargo-binary ws-flag))
                (:check (format "%s check%s" builder-rust-cargo-binary ws-flag))
                (:clippy (format "%s clippy%s" builder-rust-cargo-binary ws-flag))
                (_ (error "Unknown Rust action: %s" action)))))
    (message "builder-rust: executing '%s' in %s" cmd root)
    (call-process-shell-command cmd nil t t)))

(provide 'builder-rust)
;;; builder-rust.el ends here
