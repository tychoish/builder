;;; test-builder-window.el --- Compilation window tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify compilation window reuse and managed display settings.

;;; Code:

(require 'ert)
(require 'builder-mode)

(defmacro builder-test--with-compilation-windows (&rest body)
  "Run BODY with isolated display settings and temporary buffers."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-alist nil)
         (display-buffer-base-action nil)
         (display-buffer-overriding-action nil)
         (builder-mode--saved-settings nil)
         (original (generate-new-buffer " *builder-old*"))
         (output (generate-new-buffer " *builder-new*")))
     (unwind-protect
         (save-window-excursion
           (delete-other-windows)
           (with-current-buffer original (compilation-mode))
           (with-current-buffer output (compilation-mode))
           (set-window-buffer (selected-window) original)
           (builder-mode--apply-settings)
           ,@body)
       (builder-mode--revert-settings)
       (kill-buffer original)
       (kill-buffer output))))

(ert-deftest builder-window/reuse-current-compilation-window ()
  "New compilation output replaces the selected compilation buffer."
  (builder-test--with-compilation-windows
    (let ((window (selected-window)))
      (should (eq (with-current-buffer output (display-buffer output)) window))
      (should (eq (window-buffer window) output))
      (should (= (length (window-list)) 1)))))

(ert-deftest builder-window/preserve-source-window ()
  "Compilation output does not replace a selected source buffer."
  (builder-test--with-compilation-windows
    (with-current-buffer original (emacs-lisp-mode))
    (let ((window (selected-window)))
      (should-not (eq (display-buffer output) window))
      (should (eq (window-buffer window) original)))))

(ert-deftest builder-window/preserve-non-compilation-output ()
  "Other output does not replace the selected compilation buffer."
  (builder-test--with-compilation-windows
    (with-current-buffer output (fundamental-mode))
    (let ((window (selected-window)))
      (should-not (eq (display-buffer output) window))
      (should (eq (window-buffer window) original)))))

(ert-deftest builder-window/managed-display-settings ()
  "Keep existing display rules and restore them after disabling Builder."
  (let* ((prior '(("existing" (display-buffer-same-window))))
         (display-buffer-alist (copy-tree prior))
         (builder-mode--saved-settings nil))
    (unwind-protect
        (progn
          (builder-mode--apply-settings)
          (should (equal (cdr display-buffer-alist) prior))
          (builder-mode--revert-settings)
          (should (equal display-buffer-alist prior)))
      (when builder-mode--saved-settings
        (builder-mode--revert-settings)))))

(ert-deftest builder-window/disable-reuse-setting ()
  "Removing the managed display setting restores normal window selection."
  (let ((builder-mode-managed-settings
         (assq-delete-all 'display-buffer-alist
                          (copy-tree builder-mode-managed-settings))))
    (builder-test--with-compilation-windows
      (should-not (eq (display-buffer output) (selected-window))))))

(provide 'test-builder-window)
;;; test-builder-window.el ends here
