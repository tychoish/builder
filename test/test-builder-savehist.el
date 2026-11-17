;;; test-builder-savehist.el --- Builder history tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for compilation history registration.
;;; Code:
(require 'ert)
(require 'builder-mode)
(ert-deftest builder/mode-registers-compile-history ()
  (let ((savehist-additional-variables nil))
    (unwind-protect
        (progn (builder-mode 1) (should (memq 'compile-history savehist-additional-variables)))
      (builder-mode -1))))
(provide 'test-builder-savehist)
;;; test-builder-savehist.el ends here
