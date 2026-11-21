;;; test-builder-elpaish.el --- Tests for builder-elpaish -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; Covers the `elpaish' MCP service hosted by `builder-elpaish'.  Tests that
;; need ELPAish skip when it is not installed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'builder-elpaish)

(declare-function mcpkit-get-service "mcpkit")
(declare-function mcpkit-service-tools "mcpkit")
(declare-function mcpkit-tool-handler "mcpkit")

(defun builder-elpaish-test--call (tool args)
  "Call MCP TOOL on the `elpaish' service with ARGS and return its result."
  (let* ((service (mcpkit-get-service 'elpaish))
         (handler (mcpkit-tool-handler (map-elt (mcpkit-service-tools service) (symbol-name tool))))
         result)
    (funcall handler args (lambda (err res) (if err (error "%s" err) (setq result res))))
    result))

(ert-deftest builder-elpaish-test-tools-registered ()
  "The elpaish service exposes exactly the ELPAish-specific tools."
  (skip-unless builder-elpaish-service)
  (let ((names (sort (map-keys (mcpkit-service-tools (mcpkit-get-service 'elpaish))) #'string<)))
    (should (equal names '("elpaish_rebuild" "elpaish_recipe" "elpaish_status"
                           "elpaish_upgrade" "elpaish_workflow_dispatch")))))

(ert-deftest builder-elpaish-test-recipe-and-required-package ()
  "`elpaish_recipe' returns a snippet and rejects a missing package."
  (skip-unless builder-elpaish-service)
  (should (string-match-p "elpaish-register-package"
                          (plist-get (builder-elpaish-test--call 'elpaish_recipe '(:package "demo"))
                                     :snippet)))
  (should-error (builder-elpaish-test--call 'elpaish_recipe nil)))

(ert-deftest builder-elpaish-test-upgrade-defaults-to-all ()
  "`elpaish_upgrade' without packages upgrades everything upgradable."
  (skip-unless builder-elpaish-service)
  (let (seen)
    (cl-letf (((symbol-function 'elpaish-ops-upgrade)
               (lambda (&rest args) (setq seen args) (list :status "ok"))))
      (builder-elpaish-test--call 'elpaish_upgrade nil)
      (should (eq (plist-get seen :all) t))
      (builder-elpaish-test--call 'elpaish_upgrade '(:packages ["a"] :reinstall t))
      (should (equal (plist-get seen :packages) '("a")))
      (should-not (plist-get seen :all))
      (should (eq (plist-get seen :reinstall) t)))))

(ert-deftest builder-elpaish-test-missing-elpaish-is-a-clear-error ()
  "Tool handlers explain that ELPAish is missing instead of failing obscurely."
  (cl-letf (((symbol-function 'require)
             (lambda (feature &rest _) (unless (eq feature 'elpaish-ops) feature))))
    (should-error (builder-elpaish--require-ops) :type 'user-error)))

(provide 'test-builder-elpaish)
;;; test-builder-elpaish.el ends here
