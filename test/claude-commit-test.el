;;; claude-commit-test.el --- Tests for claude-commit.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for claude-commit.el

;;; Code:

(require 'ert)
(require 'claude-commit)

;;; Test Helpers

(defmacro with-temp-git-repo (&rest body)
  "Execute BODY in a temporary git repository."
  `(let ((temp-dir (make-temp-file "claude-commit-test" t)))
     (unwind-protect
         (let ((default-directory temp-dir))
           (shell-command "git init")
           (shell-command "git config user.email 'test@example.com'")
           (shell-command "git config user.name 'Test User'")
           ,@body)
       (delete-directory temp-dir t))))

;;; Spinner Tests

(ert-deftest claude-commit-test-spinner-styles ()
  "Test that all spinner styles are defined."
  (dolist (style '(dots circle arrow stars blocks moon earth bounce))
    (should (assoc style claude-commit--spinner-frames-alist))))

(ert-deftest claude-commit-test-spinner-cycle ()
  "Test cycling through spinner styles."
  (let ((original-style claude-commit-spinner-style))
    (unwind-protect
        (progn
          (setq claude-commit-spinner-style 'dots)
          (claude-commit-cycle-spinner)
          (should (not (eq claude-commit-spinner-style 'dots))))
      (setq claude-commit-spinner-style original-style))))

(ert-deftest claude-commit-test-spinner-start-stop ()
  "Test starting and stopping the spinner."
  (claude-commit--start-spinner "Testing")
  (should claude-commit--spinner-timer)
  (claude-commit--stop-spinner)
  (should (null claude-commit--spinner-timer)))

;;; Utility Function Tests

(ert-deftest claude-commit-test-clean-message ()
  "Test commit message cleaning."
  (should (string= (claude-commit--clean-commit-message "\"test\"") "test"))
  (should (string= (claude-commit--clean-commit-message "'test'") "test"))
  (should (string= (claude-commit--clean-commit-message "`test`") "test"))
  (should (string= (claude-commit--clean-commit-message "**feat:** test") "test"))
  (should (string= (claude-commit--clean-commit-message "  test  ") "test")))

(ert-deftest claude-commit-test-truncate-diff ()
  "Test diff truncation."
  (let ((claude-commit-max-diff-size 10))
    (should (string= (claude-commit--truncate-diff "short") "short"))
    (should (string-match "truncated" (claude-commit--truncate-diff "this is a very long diff")))))

;;; Git Integration Tests

(ert-deftest claude-commit-test-git-info ()
  "Test getting git information."
  (with-temp-git-repo
   (write-region "test" nil "test.txt")
   (shell-command "git add test.txt")
   (let ((info (claude-commit--get-git-info)))
     (should (= (length info) 3))
     (should (stringp (nth 0 info)))  ; status
     (should (stringp (nth 1 info)))  ; staged diff
     (should (stringp (nth 2 info))))))  ; unstaged diff

;;; Configuration Tests

(ert-deftest claude-commit-test-customization ()
  "Test that customization variables are defined."
  (should (boundp 'claude-commit-model))
  (should (boundp 'claude-commit-prompt-template))
  (should (boundp 'claude-commit-auto-stage-all))
  (should (boundp 'claude-commit-spinner-style))
  (should (boundp 'claude-commit-max-diff-size))
  (should (boundp 'claude-commit-success-emoji)))

(ert-deftest claude-commit-test-model-options ()
  "Test model option validation."
  (let ((valid-models '("haiku" "sonnet" "opus")))
    (should (member claude-commit-model valid-models))))

;;; Mode Tests

(ert-deftest claude-commit-test-minor-mode ()
  "Test the minor mode."
  (claude-commit-mode 1)
  (should claude-commit-mode)
  (should (keymapp claude-commit-mode-map))
  (claude-commit-mode -1)
  (should (not claude-commit-mode)))

;;; Async Tests

(ert-deftest claude-commit-test-async-command ()
  "Test async command execution."
  (let ((result nil)
        (done nil))
    (claude-commit--async-shell-command
     "echo 'test output'"
     (lambda (output)
       (setq result output)
       (setq done t)))
    ;; Wait for async completion
    (while (not done)
      (sit-for 0.1))
    (should (string-match "test output" result))))

;;; Mock Claude CLI Tests

(ert-deftest claude-commit-test-process-result ()
  "Test processing Claude results."
  (let ((claude-commit--auto-commit-pending nil)
        (kill-ring nil))
    (claude-commit--process-result "feat: Add new feature")
    (should (string= (car kill-ring) "feat: Add new feature"))))

(ert-deftest claude-commit-test-process-empty-result ()
  "Test processing empty Claude results."
  (let ((message-called nil))
    (cl-letf (((symbol-function 'message)
               (lambda (&rest args)
                 (setq message-called t))))
      (claude-commit--process-result "")
      (should message-called))))

;;; Integration Tests (requires Claude CLI)

(ert-deftest claude-commit-test-claude-cli-available ()
  "Test if Claude CLI is available."
  :tags '(integration)
  (let ((claude-available (executable-find "claude")))
    (if claude-available
        (should claude-available)
      (message "Claude CLI not found - skipping integration tests"))))

;;; Performance Tests

(ert-deftest claude-commit-test-large-diff-performance ()
  "Test performance with large diffs."
  (let ((large-diff (make-string 10000 ?a))
        (start-time (current-time)))
    (claude-commit--truncate-diff large-diff)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 0.1)))))  ; Should complete in under 100ms

(provide 'claude-commit-test)
;;; claude-commit-test.el ends here