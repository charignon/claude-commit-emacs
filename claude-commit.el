;;; claude-commit.el --- AI-powered commit messages using Claude -*- lexical-binding: t; -*-

;; Author: Laurent Charignon <charignon@fastmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (magit "3.0.0"))
;; Keywords: git, commit, ai, claude, tools
;; URL: https://github.com/charignon/claude-commit-emacs

;;; Commentary:

;; claude-commit.el provides AI-powered commit message generation using
;; Anthropic's Claude via the Claude Code CLI.  It offers multiple entry points:
;;
;; 1. Generate commit messages on-demand
;; 2. Auto-commit with generated messages
;; 3. Insert generated messages into commit buffers
;; 4. Beautiful spinner animations during generation
;;
;; Features:
;; - Multiple spinner styles for visual feedback
;; - Configurable Claude models (haiku, sonnet, opus)
;; - Custom prompt refinement
;; - Magit integration with automatic refresh
;; - Async execution to prevent UI blocking
;;
;; Requirements:
;; - Claude Code CLI installed: npm install -g @anthropic-ai/claude
;; - Git repository
;; - Magit (recommended)
;;
;; Usage:
;;   M-x claude-commit-generate    ; Generate commit message
;;   M-x claude-commit-auto        ; Auto-commit with AI message
;;   M-x claude-commit-insert      ; Insert into commit buffer
;;   M-x claude-commit-config      ; Open configuration

;;; Code:

(require 'seq)
(require 'cl-lib)

;;; Customization

(defgroup claude-commit nil
  "AI-powered commit messages using Claude."
  :group 'tools
  :group 'vc
  :prefix "claude-commit-"
  :link '(url-link :tag "GitHub" "https://github.com/charignon/claude-commit-emacs"))

(defcustom claude-commit-model "haiku"
  "Claude model to use for commit message generation.
Available options: haiku, sonnet, opus."
  :type '(choice (const :tag "Haiku (fast, cost-effective)" "haiku")
                 (const :tag "Sonnet (balanced)" "sonnet")
                 (const :tag "Opus (most capable)" "opus"))
  :group 'claude-commit)

(defcustom claude-commit-prompt-template
  "Generate a concise commit message for these changes. Focus on what was changed and why. Use conventional commit format (type(scope): description). Return ONLY the commit message, nothing else."
  "Template prompt for Claude commit message generation.
This can be customized to change the style and format of generated messages."
  :type 'string
  :group 'claude-commit)

(defcustom claude-commit-auto-stage-all nil
  "When non-nil, auto-commit will stage all files.
When nil, only commit already tracked files."
  :type 'boolean
  :group 'claude-commit)

(defcustom claude-commit-spinner-style 'dots
  "Style of spinner animation to display during generation.
Available styles: dots, circle, arrow, stars, blocks, moon, earth, bounce."
  :type '(choice (const :tag "Dots (⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏)" dots)
                 (const :tag "Circle (◐◓◑◒)" circle)
                 (const :tag "Arrow (←↖↑↗→↘↓↙)" arrow)
                 (const :tag "Stars (✶✸✹✺)" stars)
                 (const :tag "Blocks (▉▊▋▌▍▎▏)" blocks)
                 (const :tag "Moon phases (🌑🌒🌓🌔🌕🌖🌗🌘)" moon)
                 (const :tag "Earth (🌍🌎🌏)" earth)
                 (const :tag "Bounce (⠁⠂⠄⡀⢀⠠⠐⠈)" bounce))
  :group 'claude-commit)

(defcustom claude-commit-max-diff-size 3000
  "Maximum size of diff to send to Claude.
Larger diffs will be truncated to prevent API limits."
  :type 'integer
  :group 'claude-commit)

(defcustom claude-commit-success-emoji "🎉"
  "Emoji to display on successful auto-commit."
  :type 'string
  :group 'claude-commit)

;;; Internal Variables

(defvar claude-commit--spinner-timer nil
  "Timer for the spinner animation.")

(defvar claude-commit--spinner-frames-alist
  '((dots . ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])
    (circle . ["◐" "◓" "◑" "◒"])
    (arrow . ["←" "↖" "↑" "↗" "→" "↘" "↓" "↙"])
    (stars . ["✶" "✸" "✹" "✺" "✹" "✸"])
    (blocks . ["▉" "▊" "▋" "▌" "▍" "▎" "▏" "▎" "▍" "▌" "▋" "▊" "▉"])
    (moon . ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])
    (earth . ["🌍" "🌎" "🌏"])
    (bounce . ["⠁" "⠂" "⠄" "⡀" "⢀" "⠠" "⠐" "⠈"]))
  "Available spinner animation styles.")

(defvar claude-commit--spinner-current-frame 0
  "Current frame index for spinner animation.")

(defvar claude-commit--spinner-message ""
  "Message to display with the spinner.")

(defvar claude-commit--auto-commit-pending nil
  "Flag to indicate if we should auto-commit after getting Claude response.")

;;; Spinner Functions

(defun claude-commit--start-spinner (message)
  "Start a spinner animation with MESSAGE."
  (claude-commit--stop-spinner)
  (setq claude-commit--spinner-message message)
  (setq claude-commit--spinner-current-frame 0)
  (let ((frames (or (cdr (assoc claude-commit-spinner-style claude-commit--spinner-frames-alist))
                    (cdr (assoc 'dots claude-commit--spinner-frames-alist)))))
    (setq claude-commit--spinner-timer
          (run-with-timer 0 0.1
                          (lambda (frames)
                            (message "%s %s..."
                                     (aref frames claude-commit--spinner-current-frame)
                                     claude-commit--spinner-message)
                            (setq claude-commit--spinner-current-frame
                                  (% (1+ claude-commit--spinner-current-frame)
                                     (length frames))))
                          frames))))

(defun claude-commit--stop-spinner ()
  "Stop the spinner animation."
  (when claude-commit--spinner-timer
    (cancel-timer claude-commit--spinner-timer)
    (setq claude-commit--spinner-timer nil)))

;;; Utility Functions

(defun claude-commit--async-shell-command (command callback)
  "Execute COMMAND asynchronously and call CALLBACK with the result."
  (let ((output-buffer (generate-new-buffer " *claude-commit-async*")))
    (set-process-sentinel
     (start-process "claude-commit" output-buffer shell-file-name shell-command-switch command)
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer (process-buffer process)
           (let ((output (string-trim (buffer-string))))
             (kill-buffer)
             (funcall callback output))))))))

(defun claude-commit--get-git-info ()
  "Get git status and diff information."
  (let* ((default-directory (or (vc-root-dir) default-directory))
         (status-output (shell-command-to-string "git status --porcelain"))
         (diff-output (shell-command-to-string "git diff --cached"))
         (unstaged-diff (shell-command-to-string "git diff")))
    (list status-output diff-output unstaged-diff)))

(defun claude-commit--truncate-diff (diff)
  "Truncate DIFF if it exceeds `claude-commit-max-diff-size'."
  (if (> (length diff) claude-commit-max-diff-size)
      (concat (substring diff 0 claude-commit-max-diff-size) "\n... (truncated)")
    diff))

(defun claude-commit--clean-commit-message (msg)
  "Clean up commit message MSG by removing quotes and formatting."
  (setq msg (string-trim msg))
  ;; Remove ANSI escape sequences and terminal control codes
  (setq msg (replace-regexp-in-string "\033\\[[0-9;]*[a-zA-Z]" "" msg))
  (setq msg (replace-regexp-in-string "\\[\\?[0-9]+[a-z]" "" msg))
  ;; Remove quotes and formatting
  (setq msg (replace-regexp-in-string "^[\"'`]" "" msg))
  (setq msg (replace-regexp-in-string "[\"'`]$" "" msg))
  (setq msg (replace-regexp-in-string "^\\*\\*.*:\\*\\* " "" msg))
  msg)

(defun claude-commit--refresh-magit ()
  "Refresh magit status buffers if magit is available."
  (require 'magit nil t)
  (when (featurep 'magit)
    (if (fboundp 'magit-refresh-all)
        (magit-refresh-all)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (eq major-mode 'magit-status-mode)
                     (get-buffer-window buf 'visible))
            (magit-refresh)))))))

;;; Core Functions

(defun claude-commit--process-result (result)
  "Process the result from Claude CLI."
  (claude-commit--stop-spinner)

  (if (and result (not (string-empty-p result)))
      (let ((commit-msg (claude-commit--clean-commit-message result)))
        ;; Put in kill ring for easy access
        (kill-new commit-msg)

        ;; Handle auto-commit or manual
        (if claude-commit--auto-commit-pending
            (progn
              (setq claude-commit--auto-commit-pending nil)
              ;; Commit using call-process to avoid output interference
              (let ((exit-code (call-process-shell-command
                               (format "git commit -m %s" (shell-quote-argument commit-msg))
                               nil nil nil)))
                ;; Refresh magit buffers
                (claude-commit--refresh-magit)
                ;; Show success message with delay to ensure visibility
                (run-at-time 0.1 nil
                             (lambda (msg emoji)
                               (message "%s Auto-committed: %s" emoji msg))
                             (if (> (length commit-msg) 50)
                                 (concat (substring commit-msg 0 47) "...")
                               commit-msg)
                             claude-commit-success-emoji)))
          (message "✓ Commit message: %s" commit-msg)))
    (message "✗ Failed to generate commit message")))

(defun claude-commit--generate-async (status-output diff-output unstaged-diff)
  "Generate commit message asynchronously using Claude CLI."
  (let* ((temp-file (make-temp-file "claude-commit-" nil ".txt")))
    ;; Write git info to temp file
    (with-temp-file temp-file
      (insert "Git Status:\n" status-output "\n\n"
              "Staged Changes:\n"
              (if (string-empty-p diff-output)
                  "No staged changes"
                (claude-commit--truncate-diff diff-output))
              "\n\nUnstaged Changes (for context):\n"
              (if (string-empty-p unstaged-diff)
                  "No unstaged changes"
                (claude-commit--truncate-diff unstaged-diff))))

    ;; Call Claude Code CLI asynchronously
    (let ((command (format "cat %s | claude --model %s -p %s"
                          (shell-quote-argument temp-file)
                          claude-commit-model
                          (shell-quote-argument claude-commit-prompt-template))))
      (claude-commit--async-shell-command command
                                         (lambda (result)
                                           (delete-file temp-file)
                                           (claude-commit--process-result result))))))

;;; Public API

;;;###autoload
(defun claude-commit-generate ()
  "Generate a commit message using Claude based on current git changes."
  (interactive)
  (let* ((default-directory (or (vc-root-dir) default-directory))
         (git-info (claude-commit--get-git-info))
         (status-output (nth 0 git-info))
         (diff-output (nth 1 git-info))
         (unstaged-diff (nth 2 git-info)))

    (if (and (string-empty-p status-output)
             (string-empty-p diff-output)
             (string-empty-p unstaged-diff))
        (message "No git changes found to generate commit message for")

      ;; Start spinner and generate
      (claude-commit--start-spinner "Generating commit message with Claude")
      (claude-commit--generate-async status-output diff-output unstaged-diff))))

;;;###autoload
(defun claude-commit-auto ()
  "Stage changes and auto-commit with Claude-generated message."
  (interactive)
  (let* ((default-directory (or (vc-root-dir) default-directory))
         (status-output (shell-command-to-string "git status --porcelain")))

    (if (string-empty-p status-output)
        (message "No changes to commit")

      ;; Stage changes based on configuration
      (if claude-commit-auto-stage-all
          (progn
            (shell-command "git add -A")
            (shell-command "git reset flycheck_*"))  ; Exclude temp files
        (shell-command "git add -u"))  ; Only tracked files

      ;; Set flag for auto-commit and generate message
      (setq claude-commit--auto-commit-pending t)
      (claude-commit-generate))))

;;;###autoload
(defun claude-commit-insert ()
  "Insert Claude-generated commit message into current commit buffer."
  (interactive)
  (if (and kill-ring (not (string-empty-p (car kill-ring))))
      (let ((commit-msg (string-trim (car kill-ring))))
        (when (and (boundp 'git-commit-mode) git-commit-mode)
          (goto-char (point-min))
          (insert commit-msg)
          (message "Inserted Claude-generated commit message")))
    (message "No Claude-generated commit message found. Run claude-commit-generate first.")))

;;;###autoload
(defun claude-commit-magit ()
  "Generate commit message with Claude for magit workflow."
  (interactive)
  (if (and (fboundp 'magit-anything-staged-p) (magit-anything-staged-p))
      (claude-commit-generate)
    (message "No changes staged for commit. Stage your changes first with 's' in magit.")))

;;; Configuration and Demo Functions

;;;###autoload
(defun claude-commit-demo-spinner ()
  "Demo the current spinner style for 2 seconds."
  (interactive)
  (claude-commit--start-spinner (format "Demo of '%s' spinner" claude-commit-spinner-style))
  (run-at-time 2 nil 'claude-commit--stop-spinner))

;;;###autoload
(defun claude-commit-cycle-spinner ()
  "Cycle through available spinner styles."
  (interactive)
  (let* ((styles (mapcar 'car claude-commit--spinner-frames-alist))
         (current-index (cl-position claude-commit-spinner-style styles))
         (next-index (% (1+ current-index) (length styles)))
         (next-style (nth next-index styles)))
    (setq claude-commit-spinner-style next-style)
    (message "Spinner style changed to: %s" next-style)
    (claude-commit-demo-spinner)))

;;;###autoload
(defun claude-commit-config ()
  "Open claude-commit customization group."
  (interactive)
  (customize-group 'claude-commit))

;;; Integration with Magit

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-c c") 'claude-commit-magit))

(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map (kbd "C-c C-i") 'claude-commit-insert))

;;; Minor Mode

(defvar claude-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g g") 'claude-commit-generate)
    (define-key map (kbd "C-c g a") 'claude-commit-auto)
    (define-key map (kbd "C-c g c") 'claude-commit-config)
    map)
  "Keymap for claude-commit-mode.")

;;;###autoload
(define-minor-mode claude-commit-mode
  "Minor mode for AI-powered commit messages using Claude."
  :lighter " Claude"
  :keymap claude-commit-mode-map
  :group 'claude-commit
  :global t)

(provide 'claude-commit)
;;; claude-commit.el ends here