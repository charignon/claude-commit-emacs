;;; demo-config.el --- Example configuration for claude-commit.el -*- lexical-binding: t; -*-

;;; Commentary:
;; This file shows various configuration examples for claude-commit.el
;; Copy and modify these snippets for your own config!

;;; Code:

;; ============================================================================
;; Basic Setup
;; ============================================================================

(use-package claude-commit
  :ensure t
  :config
  (claude-commit-mode 1))

;; ============================================================================
;; Customized Setup with Personal Preferences
;; ============================================================================

(use-package claude-commit
  :ensure t
  :custom
  ;; Use Sonnet model for better quality (haiku is faster/cheaper)
  (claude-commit-model "sonnet")

  ;; Use moon phases spinner for fun
  (claude-commit-spinner-style 'moon)

  ;; Auto-stage all files when using auto-commit
  (claude-commit-auto-stage-all t)

  ;; Custom success emoji
  (claude-commit-success-emoji "🚀")

  :config
  (claude-commit-mode 1))

;; ============================================================================
;; Team Configuration with Conventional Commits
;; ============================================================================

(use-package claude-commit
  :ensure t
  :custom
  ;; Team-specific prompt following Angular conventions
  (claude-commit-prompt-template
   "Generate a commit message following Angular commit conventions:
    - Format: type(scope): description
    - Types: feat, fix, docs, style, refactor, test, chore
    - Scope: affected module/component
    - Description: imperative mood, lowercase
    - Max 50 chars for header
    - Include body if needed for 'why' context
    Return only the commit message.")

  ;; Larger diff size for monorepo
  (claude-commit-max-diff-size 5000)

  :config
  (claude-commit-mode 1))

;; ============================================================================
;; Evil Mode Users
;; ============================================================================

(use-package claude-commit
  :ensure t
  :config
  (claude-commit-mode 1)

  ;; Add Evil mode bindings
  (with-eval-after-load 'evil
    (evil-define-key 'normal claude-commit-mode-map
      (kbd ", c g") 'claude-commit-generate
      (kbd ", c a") 'claude-commit-auto
      (kbd ", c c") 'claude-commit-config)))

;; ============================================================================
;; Doom Emacs Configuration
;; ============================================================================

;; In packages.el:
;; (package! claude-commit)

;; In config.el:
(use-package! claude-commit
  :config
  (claude-commit-mode 1)

  ;; Doom-specific keybindings
  (map! :leader
        :desc "Generate commit message"
        "g c g" #'claude-commit-generate
        :desc "Auto-commit with Claude"
        "g c a" #'claude-commit-auto
        :desc "Claude commit config"
        "g c C" #'claude-commit-config))

;; ============================================================================
;; Spacemacs Configuration
;; ============================================================================

;; Add to dotspacemacs-configuration-layers:
;; '((claude-commit :location (recipe
;;                             :fetcher github
;;                             :repo "charignon/claude-commit-emacs")))

;; In user-config:
(use-package claude-commit
  :config
  (claude-commit-mode 1)

  ;; Spacemacs-style keybindings
  (spacemacs/set-leader-keys
    "gcc" 'claude-commit-generate
    "gca" 'claude-commit-auto
    "gcC" 'claude-commit-config))

;; ============================================================================
;; Custom Keybindings
;; ============================================================================

(use-package claude-commit
  :ensure t
  :bind (;; Global bindings
         ("C-x v c" . claude-commit-generate)
         ("C-x v C" . claude-commit-auto)

         ;; Bind to function keys
         ("<f5>" . claude-commit-generate)
         ("<f6>" . claude-commit-auto))
  :config
  (claude-commit-mode 1))

;; ============================================================================
;; Project-Specific Configuration
;; ============================================================================

(use-package claude-commit
  :ensure t
  :config
  (claude-commit-mode 1)

  ;; Different prompts for different projects
  (defun my/claude-commit-setup ()
    "Set up project-specific claude-commit configuration."
    (cond
     ;; Work project
     ((string-match-p "work-project" default-directory)
      (setq-local claude-commit-prompt-template
                  "Generate commit with JIRA ticket ID (PROJ-XXXX) if applicable."))

     ;; Personal projects
     ((string-match-p "personal" default-directory)
      (setq-local claude-commit-prompt-template
                  "Generate fun, emoji-filled commit message."))

     ;; Default
     (t
      (setq-local claude-commit-prompt-template
                  claude-commit-prompt-template))))

  (add-hook 'find-file-hook 'my/claude-commit-setup))

;; ============================================================================
;; Integration with Other Packages
;; ============================================================================

(use-package claude-commit
  :ensure t
  :config
  (claude-commit-mode 1)

  ;; Integration with diff-hl
  (with-eval-after-load 'diff-hl
    (add-hook 'claude-commit-mode-hook 'diff-hl-mode))

  ;; Integration with git-gutter
  (with-eval-after-load 'git-gutter
    (add-hook 'claude-commit-mode-hook 'git-gutter-mode))

  ;; Auto-refresh projectile after commits
  (with-eval-after-load 'projectile
    (advice-add 'claude-commit--process-result :after
                (lambda (&rest _) (projectile-invalidate-cache nil)))))

;; ============================================================================
;; Advanced Workflow Automation
;; ============================================================================

(use-package claude-commit
  :ensure t
  :config
  (claude-commit-mode 1)

  ;; Auto-commit on save for certain files
  (defun my/auto-commit-on-save ()
    "Auto-commit certain files on save."
    (when (and (buffer-file-name)
               (string-match-p "\\.org$" (buffer-file-name))
               (string-match-p "journal" (buffer-file-name)))
      (claude-commit-auto)))

  (add-hook 'after-save-hook 'my/auto-commit-on-save)

  ;; Function to commit and push
  (defun my/claude-commit-and-push ()
    "Commit with Claude and push to remote."
    (interactive)
    (claude-commit-auto)
    (run-at-time 2 nil (lambda () (shell-command "git push"))))

  (global-set-key (kbd "C-c g p") 'my/claude-commit-and-push))

;; ============================================================================
;; Debugging Configuration
;; ============================================================================

(use-package claude-commit
  :ensure t
  :custom
  ;; Enable debug output (if implemented)
  ;; (claude-commit-debug t)

  :config
  (claude-commit-mode 1)

  ;; Add debug wrapper
  (defun my/claude-commit-debug-generate ()
    "Generate commit with debug output."
    (interactive)
    (message "Starting claude-commit generation...")
    (let ((start-time (current-time)))
      (claude-commit-generate)
      (run-at-time 3 nil
                   (lambda (start)
                     (message "Generation took: %.2f seconds"
                             (float-time (time-subtract (current-time) start))))
                   start-time))))

(provide 'demo-config)
;;; demo-config.el ends here