# claude-commit.el

> AI-powered commit messages using Anthropic's Claude 🎉

[![MELPA](https://melpa.org/packages/claude-commit-badge.svg)](https://melpa.org/#/claude-commit)

Generate meaningful, conventional commit messages automatically using Anthropic's Claude AI. Perfect for developers who want consistent, descriptive commit messages without the mental overhead.

## ✨ Features

- **🤖 AI-Powered**: Uses Anthropic's Claude via the Claude Code CLI
- **🎯 Smart Analysis**: Analyzes both staged and unstaged changes for context
- **⚡ Multiple Entry Points**: Generate on-demand, auto-commit, or insert into commit buffers
- **🎨 Beautiful Spinners**: 8 different animation styles during generation
- **🔧 Highly Configurable**: Custom prompts, models, and behavior
- **🧙 Magit Integration**: Seamless workflow with Magit
- **⚡ Async Execution**: Non-blocking UI with real-time feedback

## 🎬 Demo

![Claude Commit Demo](demo.gif)

*Watch as Claude analyzes your changes and generates perfect commit messages with beautiful spinner animations!*

## 📋 Requirements

- Emacs 26.1+
- [Claude Code CLI](https://www.npmjs.com/package/@anthropic-ai/claude): `npm install -g @anthropic-ai/claude`
- Git repository
- Magit (recommended but optional)

## 📦 Installation

### Via MELPA (Recommended)

```elisp
(use-package claude-commit
  :ensure t
  :config
  (claude-commit-mode 1))
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/charignon/claude-commit-emacs.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/claude-commit-emacs")
   (require 'claude-commit)
   (claude-commit-mode 1)
   ```

## 🚀 Usage

### Quick Start

1. Make some changes to your code
2. Stage them with `git add` or in Magit with `s`
3. Run `M-x claude-commit-generate`
4. Watch the beautiful spinner as Claude analyzes your changes
5. Use the generated message or run `M-x claude-commit-auto` to commit automatically

### Available Commands

| Command | Description | Keybinding |
|---------|-------------|------------|
| `claude-commit-generate` | Generate commit message for current changes | `C-c g g` |
| `claude-commit-auto` | Stage changes and auto-commit with AI message | `C-c g a` |
| `claude-commit-insert` | Insert generated message into commit buffer | `C-c C-i` (in commit buffer) |
| `claude-commit-magit` | Generate message for Magit workflow | `C-c c` (in magit-status) |
| `claude-commit-config` | Open configuration | `C-c g c` |
| `claude-commit-demo-spinner` | Demo current spinner style | - |
| `claude-commit-cycle-spinner` | Cycle through spinner styles | - |

### Workflow Examples

#### Standard Git Workflow
```bash
# Make your changes
git add .
M-x claude-commit-generate  # Generates message
git commit -m "paste the generated message"
```

#### Auto-Commit Workflow
```bash
# Make your changes (don't stage them)
M-x claude-commit-auto  # Stages and commits automatically
```

#### Magit Workflow
```
1. Open Magit: M-x magit-status
2. Stage changes: s
3. Generate message: C-c c
4. Start commit: c c
5. Insert message: C-c C-i
6. Commit: C-c C-c
```

## ⚙️ Configuration

### Basic Configuration

```elisp
(use-package claude-commit
  :ensure t
  :custom
  ;; Choose your Claude model
  (claude-commit-model "sonnet")  ; haiku, sonnet, or opus

  ;; Customize the prompt
  (claude-commit-prompt-template
   "Create a commit message following Angular conventions. Focus on the business impact.")

  ;; Auto-stage all files vs tracked only
  (claude-commit-auto-stage-all t)

  ;; Choose your favorite spinner
  (claude-commit-spinner-style 'moon)  ; dots, circle, arrow, stars, blocks, moon, earth, bounce

  ;; Customize success emoji
  (claude-commit-success-emoji "🚀")

  :config
  (claude-commit-mode 1))
```

### Advanced Configuration

```elisp
;; Custom keybindings
(use-package claude-commit
  :ensure t
  :bind (("C-x g a" . claude-commit-auto)
         ("C-x g g" . claude-commit-generate))
  :custom
  ;; Limit diff size sent to Claude (for large changes)
  (claude-commit-max-diff-size 5000)

  ;; Custom prompt for your team's style
  (claude-commit-prompt-template
   "Generate a commit message using our team convention:
    - Start with JIRA ticket ID if applicable
    - Use imperative mood
    - Max 50 characters for title
    - Include 'why' not just 'what'
    Return only the commit message.")

  :config
  (claude-commit-mode 1))
```

## 🎨 Spinner Styles

Choose from 8 beautiful spinner animations:

- **dots**: ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏ (default)
- **circle**: ◐◓◑◒
- **arrow**: ←↖↑↗→↘↓↙
- **stars**: ✶✸✹✺✹✸
- **blocks**: ▉▊▋▌▍▎▏▎▍▌▋▊▉
- **moon**: 🌑🌒🌓🌔🌕🌖🌗🌘
- **earth**: 🌍🌎🌏
- **bounce**: ⠁⠂⠄⡀⢀⠠⠐⠈

Try them with `M-x claude-commit-cycle-spinner`!

## 🔧 Troubleshooting

### Common Issues

**"claude: command not found"**
```bash
# Install Claude Code CLI
npm install -g @anthropic-ai/claude

# Verify installation
which claude
claude --version
```

**"No git changes found"**
- Make sure you're in a git repository
- Stage your changes with `git add` or in Magit

**Spinner not showing**
- This is normal for very fast responses
- Try `M-x claude-commit-demo-spinner` to test

**Large repositories are slow**
- Adjust `claude-commit-max-diff-size` to limit diff size
- Consider staging specific files rather than using `git add .`

### Debug Mode

```elisp
;; Enable debug messages
(setq claude-commit-debug t)
```

## 🤝 Contributing

We welcome contributions! Here's how to get started:

1. Fork the repository
2. Create a feature branch: `git checkout -b my-new-feature`
3. Make your changes
4. Add tests if applicable
5. Commit using claude-commit: `M-x claude-commit-auto` 😉
6. Push to your branch: `git push origin my-new-feature`
7. Create a Pull Request

### Development Setup

```bash
git clone https://github.com/charignon/claude-commit-emacs.git
cd claude-commit-emacs

# Install dependencies
npm install -g @anthropic-ai/claude

# Run tests
emacs -batch -l ert -l test/claude-commit-test.el -f ert-run-tests-batch-and-exit
```

## 🙏 Acknowledgments

- [Anthropic](https://www.anthropic.com/) for Claude AI
- [Claude Code CLI](https://www.npmjs.com/package/@anthropic-ai/claude) team
- [Magit](https://magit.vc/) for Git integration inspiration
- The Emacs community for feedback and contributions

## 🔗 Related Projects

- [conventional-commits](https://www.conventionalcommits.org/) - Commit message convention
- [commitizen](https://github.com/commitizen/cz-cli) - CLI for conventional commits
- [gitmoji](https://gitmoji.dev/) - Emoji guide for commit messages

---

Made with ❤️ and Claude AI. Star ⭐ this repo if it makes your git workflow more delightful!