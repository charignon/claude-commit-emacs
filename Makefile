# Makefile for claude-commit.el

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

ELS = claude-commit.el
ELCS = $(ELS:.el=.elc)

.PHONY: all compile clean test package install

all: compile

# Byte compile
compile: $(ELCS)

%.elc: %.el
	@echo "Compiling $<..."
	@$(BATCH) -f batch-byte-compile $<

# Clean compiled files
clean:
	@echo "Cleaning..."
	@rm -f $(ELCS)
	@rm -f *.tar

# Run tests
test:
	@echo "Running tests..."
	@$(BATCH) -l ert -l test/claude-commit-test.el -f ert-run-tests-batch-and-exit

# Check for issues
check:
	@echo "Checking for issues..."
	@$(BATCH) --eval "(progn (require 'checkdoc) (checkdoc-file \"claude-commit.el\"))"
	@$(BATCH) --eval "(progn (require 'package-lint) (package-lint-batch-and-exit \"claude-commit.el\"))" 2>/dev/null || true

# Create package tarball
package: clean
	@echo "Creating package..."
	@tar cf claude-commit-$(shell grep -o "Version: [0-9.]*" claude-commit.el | cut -d' ' -f2).tar \
		--exclude=.git \
		--exclude=test \
		--exclude=Makefile \
		claude-commit.el \
		README.md

# Install locally
install: compile
	@echo "Installing locally..."
	@cp claude-commit.el* ~/.emacs.d/site-lisp/ 2>/dev/null || \
		mkdir -p ~/.emacs.d/site-lisp && cp claude-commit.el* ~/.emacs.d/site-lisp/

help:
	@echo "Available targets:"
	@echo "  compile  - Byte compile elisp files"
	@echo "  clean    - Remove compiled files"
	@echo "  test     - Run tests"
	@echo "  check    - Run linting and checks"
	@echo "  package  - Create distribution tarball"
	@echo "  install  - Install to ~/.emacs.d/site-lisp"