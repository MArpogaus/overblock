# Development tasks.  Run `make' to check everything, as the CI does.
#
#   make compile   byte-compile, warnings are errors
#   make lint      package-lint, the MELPA rules
#   make relint    regexp and docstring escapes
#   make test      ERT test suite, STRICT=1 to refuse to skip
#   make test-live the suite against a real ipython; skips without one
#   make scroll    scrolling tests, which need a display
#   make clean     remove build output and the tool sandbox
#
# The indent, checkdoc and complexity checks are pre-commit hooks of
# https://github.com/MArpogaus/emacs-pre-commit-hooks, not targets here.
#
# The checks install their tools and this package's dependencies into
# $(SANDBOX), so a fresh checkout needs nothing but Emacs and make.

EMACS   ?= emacs
SANDBOX ?= .sandbox
# The sandbox is done when the stamp is there: a run that dies half
# way leaves the directory behind, and a directory target would then
# count as made and the tools stay missing.
STAMP   := $(SANDBOX)/.installed
DEPS    ?= package-lint relint code-cells comint-mime

SRC  := $(filter-out %-autoloads.el %-pkg.el,$(wildcard *.el))
TEST := $(wildcard test/*.el)
# The live suite drives a real IPython, so the batch suite does not load
# it; `test-live' below is its target.  It is still in TEST, so it is
# byte-compiled and relinted with the rest.
SUITE := $(filter-out test/overblock-pycell-live-test.el,$(TEST))

# Elisp programs live in variables: make joins their continuation lines,
# while a backslash inside a quoted recipe line would reach Emacs as is.
init = (progn (setq package-user-dir (expand-file-name "$(SANDBOX)")) \
              (require (quote package)) \
              (add-to-list (quote package-archives) \
                           (cons "melpa" "https://melpa.org/packages/") t) \
              (package-initialize))
bootstrap = (progn (package-refresh-contents) \
                   (dolist (p (quote ($(DEPS)))) \
                     (unless (package-installed-p p) (package-install p))))
strict = (unless (overblock-md-program) \
           (error "No markdown converter: the markdown tests would skip"))

BATCH = $(EMACS) -Q --batch -L . -L test --eval '$(init)'

.PHONY: all compile lint relint test test-live scroll clean

all: compile lint relint test

$(STAMP):
	@$(EMACS) -Q --batch --eval '$(init)' --eval '$(bootstrap)'
	@touch $@

compile: $(STAMP)
	@$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC) $(TEST)
	@rm -f ./*.elc test/*.elc

# Two packages live here, and these are their files.  overblock is the
# block layer and the two modes built on it, and it knows nothing about
# Python; overblock-pycell is the notebook that uses the layer.  package-lint
# reads one main file and calls every symbol outside its prefix an
# error, so it is run once for each package.  The lists below are the
# whole of each package: what moves when the two get a repository each.
OVERBLOCK := overblock.el overblock-md.el overblock-repl.el \
             overblock-md-preview.el overblock-pydoc.el
PYCELL    := overblock-pycell.el

# overblock-pycell requires overblock, which has no MELPA recipe yet, so
# package-lint calls the dependency uninstallable.  The layer is in this
# checkout, so it is registered from here: a descriptor alone, no copy of
# the sources, or the copy would shadow the working tree on the load
# path.  This goes when overblock is published and the sandbox can
# install it like any other dependency.
OBVER := $(shell sed -n 's/^;; Version: //p' overblock.el)
OBDIR := $(SANDBOX)/overblock-$(OBVER)
OBPKG := (define-package "overblock" "$(OBVER)" \
          "Text blocks over a buffer" (quote ((emacs "29.1"))))

lint: $(STAMP)
	@mkdir -p $(OBDIR)
	@echo '$(OBPKG)' > $(OBDIR)/overblock-pkg.el
	@: > $(OBDIR)/overblock-autoloads.el
	@$(BATCH) --eval '(setq package-lint-main-file "overblock.el")' \
	  -f package-lint-batch-and-exit $(OVERBLOCK)
	@$(BATCH) --eval '(setq package-lint-main-file "overblock-pycell.el")' \
	  -f package-lint-batch-and-exit $(PYCELL)

# What checkdoc and package-lint both let through: a docstring escape
# written \= rather than \\=, which the reader eats, so `describe-function'
# shows the reader the = as text.
relint: $(STAMP)
	@$(BATCH) -l relint -f relint-batch $(SRC) $(TEST)

# A fifth of the suite renders markdown and skips itself where no
# converter is installed.  On a machine that is meant to have one that
# silence is a lie, so STRICT=1 makes it a failure instead.
test: $(STAMP)
	@$(BATCH) $(addprefix -l ,$(SUITE)) \
	  $(if $(STRICT),--eval '$(strict)') -f ert-run-tests-batch-and-exit

# What only a live interpreter can prove: two faults — a read-only
# notebook wedging the pass, and a result painted as a prompt — survived
# every batch check, because no batch test starts a process.  Not part
# of `all', because a machine without an ipython can only skip it; the
# CI installs one and runs this with STRICT=1, where a skip is a
# failure rather than a line nobody reads.
test-live: $(STAMP)
	@if command -v ipython >/dev/null 2>&1; then \
	  $(BATCH) -l test/overblock-pycell-live-test.el -f ert-run-tests-batch-and-exit; \
	elif [ -n "$(STRICT)" ]; then \
	  echo "test-live: no ipython installed, and STRICT asks for one"; \
	  exit 1; \
	else echo "test-live: no ipython installed, skipping"; fi

# A block is one buffer line and can be taller than the window, and only
# a graphical frame gives a line a pixel height.  These tests therefore
# run in a real frame, under `xvfb-run' where there is no display.
# With -a: xvfb-run picks a free display instead of exiting 1 over a
# stale lock file, which reads like a test failure.
XVFB := $(shell command -v xvfb-run >/dev/null 2>&1 && echo xvfb-run -a)

scroll: $(STAMP)
	@rm -f scroll-report.txt
	@$(XVFB) $(EMACS) -Q -L . -L test --eval '$(init)' \
	  -l test/run-scroll.el; status=$$?; \
	  cat scroll-report.txt 2>/dev/null; exit $$status

clean:
	@rm -rf $(SANDBOX) ./*.elc test/*.elc scroll-report.txt
