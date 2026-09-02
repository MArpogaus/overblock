# Development tasks.  Run `make' to check everything, as the CI does.
#
#   make compile   byte-compile, warnings are errors
#   make checkdoc  documentation style
#   make lint      package-lint, the MELPA rules
#   make relint    regexp and docstring escapes
#   make test      ERT test suite, STRICT=1 to refuse to skip
#   make complexity  what each function costs a reader
#   make format    indent every Lisp file in place
#   make scroll    scrolling tests, which need a display
#   make clean     remove build output and the tool sandbox
#
# The checks install their tools and this package's dependencies into
# $(SANDBOX), so a fresh checkout needs nothing but Emacs and make.

EMACS   ?= emacs
SANDBOX ?= .sandbox
DEPS    ?= package-lint relint code-cells comint-mime

SRC  := $(filter-out %-autoloads.el %-pkg.el,$(wildcard *.el))
TEST := $(wildcard test/*.el)
# Everything written in Lisp, the parts that are no package included.
LISP := $(SRC) $(TEST) $(wildcard demo/*.el) $(wildcard tools/*.el)

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
checkdoc = (progn (require (quote checkdoc)) \
                  (setq checkdoc-verb-check-experimental-flag nil) \
                  (dolist (f command-line-args-left) (checkdoc-file f)))

BATCH = $(EMACS) -Q --batch -L . -L test -L tools --eval '$(init)'

.PHONY: all compile checkdoc lint relint test complexity format scroll clean

all: compile checkdoc lint relint test

$(SANDBOX):
	@$(EMACS) -Q --batch --eval '$(init)' --eval '$(bootstrap)'

compile: $(SANDBOX)
	@$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC) $(TEST)
	@rm -f ./*.elc test/*.elc

# checkdoc reports on stderr and always exits zero, so treat any output
# as a failure.
checkdoc:
	@out=$$($(BATCH) --eval '$(checkdoc)' $(SRC) 2>&1); \
	  if [ -n "$$out" ]; then printf '%s\n' "$$out"; exit 1; fi

# Two packages live here: the block layer, which knows nothing about
# Python, and the notebook that uses it.  package-lint reads one main
# file and calls every symbol outside its prefix an error, so it is run
# once for each of them.
lint: $(SANDBOX)
	@$(BATCH) --eval '(setq package-lint-main-file "overblock.el")' \
	  -f package-lint-batch-and-exit overblock.el overblock-md.el \
	  overblock-repl.el
	@$(BATCH) --eval '(setq package-lint-main-file "pycell.el")' \
	  -f package-lint-batch-and-exit pycell.el

# What checkdoc and package-lint both let through: a docstring escape
# written \= rather than \\=, which the reader eats, so `describe-function'
# shows the reader the = as text.
relint: $(SANDBOX)
	@$(BATCH) -l relint -f relint-batch $(SRC) $(TEST)

# A fifth of the suite renders markdown and skips itself where no
# converter is installed.  On a machine that is meant to have one that
# silence is a lie, so STRICT=1 makes it a failure instead.
test: $(SANDBOX)
	@$(BATCH) $(addprefix -l ,$(TEST)) \
	  $(if $(STRICT),--eval '$(strict)') -f ert-run-tests-batch-and-exit

# A block is one buffer line and can be taller than the window, and only
# a graphical frame gives a line a pixel height.  These tests therefore
# run in a real frame, under `xvfb-run' where there is no display.
# With -a: xvfb-run picks a free display instead of exiting 1 over a
# stale lock file, which reads like a test failure.
XVFB := $(shell command -v xvfb-run >/dev/null 2>&1 && echo xvfb-run -a)

scroll: $(SANDBOX)
	@rm -f scroll-report.txt
	@$(XVFB) $(EMACS) -Q -L . -L test --eval '$(init)' \
	  -l test/run-scroll.el; status=$$?; \
	  cat scroll-report.txt 2>/dev/null; exit $$status

# A report and not a gate: there is no complexity rule to fail, only
# functions worth looking at.  See tools/complexity.el.
complexity:
	@$(BATCH) -l complexity \
	  --eval '(complexity-report command-line-args-left)' $(SRC)

# The formatter loads each file before indenting it, so a macro of this
# package indents its body the way its `declare' says; that needs the
# load path and the dependencies, which is why it wants the sandbox.
# It answers 1 when it had to change something, which is how the hook
# stops a commit; from make that is a job done, not a failure.
format: $(SANDBOX)
	@$(BATCH) -l tools/indent.el $(LISP) || true

clean:
	@rm -rf $(SANDBOX) ./*.elc test/*.elc scroll-report.txt
