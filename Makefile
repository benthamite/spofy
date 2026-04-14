EMACS ?= emacs
ELPACA_REPOS := $(dir $(CURDIR))
LOAD_PATH := -L $(CURDIR) \
             -L $(ELPACA_REPOS)transient/lisp \
             -L $(ELPACA_REPOS)cond-let

# Optional integration modules that need their deps on load-path
CONSULT_DIR := $(ELPACA_REPOS)consult
EMBARK_DIR := $(ELPACA_REPOS)embark
COMPAT_DIR := $(ELPACA_REPOS)compat

ifneq ($(wildcard $(CONSULT_DIR)),)
  LOAD_PATH += -L $(CONSULT_DIR)
endif
ifneq ($(wildcard $(EMBARK_DIR)),)
  LOAD_PATH += -L $(EMBARK_DIR)
endif
ifneq ($(wildcard $(COMPAT_DIR)),)
  LOAD_PATH += -L $(COMPAT_DIR)
endif

SRC_FILES := $(filter-out test/%,$(wildcard *.el))

.PHONY: test compile clean

test:
	for f in test/*-test.el; do \
	  echo "=== Running $$(basename $$f) ===" ; \
	  $(EMACS) -Q --batch $(LOAD_PATH) -L test \
	    -l ert -l "$$f" \
	    -f ert-run-tests-batch-and-exit || exit 1 ; \
	done

compile:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval '(setq byte-compile-error-on-warn nil)' \
	  -f batch-byte-compile $(SRC_FILES)

clean:
	rm -f *.elc
