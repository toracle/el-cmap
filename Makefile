export EMACS ?= $(shell command -v emacs 2>/dev/null)

# Project directories
TEST_DIR = tests
SRC_DIR = .
DEPS_DIR = .deps

# Check if cask is available
CASK_AVAILABLE := $(shell command -v cask 2> /dev/null)

# Cask directory
ifdef CASK_AVAILABLE
    CASK_DIR := $(shell cask package-directory)
    DEPS_INSTALLED = cask-deps
else
    CASK_DIR = $(DEPS_DIR)
    DEPS_INSTALLED = direct-deps
endif

EMACS_BATCH = $(EMACS) --batch --quick

# Set verbose flag
VERBOSE ?= 0
ifeq ($(VERBOSE),1)
    V_FLAG=--eval "(setq ert-verbose t)"
else
    V_FLAG=--eval "(setq ert-quiet t)"
endif

.PHONY: test test-model test-repr test-buffer test-enhanced test-pure clean cask-deps direct-deps test-with-cask test-without-cask test-both docker-test test-coverage

# Install dependencies using Cask if available
$(CASK_DIR): Cask
ifdef CASK_AVAILABLE
	@echo "Installing dependencies using Cask..."
	@cask install
	@touch $(CASK_DIR)
else
	@echo "Installing dependencies directly (Cask not available)..."
	@mkdir -p $(DEPS_DIR)
	@if [ ! -d "$(DEPS_DIR)/s" ]; then \
		git clone https://github.com/magnars/s.el.git $(DEPS_DIR)/s 2>/dev/null || true; \
	fi
	@if [ ! -d "$(DEPS_DIR)/dash" ]; then \
		git clone https://github.com/magnars/dash.el.git $(DEPS_DIR)/dash 2>/dev/null || true; \
	fi
	@touch $(DEPS_DIR)
endif

cask-deps: $(CASK_DIR)

direct-deps: $(DEPS_DIR)
	@mkdir -p $(DEPS_DIR)
	@if [ ! -d "$(DEPS_DIR)/s" ]; then \
		git clone https://github.com/magnars/s.el.git $(DEPS_DIR)/s 2>/dev/null || true; \
	fi
	@if [ ! -d "$(DEPS_DIR)/dash" ]; then \
		git clone https://github.com/magnars/dash.el.git $(DEPS_DIR)/dash 2>/dev/null || true; \
	fi

# Default target - run all tests
test: $(DEPS_INSTALLED)
	@echo "Running all tests..."
ifdef CASK_AVAILABLE
	@cask emacs --batch \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=el-cmap-test.el
else
	@$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=el-cmap-test.el
endif

# Run model tests
test-model: $(DEPS_INSTALLED)
	@echo "Running model tests..."
ifeq ($(CASK_AVAILABLE),)
	@$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-model.el \
		--load=$(TEST_DIR)/test-el-cmap-model.el \
		--funcall=ert-run-tests-batch-and-exit
else
	@cask emacs --batch \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-model.el \
		--load=$(TEST_DIR)/test-el-cmap-model.el \
		--funcall=ert-run-tests-batch-and-exit
endif

# Run representation tests
test-repr: $(DEPS_INSTALLED)
	@echo "Running representation tests..."
ifeq ($(CASK_AVAILABLE),)
	@$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-model.el \
		--load=el-cmap-repr.el \
		--load=$(TEST_DIR)/test-el-cmap-repr.el \
		--funcall=ert-run-tests-batch-and-exit
else
	@cask emacs --batch \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-model.el \
		--load=el-cmap-repr.el \
		--load=$(TEST_DIR)/test-el-cmap-repr.el \
		--funcall=ert-run-tests-batch-and-exit
endif

# Run buffer tests
test-buffer: $(DEPS_INSTALLED)
	@echo "Running buffer tests..."
ifeq ($(CASK_AVAILABLE),)
	@$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-buffer.el \
		--load=$(TEST_DIR)/test-el-cmap-buffer.el \
		--funcall=ert-run-tests-batch-and-exit
else
	@cask emacs --batch \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-buffer.el \
		--load=$(TEST_DIR)/test-el-cmap-buffer.el \
		--funcall=ert-run-tests-batch-and-exit
endif

# Run enhanced tests only
test-enhanced: $(DEPS_INSTALLED)
	@echo "Running enhanced tests..."
ifeq ($(CASK_AVAILABLE),)
	@$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=ert \
		--load=cl-lib \
		--load=$(TEST_DIR)/test-el-cmap-model-enhanced.el \
		--load=$(TEST_DIR)/test-el-cmap-buffer-enhanced.el \
		--load=$(TEST_DIR)/test-el-cmap-repr-enhanced.el \
		--funcall=ert-run-tests-batch-and-exit
else
	@cask emacs --batch \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=cl-lib \
		--load=$(TEST_DIR)/test-el-cmap-model-enhanced.el \
		--load=$(TEST_DIR)/test-el-cmap-buffer-enhanced.el \
		--load=$(TEST_DIR)/test-el-cmap-repr-enhanced.el \
		--funcall=ert-run-tests-batch-and-exit
endif

# Run pure function tests only
test-pure: $(DEPS_INSTALLED)
	@echo "Running pure function tests..."
ifeq ($(CASK_AVAILABLE),)
	@$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=ert \
		--load=cl-lib \
		--load=el-cmap-model.el \
		--load=el-cmap-model-pure.el \
		--load=$(TEST_DIR)/test-el-cmap-model-pure.el \
		--funcall=ert-run-tests-batch-and-exit
else
	@cask emacs --batch \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=cl-lib \
		--load=el-cmap-model.el \
		--load=el-cmap-model-pure.el \
		--load=$(TEST_DIR)/test-el-cmap-model-pure.el \
		--funcall=ert-run-tests-batch-and-exit
endif

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@find . -name "*.elc" -type f -delete 2>/dev/null || true
ifdef CASK_AVAILABLE
	@cask clean-elc 2>/dev/null || true
else
	@rm -rf $(DEPS_DIR) 2>/dev/null || true
endif

# Test with Cask (forcing Cask to be used regardless of environment)
test-with-cask:
	@echo "Forcing tests to run using Cask..."
	@CASK_AVAILABLE=true $(MAKE) test

# Test without Cask (forcing direct dependencies regardless of environment)
test-without-cask:
	@echo "Forcing tests to run using direct dependencies..."
	@CASK_AVAILABLE= $(MAKE) test

# Test in both environments
test-both: clean test-with-cask clean test-without-cask

# Docker test environment
docker-test:
	docker run -v $(shell pwd):/el-cmap --rm silex/emacs:27.2-ci-cask sh -c "cd /el-cmap && make test"

# Run tests with coverage
test-coverage: $(DEPS_INSTALLED) clean
	@echo "Running tests with coverage..."
	@mkdir -p coverage
ifdef CASK_AVAILABLE
	@cask emacs --batch \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=run-coverage.el
else
	@$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=run-coverage.el
endif
	@if [ -f "coverage/lcov.info" ]; then \
		echo "Coverage report generated in coverage/lcov.info"; \
		if command -v lcov >/dev/null 2>&1; then \
			mkdir -p coverage/html; \
			lcov --summary coverage/lcov.info; \
			genhtml coverage/lcov.info --output-directory coverage/html; \
			echo "HTML report generated in coverage/html/index.html"; \
		fi \
	else \
		echo "Coverage report was not generated. Check for errors."; \
	fi