# Emacs executable settings
EMACS ?= emacs
EMACS_BATCH = $(EMACS) --batch --quick

# Project directories
TEST_DIR = tests
SRC_DIR = .

# Set verbose flag
VERBOSE ?= 0
ifeq ($(VERBOSE),1)
    V_FLAG=--eval "(setq ert-verbose t)"
else
    V_FLAG=--eval "(setq ert-quiet t)"
endif

.PHONY: test test-model test-repr test-buffer test-enhanced test-pure install-deps clean

# Install dependencies using Cask
install-deps:
	@command -v cask >/dev/null 2>&1 || { echo >&2 "Cask is required. Please install it using instructions from https://github.com/cask/cask"; exit 1; }
	@echo "Installing dependencies using Cask..."
	@cask install

# Default target - run all tests
test: install-deps
	@echo "Running all tests..."
	cask exec $(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=el-cmap-test.el

# Run model tests
test-model: install-deps
	@echo "Running model tests..."
	cask exec $(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-model.el \
		--load=$(TEST_DIR)/test-el-cmap-model.el \
		--funcall=ert-run-tests-batch-and-exit

# Run representation tests
test-repr: install-deps
	@echo "Running representation tests..."
	cask exec $(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-model.el \
		--load=el-cmap-repr.el \
		--load=$(TEST_DIR)/test-el-cmap-repr.el \
		--funcall=ert-run-tests-batch-and-exit

# Run buffer tests
test-buffer: install-deps
	@echo "Running buffer tests..."
	cask exec $(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-buffer.el \
		--load=$(TEST_DIR)/test-el-cmap-buffer.el \
		--funcall=ert-run-tests-batch-and-exit

# Run enhanced tests only
test-enhanced: install-deps
	@echo "Running enhanced tests..."
	cask exec $(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=cl-lib \
		--load=$(TEST_DIR)/test-el-cmap-model-enhanced.el \
		--load=$(TEST_DIR)/test-el-cmap-buffer-enhanced.el \
		--load=$(TEST_DIR)/test-el-cmap-repr-enhanced.el \
		--funcall=ert-run-tests-batch-and-exit

# Run pure function tests only
test-pure: install-deps
	@echo "Running pure function tests..."
	cask exec $(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		$(V_FLAG) \
		--load=ert \
		--load=cl-lib \
		--load=el-cmap-model.el \
		--load=el-cmap-model-pure.el \
		--load=$(TEST_DIR)/test-el-cmap-model-pure.el \
		--funcall=ert-run-tests-batch-and-exit

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@find . -name "*.elc" -type f -delete
	@cask clean-elc

# Clean Cask artifacts
cask-clean:
	@echo "Cleaning Cask artifacts..."
	@cask clean-elc
	@rm -rf .cask

# Run with Docker (for CI environments)
docker-test:
	docker run -v $(shell pwd):/el-cmap --rm silex/emacs:27.2-ci-cask sh -c "cd /el-cmap && cask install && make test"