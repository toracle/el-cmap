# Emacs executable settings
EMACS ?= emacs
EMACS_BATCH = $(EMACS) --batch --quick

# Project directories
TEST_DIR = tests
SRC_DIR = .

# Dependencies
DEPS_DIR = deps

.PHONY: test test-model test-repr test-buffer test-enhanced test-pure install-deps clean

# Create deps directory if it doesn't exist
$(DEPS_DIR):
	mkdir -p $(DEPS_DIR)

# Install dependencies if needed
install-deps: $(DEPS_DIR)
	@echo "Installing dependencies in $(DEPS_DIR)..."
	@if [ ! -d "$(DEPS_DIR)/s" ]; then \
		git clone https://github.com/magnars/s.el.git $(DEPS_DIR)/s; \
	fi
	@if [ ! -d "$(DEPS_DIR)/dash" ]; then \
		git clone https://github.com/magnars/dash.el.git $(DEPS_DIR)/dash; \
	fi

# Set verbose flag
VERBOSE ?= 0
ifeq ($(VERBOSE),1)
    V_FLAG=--eval "(setq ert-verbose t)"
else
    V_FLAG=--eval "(setq ert-quiet t)"
endif

# Default target - run all tests
test: install-deps
	$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=el-cmap-test.el

# Run model tests
test-model: install-deps
	$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-model.el \
		--load=$(TEST_DIR)/test-el-cmap-model.el \
		--funcall=ert-run-tests-batch-and-exit

# Run representation tests
test-repr: install-deps
	$(EMACS_BATCH) \
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

# Run buffer tests
test-buffer: install-deps
	$(EMACS_BATCH) \
		--directory=$(SRC_DIR) \
		--directory=$(TEST_DIR) \
		--directory=$(DEPS_DIR)/s \
		--directory=$(DEPS_DIR)/dash \
		$(V_FLAG) \
		--load=ert \
		--load=el-cmap-buffer.el \
		--load=$(TEST_DIR)/test-el-cmap-buffer.el \
		--funcall=ert-run-tests-batch-and-exit

# Run enhanced tests only
test-enhanced: install-deps
	$(EMACS_BATCH) \
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

# Run pure function tests only
test-pure: install-deps
	$(EMACS_BATCH) \
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

# Clean compiled files
clean:
	@echo "Cleaning compiled files..."
	@find . -name "*.elc" -type f -delete

# Remove dependencies
clean-deps:
	@echo "Removing dependencies..."
	@rm -rf $(DEPS_DIR)

# Run with Docker (for CI environments)
docker-test:
	docker run -v $(shell pwd):/el-cmap --rm silex/emacs:27.2 sh -c "cd /el-cmap && make test"