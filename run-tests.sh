#!/bin/bash

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

usage() {
    echo -e "Usage: $0 [options] [test-name]"
    echo -e "Options:"
    echo -e "  -h, --help     Show this help message"
    echo -e "  -v, --verbose  Show detailed test output"
    echo -e "  --with-cask    Force tests to use Cask regardless of environment"
    echo -e "  --without-cask Force tests to use direct dependencies regardless of environment"
    echo -e "  --test-both    Run tests with both Cask and direct dependencies"
    echo -e "Available test targets:"
    echo -e "  all            Run all tests (default)"
    echo -e "  model          Run model tests only"
    echo -e "  repr           Run representation tests only"
    echo -e "  buffer         Run buffer tests only"
    echo -e "  enhanced       Run enhanced tests with reduced side effects"
    echo -e "  pure           Run pure functional model tests"
    exit 1
}

# Check if Cask is available and inform the user about dependency management
if command -v cask >/dev/null 2>&1; then
    echo -e "${BLUE}Using Cask for dependency management${NC}"
    echo -e "Dependencies will be installed via Cask from package repositories."
else
    echo -e "${YELLOW}Cask not found. Using direct dependency installation.${NC}"
    echo -e "Dependencies will be cloned from GitHub to .deps directory."
fi

TEST_TARGET="test"
VERBOSE=""
CASK_MODE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            usage
            ;;
        -v|--verbose)
            VERBOSE="VERBOSE=1"
            shift
            ;;
        --with-cask)
            CASK_MODE="test-with-cask"
            shift
            ;;
        --without-cask)
            CASK_MODE="test-without-cask"
            shift
            ;;
        --test-both)
            CASK_MODE="test-both"
            shift
            ;;
        model)
            TEST_TARGET="test-model"
            shift
            ;;
        repr)
            TEST_TARGET="test-repr"
            shift
            ;;
        buffer)
            TEST_TARGET="test-buffer"
            shift
            ;;
        enhanced)
            TEST_TARGET="test-enhanced"
            shift
            ;;
        pure)
            TEST_TARGET="test-pure"
            shift
            ;;
        all)
            TEST_TARGET="test"
            shift
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            usage
            ;;
    esac
done

# Check if we're using a specific Cask mode
if [ -n "$CASK_MODE" ]; then
    if [ "$CASK_MODE" = "test-both" ]; then
        echo -e "${YELLOW}Running ${TEST_TARGET} with both Cask and direct dependencies...${NC}"
        echo -e "${BLUE}This will run the tests twice - first with Cask, then with direct dependencies.${NC}"
        MAKE_TARGET="test-both"
    else
        echo -e "${YELLOW}Running ${TEST_TARGET} with ${CASK_MODE}...${NC}"
        MAKE_TARGET="$CASK_MODE"
    fi
else
    echo -e "${YELLOW}Running ${TEST_TARGET} for el-cmap...${NC}"
    MAKE_TARGET="$TEST_TARGET"
fi

echo ""

# Run the tests
if [ -n "$VERBOSE" ]; then
    make $MAKE_TARGET $VERBOSE
    EXIT_STATUS=$?
else
    # Capture output to a temporary file to filter
    TMP_FILE=$(mktemp)
    make $MAKE_TARGET $VERBOSE > $TMP_FILE
    EXIT_STATUS=$?
    # Filter some of the verbose output for cleaner display
    cat $TMP_FILE | grep -v "Running" | grep -v "directory=" | grep -v "cask exec" | grep -v "Forcing tests"
    rm $TMP_FILE
fi

echo ""
if [ $EXIT_STATUS -eq 0 ]; then
    echo -e "${GREEN}All tests passed successfully!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed. See output above for details.${NC}"
    exit 1
fi