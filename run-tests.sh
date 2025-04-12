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
    echo -e "Available test targets:"
    echo -e "  all            Run all tests (default)"
    echo -e "  model          Run model tests only"
    echo -e "  repr           Run representation tests only"
    echo -e "  buffer         Run buffer tests only"
    echo -e "  enhanced       Run enhanced tests with reduced side effects"
    echo -e "  pure           Run pure functional model tests"
    exit 1
}

# Check if Cask is installed
if ! command -v cask >/dev/null 2>&1; then
    echo -e "${RED}Cask is required but not installed.${NC}"
    echo -e "Please install Cask from: https://github.com/cask/cask"
    exit 1
fi

TEST_TARGET="test"
VERBOSE=""

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

echo -e "${YELLOW}Running ${TEST_TARGET} for el-cmap...${NC}"
echo ""

# Make sure dependencies are installed
echo -e "${BLUE}Installing dependencies with Cask...${NC}"
cask install >/dev/null 2>&1

# Run the tests
if [ -n "$VERBOSE" ]; then
    make $TEST_TARGET $VERBOSE
else
    # Filter some of the verbose output for cleaner display
    make $TEST_TARGET $VERBOSE | grep -v "Running" | grep -v "directory=" | grep -v "cask exec"
fi

# Check exit status
if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}All tests passed successfully!${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}Some tests failed. See output above for details.${NC}"
    exit 1
fi