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
    echo -e "  --coverage     Run tests with coverage reporting"
    echo -e "  --html-report  Generate HTML coverage report (implies --coverage)"
    echo -e "Available test targets:"
    echo -e "  all            Run all tests (default)"
    echo -e "  model          Run model tests only"
    echo -e "  repr           Run representation tests only"
    echo -e "  buffer         Run buffer tests only"
    echo -e "  enhanced       Run enhanced tests with reduced side effects"
    echo -e "  pure           Run pure functional model tests"
    echo -e "  coverage       Run tests with coverage reporting"
    echo -e "  html-report    Run tests with coverage and generate HTML report"
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
COVERAGE=""
HTML_REPORT=""

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
        --coverage)
            COVERAGE="1"
            shift
            ;;
        --html-report)
            HTML_REPORT="1"
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
        coverage)
            TEST_TARGET="test-coverage"
            COVERAGE="1"
            shift
            ;;
        html-report)
            TEST_TARGET="test-coverage"
            COVERAGE="1"
            HTML_REPORT="1"
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
elif [ -n "$COVERAGE" ] && [ "$TEST_TARGET" != "test-coverage" ]; then
    echo -e "${YELLOW}Running ${TEST_TARGET} with coverage enabled...${NC}"
    MAKE_TARGET="test-coverage"
else
    echo -e "${YELLOW}Running ${TEST_TARGET} for el-cmap...${NC}"
    MAKE_TARGET="$TEST_TARGET"
fi

# Create coverage directory if needed
if [ -n "$COVERAGE" ] || [ "$TEST_TARGET" = "test-coverage" ]; then
    mkdir -p coverage
    echo -e "${BLUE}Coverage reporting is enabled. Reports will be generated in the coverage directory.${NC}"
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
    
    # Display coverage report info if coverage was enabled
    if [ -n "$COVERAGE" ] || [ "$TEST_TARGET" = "test-coverage" ]; then
        if [ -f "coverage/lcov.info" ]; then
            echo -e "${BLUE}Coverage report generated in coverage/lcov.info${NC}"
            
            # If lcov is installed, show a summary
            if command -v lcov >/dev/null 2>&1; then
                echo -e "${YELLOW}Coverage Summary:${NC}"
                lcov --summary coverage/lcov.info
                
                # Generate HTML report if requested
                if [ -n "$HTML_REPORT" ]; then
                    mkdir -p coverage/html
                    genhtml coverage/lcov.info --output-directory coverage/html
                    echo -e "${BLUE}HTML report generated in coverage/html/index.html${NC}"
                fi
            fi
            
            echo -e "${YELLOW}To view coverage in Emacs:${NC}"
            echo -e "1. Install cov-mode: M-x package-install RET cov RET"
            echo -e "2. Load the coverage viewer: M-x load-file RET $(pwd)/show-coverage.el RET"
            echo -e "3. Enable coverage overlay: M-x el-cmap-coverage-show RET"
        else
            echo -e "${RED}Coverage report was not generated. Check for errors above.${NC}"
        fi
    fi
    
    exit 0
else
    echo -e "${RED}Some tests failed. See output above for details.${NC}"
    exit 1
fi