;; Setup load paths
(add-to-list 'load-path ".")
(add-to-list 'load-path "tests")

;; Load required libraries
(require 'ert)
(require 'dash)
(require 's)
(require 'cl-lib)

;; Set batch mode for consistent behavior in tests
(setq noninteractive t)

;; Load coverage setup if available
(when (locate-library "el-cmap-coverage")
  (require 'el-cmap-coverage)
  (message "Code coverage enabled via undercover"))

;; Load basic test modules
(require 'test-el-cmap-model)
(require 'test-el-cmap-repr)
(require 'test-el-cmap-buffer)

;; Load enhanced test modules with reduced side effects
(require 'test-el-cmap-model-enhanced)
(require 'test-el-cmap-repr-enhanced)
(require 'test-el-cmap-buffer-enhanced)

;; Load pure functional model tests
(require 'test-el-cmap-model-pure)

;; Run all tests
(ert t)

;; Output coverage summary after tests complete
(when (featurep 'undercover)
  (message "Generating coverage report...")
  (undercover-report)
  (when (file-exists-p "coverage/lcov.info")
    (message "Coverage report generated in coverage/lcov.info")))
