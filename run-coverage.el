;;; run-coverage.el --- Run tests with coverage

;;; Commentary:
;; A simpler, more direct approach to running tests with coverage

;;; Code:

;; Set up load paths
(add-to-list 'load-path ".")
(add-to-list 'load-path "tests")

;; Load needed packages
(require 'ert)
(require 'dash)
(require 's)
(require 'cl-lib)
(require 'undercover)

;; Set up coverage for all el-cmap files
(setq undercover-force-coverage t)

;; Delete existing report to avoid merge errors
(when (file-exists-p "coverage/lcov.info")
  (delete-file "coverage/lcov.info"))

(undercover "el-cmap-*.el"
           (:exclude "*-test.el")
           (:report-format 'lcov)
           (:send-report nil)
           (:merge-report nil)
           (:report-file "coverage/lcov.info"))

;; Make sure we load the source files first
(load-file "el-cmap-model.el")
(load-file "el-cmap-model-pure.el")
(load-file "el-cmap-repr.el")
(load-file "el-cmap-buffer.el")
(load-file "el-cmap-render.el")
(load-file "el-cmap-mode.el")

;; Load test files
(require 'test-el-cmap-model)
(require 'test-el-cmap-repr)
(require 'test-el-cmap-buffer)
(require 'test-el-cmap-model-enhanced)
(require 'test-el-cmap-repr-enhanced)
(require 'test-el-cmap-buffer-enhanced)
(require 'test-el-cmap-model-pure)

;; Run all tests
(ert t)

;; Generate coverage report
(undercover-report)

;;; run-coverage.el ends here