;;; el-cmap-coverage.el --- Coverage setup for el-cmap

;;; Commentary:
;; Set up undercover.el for code coverage reporting

;;; Code:

(require 'undercover)

;; Debug settings
(setq undercover-force-coverage t)

;; List all files explicitly to ensure coverage
(let ((source-files '("el-cmap-model.el"
                       "el-cmap-model-pure.el"
                       "el-cmap-repr.el"
                       "el-cmap-buffer.el"
                       "el-cmap-render.el"
                       "el-cmap-mode.el")))
  
  ;; First apply undercover to each file individually
  (dolist (file source-files)
    (message "Setting up coverage for %s" file))
  
  ;; Then use the wildcard approach as a fallback
  (undercover source-files
              ;; Specify report format (we'll use lcov for HTML reports)
              (:report-format 'lcov)
              ;; Don't send reports to external service
              (:send-report nil)
              ;; Set up report file
              (:report-file "coverage/lcov.info")))

;; This is important: when loading files for coverage analysis,
;; display a message to confirm undercover is working
(message "Coverage tracking enabled for el-cmap files")

(provide 'el-cmap-coverage)
;;; el-cmap-coverage.el ends here