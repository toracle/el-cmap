;;; show-coverage.el --- Display coverage info in Emacs

;;; Commentary:
;; This file sets up coverage overlay for el-cmap

;;; Code:

(require 'cov)

;; Configure cov-mode for lcov format
(setq cov-coverage-file-paths '("coverage/lcov.info"))
(setq cov-high-threshold 85)
(setq cov-med-threshold 65)

;; Enable coverage overlays for all el-cmap files
(defun el-cmap-coverage-show ()
  "Show code coverage for el-cmap files."
  (interactive)
  (unless (file-exists-p "coverage/lcov.info")
    (user-error "No coverage information found. Run tests with coverage first"))
  
  ;; Apply to already open buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (string-match "el-cmap-.*\\.el$" buffer-file-name))
        (cov-mode 1)
        (message "Enabled coverage overlay in %s" (buffer-name)))))
  
  ;; Add hook for future buffers
  (add-hook 'find-file-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-match "el-cmap-.*\\.el$" buffer-file-name))
                (cov-mode 1))))
  
  ;; Print coverage summary if lcov is available
  (when (executable-find "lcov")
    (shell-command "lcov --summary coverage/lcov.info"))
  
  (message "Coverage overlay enabled for el-cmap files"))

;; Function to generate HTML report from lcov data
(defun el-cmap-coverage-html ()
  "Generate HTML coverage report from lcov data."
  (interactive)
  (unless (file-exists-p "coverage/lcov.info")
    (user-error "No coverage information found. Run tests with coverage first"))
  
  (unless (executable-find "genhtml")
    (user-error "genhtml not found. Install lcov package first"))
  
  (let ((default-directory (file-name-directory (directory-file-name default-directory))))
    (shell-command "mkdir -p coverage/html")
    (shell-command "genhtml coverage/lcov.info --output-directory coverage/html")
    (message "HTML coverage report generated in coverage/html/index.html")
    
    ;; Open the report in browser if possible
    (let ((report-file (expand-file-name "coverage/html/index.html")))
      (when (and (file-exists-p report-file)
                 (y-or-n-p "Open coverage report in browser? "))
        (browse-url report-file)))))

(provide 'show-coverage)
;;; show-coverage.el ends here