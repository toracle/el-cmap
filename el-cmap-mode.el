;;; package --- el-cmap mode
;;; Commentary:
;;; some comment

;;; Code:

(require 'el-cmap)

(define-derived-mode el-cmap-mode fundamental-mode
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "TAB") 'forward-button)
  (local-set-key (kbd "g") 'cmap-buffer)
  
  (setq-local mode-name "el-cmap-mode")
  (setq-local cmap-path nil)
  (setq-local cmap-graph (cmap-init-graph))

  (cmap-buffer)
  )


(provide 'el-cmap-mode)
