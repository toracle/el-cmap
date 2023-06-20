;;; package --- el-cmap mode
;;; Commentary:
;;; some comment

;;; Code:

(require 'el-cmap)

(defvar *cmap-graph* (cmap-init-graph))
(defvar *cmap-path* nil)

(define-derived-mode el-cmap-mode fundamental-mode
  (setq-local mode-name "el-cmap-mode")

  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button)
  (local-set-key (kbd "g") 'cmap-buffer)
  (local-set-key (kbd "n") 'cmap-buffer-add-node)
  (local-set-key (kbd "e") 'cmap-buffer-add-edge)
  (local-set-key (kbd "f") 'cmap-buffer-select-focal-node)
  (local-set-key (kbd "C-x C-s") 'cmap-buffer-save)
  (local-set-key (kbd "C-x C-f") 'cmap-buffer-load)
  (local-set-key (kbd "C-c C-e") 'cmap-buffer-export-graph))


(defun cmap-mode-main ()
  (interactive)
  (with-current-buffer (get-buffer-create "*el-cmap-mode*")
    (switch-to-buffer "*el-cmap-mode*")
    (el-cmap-mode)
    (cmap-buffer)))


(provide 'el-cmap-mode)
