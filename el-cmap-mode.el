;;; package --- el-cmap mode
;;; Commentary:
;;; some comment

;;; Code:

(require 'el-cmap)

(defvar *cmap-graph* (cmap-init-graph))
(defvar *cmap-path* nil)
(defvar *cmap-focal-node-id* nil)


(define-derived-mode el-cmap-mode fundamental-mode
  (setq-local mode-name "el-cmap-mode")

  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button)
  (local-set-key (kbd "g") 'cmap-buffer)
  (local-set-key (kbd "n") 'cmap-buffer-add-node)
  (local-set-key (kbd "e") 'cmap-buffer-add-edge)
  (local-set-key (kbd "f") 'cmap-buffer-select-focal-node)
  (local-set-key (kbd "v") 'cmap-buffer-export-graph)
  (local-set-key (kbd "C-x C-s") 'cmap-buffer-save)
  (local-set-key (kbd "C-x C-f") 'cmap-buffer-load))


(define-derived-mode el-cmap-viewer-mode fundamental-mode
  (setq-local mode-name "el-cmap-viewer-mdoe")

  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "g") 'cmap-buffer))


(defun cmap-mode-main ()
  (interactive)
  (with-current-buffer (get-buffer-create "*el-cmap-mode*")
    (switch-to-buffer "*el-cmap-mode*")
    (el-cmap-mode)
    (cmap-buffer)))


(defun cmap-mode-viewer (image-path)
  (interactive)
  (with-current-buffer (get-buffer-create "*el-cmap-viewer*")
    (pop-to-buffer "*el-cmap-viewer*")
    (read-only-mode -1)
    (el-cmap-viewer-mode)
    (auto-image-file-mode t)
    (insert-image-file image-path nil nil nil t)
    (read-only-mode t)))


(provide 'el-cmap-mode)