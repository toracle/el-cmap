(require 's)

(defun cmap-buffer-save ()
  (interactive)
  (unless *cmap-path*
    (setq-local *cmap-path* (read-file-name "Select a file to save: ")))
  (cmap-save *cmap-graph* *cmap-path*))


(defun cmap-buffer-load ()
  (interactive)
  (setq-local *cmap-path* (read-file-name "Select a file to load: "))
  (setq-local *cmap-graph* (cmap-load *cmap-path*))
  (cmap-buffer))


(defun cmap-buffer-add-node ()
  (interactive)
  (let ((node-label (read-string "Input node label: ")))
    (cmap-add-node *cmap-graph*
                   (cmap-node (list :label node-label)))
    (cmap-buffer)))


(defun cmap-buffer-add-edge ()
  (interactive)
  (read-string "Input neighbor edge: ")
  (read-string "Input edge label: "))


(defun cmap-buffer-select-focal-node ()
  (interactive)
  (let ((node-id (cmap-buffer-get-nodes)))
    (when node-id
      (setq-local *cmap-focal-node-id* node-id)
      (cmap-buffer))))


(defun cmap-buffer-export-graph ()
  (interactive)
  (let* ((dot-content (cmap-repr-digraph *cmap-graph*))
         (dot-path (cmap-path-with-ext *cmap-path* "el" "dot"))
         (image-path (cmap-path-with-ext *cmap-path* "el" "png")))
    (with-temp-buffer
      (insert dot-content)
      (write-region nil nil dot-path))
    (call-process "dot"
                  nil nil "*el-cmap-output*"
                  "-Kfdp" "-Tpng" dot-path (concat "-o" image-path))
    (cmap-mode-viewer image-path)))


(defun cmap-buffer-get-nodes ()
  (interactive)
  (let* ((nodes (cmap-get-nodes *cmap-graph*)))
    (helm :sources (helm-make-source "All nodes" 'helm-source-sync
                     :candidates (mapcar #'car nodes)))))


(defun cmap-buffer ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)

  (insert "[")
  (insert-button "Add Node"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-add-node)))
  (insert "] [")
  (insert-button "Add Edge"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-add-edge)))
  (insert "] [")
  (insert-button "Select Focal Node"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-select-focal-node)))
  (insert "]")

  (newline)
  (newline)

  (insert "Focal Node: " *cmap-focal-node-id*)
  (newline)
  (newline)
  (insert (format "%S" *cmap-graph*))
  
  (read-only-mode)
  (goto-char 1))


(defun cmap-path-with-ext (path old-ext new-ext)
  (let* ((path-parts (s-split "\\." path))
         (filename-parts (car (-drop-last 1 path-parts))))
    (when (equal old-ext (car (last path-parts)))
      (s-join "." (list filename-parts new-ext)))))
