(require 's)
(require 'el-cmap-model)
(require 'el-cmap-render)


(defun cmap-save ()
  (interactive)
  (unless *cmap-path*
    (setq-local *cmap-path* (read-file-name "Select a file to save: ")))
  (cmap-model-save *cmap-graph* *cmap-path*))


(defun cmap-load ()
  (interactive)
  (setq-local *cmap-path* (read-file-name "Select a file to load: "))
  (setq-local *cmap-graph* (cmap-model-load *cmap-path*))
  (cmap))


(defun cmap-add-node ()
  (interactive)
  (let* ((node-label (read-string "Input node label: "))
         (node (cmap-model-add-node *cmap-graph*
                                    (cmap-model-node (list :label node-label)))))

    (unless *cmap-focal-node-id*
      (setq-local *cmap-focal-node-id* (car node)))
    (cmap)))


(defun cmap-add-edge-outward ()
  (interactive)
  (cmap-add-edge))


(defun cmap-add-edge-inward ()
  (interactive)
  (cmap-add-edge t))


(defun cmap-add-edge (&optional inward)
  (interactive)
  (let* ((node-id (cmap-select-node))
         (edge-label (read-string "Input edge label: "))
         (edge nil))

    (if inward
        (setq edge (cmap-model-edge node-id *cmap-focal-node-id*
                              (list :label edge-label)))
      (setq edge (cmap-model-edge *cmap-focal-node-id* node-id
                            (list :label edge-label))))

    (cmap-model-add-edge *cmap-graph* edge)
    (cmap)))


(defun cmap-select-focal-node ()
  (interactive)
  (let ((node-id (cmap-select-node)))
    (when node-id
      (setq-local *cmap-focal-node-id* node-id)
      (cmap))))


(defun cmap-delete-pos-at ()
  (interactive)
  (let* ((button (button-at (point)))
         (node (when button (button-get button 'node)))
         (edge (when button (button-get button 'edge))))
    (when button
      (cond
       (node
        (when (y-or-n-p "Really delete the node?")
          (let ((node-id (car node)))
            (cmap-model-remove-node *cmap-graph* node-id)
            (when (equal node-id *cmap-focal-node-id*)
              (setq-local *cmap-focal-node-id*
                          (car (first (cmap-model-get-nodes *cmap-graph*)))))
           (cmap))))
       (edge
        (when (y-or-n-p "Really delete the edge?")
          (cmap-model-remove-edge *cmap-graph* (car edge))
          (cmap)))))))


(defun cmap-rename-pos-at ()
  (interactive)
  (let* ((button (button-at (point)))
         (node (when button (button-get button 'node)))
         (edge (when button (button-get button 'edge))))
    (when button
      (cond
       (node
        (let ((node-id (car node))
              (new-node-label (read-string "Rename the node label to: ")))
          (cmap-model-set-node-prop node :label new-node-label)
          (cmap)))
       (edge
        (let ((edge-id (car edge))
              (new-edge-label (read-string "Rename the edge label to: ")))
          (cmap-model-set-edge-prop edge :label new-edge-label)
          (cmap)))))))


(defun cmap-export-graph ()
  (interactive)
  (unless *cmap-path*
    (cmap-save))
  (let* ((dot-content (cmap-repr-digraph *cmap-graph*))
         (dot-path (cmap-path-with-ext *cmap-path* "el" "dot"))
         (image-path (cmap-path-with-ext *cmap-path* "el" "png")))
    (with-temp-buffer
      (insert dot-content)
      (write-region nil nil dot-path))
    (call-process "dot"
                  nil nil "*el-cmap-output*"
                  "-Kdot" "-Tpng" dot-path (concat "-o" image-path))
    (cmap-mode-viewer image-path)))


;; define a function that invokes ido-completing-read with the node labels as candidates
;; and a lambda function that prints the node id as the action
(defun cmap-select-node ()
  (interactive)
  (setq-local ido-enable-flex-matching t)
  (let ((label (ido-completing-read "Select node: " (cmap-model-get-node-labels *cmap-graph*))))
    (message "Selected node id: %s" (cmap-model-get-node-id *cmap-graph* label))
    (cmap-model-get-node-id *cmap-graph* label)))


(defun cmap-new-graph ()
  (interactive)
  (setq-local *cmap-graph* (cmap-model-init-graph))
  (setq-local *cmap-path* nil)
  (cmap))


(defun cmap-toggle-toolbar ()
  (interactive)
  (if *cmap-toolbar-visible*
      (setq-local *cmap-toolbar-visible* nil)
    (setq-local *cmap-toolbar-visible* t))
  (cmap))


(defun cmap ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)

  (when *cmap-toolbar-visible*
    (cmap-toolbar)
    (newline)
    (newline))

  (insert "File: ")
  (insert (format "%s" *cmap-path*)) (newline)
  (newline)

  (insert "---") (newline)
  (insert "Local Graph:") (newline)
  (newline)

  (cmap-graph)

  (newline)
  (newline)

  (insert "---") (newline)
  (insert "Nodes:") (newline)
  (cmap-node-list)

  (read-only-mode)
  (goto-char 1))


(defun cmap-path-with-ext (path old-ext new-ext)
  (let* ((path-parts (s-split "\\." path))
         (filename-parts (car (-drop-last 1 path-parts))))
    (when (equal old-ext (car (last path-parts)))
      (s-join "." (list filename-parts new-ext)))))

(provide 'el-cmap-buffer)
