(require 's)

(defun cmap-buffer-save ()
  (interactive)
  (unless *cmap-path*
    (setq-local *cmap-path* (read-file-name "Select a file to save: ")))
  (cmap-model-save *cmap-graph* *cmap-path*))


(defun cmap-buffer-load ()
  (interactive)
  (setq-local *cmap-path* (read-file-name "Select a file to load: "))
  (setq-local *cmap-graph* (cmap-model-load *cmap-path*))
  (cmap-buffer))


(defun cmap-buffer-add-node ()
  (interactive)
  (let* ((node-label (read-string "Input node label: "))
         (node (cmap-model-add-node *cmap-graph*
                                    (cmap-model-node (list :label node-label)))))

    (unless *cmap-focal-node-id*
      (setq-local *cmap-focal-node-id* (car node)))
    (cmap-buffer)))


(defun cmap-buffer-add-edge-outward ()
  (interactive)
  (cmap-buffer-add-edge))


(defun cmap-buffer-add-edge-inward ()
  (interactive)
  (cmap-buffer-add-edge t))


(defun cmap-buffer-add-edge (&optional inward)
  (interactive)
  (let* ((node-id (cmap-buffer-get-node))
         (edge-label (read-string "Input edge label: "))
         (edge nil))

    (if inward
        (setq edge (cmap-model-edge node-id *cmap-focal-node-id*
                              (list :label edge-label)))
      (setq edge (cmap-model-edge *cmap-focal-node-id* node-id
                            (list :label edge-label))))

    (cmap-model-add-edge *cmap-graph* edge)
    (cmap-buffer)))


(defun cmap-buffer-select-focal-node ()
  (interactive)
  (let ((node-id (cmap-buffer-get-node)))
    (when node-id
      (setq-local *cmap-focal-node-id* node-id)
      (cmap-buffer))))


(defun cmap-buffer-export-graph ()
  (interactive)
  (unless *cmap-path*
    (cmap-buffer-save))
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


;; define a function that invokes ido-completing-read with the node labels as candidates
;; and a lambda function that prints the node id as the action
(defun cmap-buffer-get-node ()
  (interactive)
  (setq-local ido-enable-flex-matching t)
  (let ((label (ido-completing-read "Select node: " (cmap-model-get-node-labels *cmap-graph*))))
    (message "Selected node id: %s" (cmap-model-get-node-id *cmap-graph* label))
    (cmap-model-get-node-id *cmap-graph* label)))


(defun cmap-buffer-new-graph ()
  (interactive)
  (setq-local *cmap-graph* (cmap-model-init-graph))
  (setq-local *cmap-path* nil)
  (cmap-buffer))


(defun cmap-buffer-toolbar ()
  (insert "[")
  (insert-button "New Graph"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-new-graph)))

  (insert "] [")
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
  (insert "]"))


(defun cmap-buffer-graph ()
  (let ((edges (copy-sequence (cmap-model-get-directed-edges
                               *cmap-graph*
                               *cmap-focal-node-id* t))))
    (while edges
      (let* ((edge (pop edges))
             (src-node-id (cadr edge))
             (node (cmap-model-get-node *cmap-graph* src-node-id))
             (node-label (cmap-model-get-node-prop node :label))
             (edge-label (cmap-model-get-edge-prop edge :label)))
        (insert "    [")
        (insert-button (propertize node-label 'node node)
                       'follow-link "\C-m"
                       'action '(lambda (button)
                                  (setq-local *cmap-focal-node-id* (car (button-get button 'node)))
                                  (cmap-buffer))
                       'node node)
        (insert "] ----")
        (when edge-label
          (insert " ")
          (insert-button (propertize (format "%s" edge-label) 'edge edge))
          (insert " "))
        (insert "---->")
        (newline))))

  (newline)

  (when *cmap-focal-node-id*
    (let* ((node (cmap-model-get-node *cmap-graph*
                                      *cmap-focal-node-id*))
           (node-label (cmap-model-get-node-prop node :label)))

      (insert "    ----------> [")
      (insert-button (propertize (format "%s" node-label) 'node node))
      (insert "] ---------->")))

  (newline)
  (newline)

  (let ((edges (copy-sequence (cmap-model-get-directed-edges
                               *cmap-graph*
                               *cmap-focal-node-id*))))
    (while edges
      (let* ((edge (pop edges))
             (tgt-node-id (caddr edge))
             (node (cmap-model-get-node *cmap-graph* tgt-node-id))
             (node-label (cmap-model-get-node-prop node :label))
             (edge-label (cmap-model-get-edge-prop edge :label)))
        (insert "    ")
        (insert "----")
        (when edge-label
          (insert " ")
          (insert-button (format "%s" edge-label))
          (insert " "))
        (insert "----> [")
        (insert-button node-label
                       'follow-link "\C-m"
                       'action '(lambda (button)
                                  (setq-local *cmap-focal-node-id* (car (button-get button 'node)))
                                  (cmap-buffer))
                       'node node)
        (insert "]")

        (newline)))))


(defun cmap-buffer-node-list ()
  (let ((nodes (copy-sequence (cmap-model-get-nodes *cmap-graph*))))
    (while nodes
      (let ((node (pop nodes)))
        (insert " * ")
        (insert (cmap-model-get-node-prop node :label))
        (newline)))))


(defun cmap-buffer-toggle-toolbar ()
  (interactive)
  (if *cmap-toolbar-visible*
      (setq-local *cmap-toolbar-visible* nil)
    (setq-local *cmap-toolbar-visible* t))
  (cmap-buffer))


(defun cmap-buffer ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)

  (when *cmap-toolbar-visible*
    (cmap-buffer-toolbar)
    (newline)
    (newline))

  (insert "File: ")
  (insert (format "%s" *cmap-path*)) (newline)
  (newline)

  (insert "---") (newline)
  (insert "Local Graph:") (newline)
  (newline)

  (cmap-buffer-graph)

  (newline)
  (newline)

  (insert "---") (newline)
  (insert "Nodes:") (newline)
  (cmap-buffer-node-list)

  (read-only-mode)
  (goto-char 1))


(defun cmap-path-with-ext (path old-ext new-ext)
  (let* ((path-parts (s-split "\\." path))
         (filename-parts (car (-drop-last 1 path-parts))))
    (when (equal old-ext (car (last path-parts)))
      (s-join "." (list filename-parts new-ext)))))
