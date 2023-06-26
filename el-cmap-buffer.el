(require 's)

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
                  "-Kfdp" "-Tpng" dot-path (concat "-o" image-path))
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


(defun cmap-toolbar ()
  (insert "[")
  (insert-button "New Graph"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-new-graph)))

  (insert "] [")
  (insert-button "Add Node"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-add-node)))
  (insert "] [")
  (insert-button "Add Edge"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-add-edge)))
  (insert "] [")
  (insert-button "Select Focal Node"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-select-focal-node)))
  (insert "]"))


(defun cmap-graph ()
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
                                  (cmap))
                       'node node)
        (insert "] ----")
        (when edge-label
          (insert " ")
          (insert-button (propertize (format "%s" edge-label) 'edge edge))
          (insert " "))
        (insert "----> ")
        (insert "[")
        (insert-button "X"
                       'follow-link t
                       'action '(lambda (button)
                                  (cmap-model-remove-edge *cmap-graph* (car (button-get button 'edge)))
                                  (cmap))
                       'edge edge)
        (insert "]")
        (newline))))

  (newline)

  (when *cmap-focal-node-id*
    (let* ((node (cmap-model-get-node *cmap-graph*
                                      *cmap-focal-node-id*))
           (node-label (cmap-model-get-node-prop node :label)))

      (insert "        ----------> [")
      (insert-button (propertize (format "%s" node-label) 'node node))
      (insert "]")))

  (newline)

  (let ((edges (copy-sequence (cmap-model-get-directed-edges
                               *cmap-graph*
                               *cmap-focal-node-id*))))
    (insert "                     ")
    (insert "|")
    (newline)
    (while edges
      (let* ((edge (pop edges))
             (tgt-node-id (caddr edge))
             (node (cmap-model-get-node *cmap-graph* tgt-node-id))
             (node-label (cmap-model-get-node-prop node :label))
             (edge-label (cmap-model-get-edge-prop edge :label)))
        (insert "                     ")
        (insert "+---")
        (when edge-label
          (insert " ")
          (insert-button (format "%s" edge-label))
          (insert " "))
        (insert "----> [")
        (insert-button node-label
                       'follow-link "\C-m"
                       'action '(lambda (button)
                                  (setq-local *cmap-focal-node-id* (car (button-get button 'node)))
                                  (cmap))
                       'node node)
        (insert "]")
        (insert " [")
        (insert-button "X"
                       'follow-link t
                       'action '(lambda (button)
                                  (cmap-model-remove-edge *cmap-graph* (car (button-get button 'edge)))
                                  (cmap))
                       'edge edge)
        (insert "]")

        (newline)))))


(defun cmap-node-list ()
  (let ((nodes (copy-sequence (cmap-model-get-nodes *cmap-graph*))))
    (while nodes
      (let* ((node (pop nodes))
             (node-label (cmap-model-get-node-prop node :label)))
        (insert " * ")
        (insert-button node-label
                       'follow-link "\C-m"
                       'action '(lambda (button)
                                  (setq-local *cmap-focal-node-id* (car (button-get button 'node)))
                                  (cmap))
                       'node node)
        (insert " [")
        (insert-button "X"
                       'follow-link t
                       'action '(lambda (button)
                                  (let ((node-id (car (button-get button 'node))))
                                    (cmap-model-remove-node *cmap-graph* node-id)
                                    (when (equal node-id *cmap-focal-node-id*)
                                      (setq-local *cmap-focal-node-id* (car (first (cmap-model-get-nodes *cmap-graph*)))))
                                    (cmap)))
                       'node node)
        (insert "]")
        (newline)))))


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
