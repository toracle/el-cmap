(require 'el-cmap-model)
(require 'el-cmap-repr)


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

(provide 'el-cmap-render)
