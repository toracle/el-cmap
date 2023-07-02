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


(defun cmap-draw-node (node)
  (let ((node-label (cmap-model-get-node-prop node :label)))
    (insert "[")
    (insert-button node-label
                   'follow-link "\C-m"
                   'action '(lambda (button)
                              (setq-local *cmap-focal-node-id* (car (button-get button 'node)))
                              (cmap))
                   'node node)
    (insert "]")))


(defun cmap-draw-edge (edge &optional label-width)
  (let* ((edge-label (cmap-model-get-edge-prop edge :label))
         (edge-label-width (string-width edge-label))
         (width-diff (when label-width (- label-width edge-label-width)))
         (label-exist (and edge-label (not (equal edge-label "")))))
    (insert "----")
    (insert " ")
    (insert-button (format "%s" (if (and edge-label (not (equal edge-label "")))
                                    edge-label "..."))
                   'edge edge)
    (insert " ")

    (when width-diff
      (while (> width-diff 0)
        (insert "-")
        (setq-local width-diff (- width-diff 1))))
    (when label-exist
      ;; suppliment to anonymous labeled edge "...
      (insert "---"))
    (insert "----")))


(defun cmap-draw-edge-buttons (edge)
  (insert "[")
  (insert-button "X"
                 'follow-link t
                 'action '(lambda (button)
                            (cmap-model-remove-edge *cmap-graph* (car (button-get button 'edge)))
                            (cmap))
                 'edge edge)
  (insert "]"))


(defun cmap-edge-label-max-width (edges)
  (when edges
   (apply 'max (mapcar '(lambda (edge)
                          (string-width (cmap-model-get-edge-prop edge :label)))
                       edges))))


(defun cmap-graph ()
  (let* ((edges (copy-sequence (cmap-model-get-directed-edges
                                *cmap-graph*
                                *cmap-focal-node-id* t)))
         (edge-label-max-width (cmap-edge-label-max-width edges)))
    (while edges
      (let* ((edge (pop edges))
             (src-node-id (cadr edge))
             (node (cmap-model-get-node *cmap-graph* src-node-id)))
        (insert "    ")
        (insert " +")
        (cmap-draw-edge edge edge-label-max-width) (insert " ")
        (cmap-draw-node node)
        (newline))))

  (when *cmap-focal-node-id*
    (let* ((node (cmap-model-get-node *cmap-graph*
                                      *cmap-focal-node-id*)))
      (insert "     |") (newline)
      (insert "     |") (newline)
      (insert "     +----> ")
      (cmap-draw-node node)))

  (newline)

  (let* ((edges (copy-sequence (cmap-model-get-directed-edges
                               *cmap-graph*
                               *cmap-focal-node-id*)))
         (edge-label-max-width (cmap-edge-label-max-width edges)))
    (when edges
      (insert "             |")
      (newline)
      (insert "             |"))
    (newline)
    (while edges
      (let* ((edge (pop edges))
             (tgt-node-id (caddr edge))
             (node (cmap-model-get-node *cmap-graph* tgt-node-id)))
        (insert "             ")
        (insert "+") (cmap-draw-edge edge edge-label-max-width) (insert "> ")
        (cmap-draw-node node)
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
        (newline)))))

(provide 'el-cmap-render)
