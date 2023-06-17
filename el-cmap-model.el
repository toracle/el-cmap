(defun cmap-node (id &optional label)
  `(,id . ,label))


(defun cmap-init-graph ()
  (list :config nil :digraph (list :nodes nil :edges nil)))


(defun cmap-add-node (graph node)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes)))
    (plist-put digraph :nodes (add-to-list 'nodes node))
    graph))


(defun cmap-add-edge (graph edge)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (edges (plist-get digraph :edges)))
    (cmap-add-node graph (car edge))
    (cmap-add-node graph (car (cdr edge)))
    (plist-put digraph :edges (add-to-list 'edges edge))
    graph))


(provide 'el-cmap-model)
