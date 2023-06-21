;; Model manipulation of ConceptMap

(defun cmap-node-id ()
  (let* ((digraph (plist-get *cmap-graph* :digraph))
         (nodes (plist-get digraph :nodes))
         (counter 1))
    (while (assoc (format "node_%d" counter)
                  nodes)
      (setq counter (+ counter 1)))
    (format "node_%d" counter)))


(defun cmap-edge-id ()
  (let* ((digraph (plist-get *cmap-graph* :digraph))
         (edges (plist-get digraph :edges))
         (counter 1))
    (while (assoc (format "edge_%d" counter)
                  nodes)
      (setq counter (+ counter 1)))
    (format "edge_%d" counter)))


(defun cmap-node (&optional properties id)
  "Create a node. Can optionaly give PROPERTIES, especially for give node label. And ID also for debug use."
  (let ((node-id nil)
        (node-label nil))
   (if id (setq node-id id)
     (setq node-id (cmap-node-id)))
   (unless (plist-get properties :label)
     (plist-put properties :label (format "%s"(gensym "New Node "))))
   `(,node-id . ,properties)))


(defun cmap-edge (node-a-id node-b-id &optional properties id)
  (let ((edge-id nil))
    (if id (setq edge-id id)
      (setq edge-id (cmap-edge-id)))
    `(,edge-id . (,node-a-id ,node-b-id ,properties))))


(defun cmap-init-graph ()
  (list :config nil :digraph (list :nodes nil :edges nil)))


(defun cmap-add-node (graph node)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (node-id (car node))
         (node-entry (assoc node-id nodes)))
    (if node-entry
        (setf (cdr node-entry) (cdr node))
      (plist-put digraph :nodes (add-to-list 'nodes node)))
    graph))


(defun cmap-get-node (graph node-id)
  (let* ((digraph (plist-get graph :digarph))
         (nodes (plist-get digraph :nodes))
         (node (assoc node-id nodes)))
    node))


(defun cmap-get-nodes (graph)
  (plist-get (plist-get graph :digraph) :nodes))


(defun cmap-add-edge (graph edge)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (edges (plist-get digraph :edges))
         (edge-id (car edge))
         (node-a-id (cadr edge))
         (node-b-id (caddr edge)))
    
    (unless (cmap-get-node graph node-a-id)
      (cmap-add-node graph (cmap-node nil node-a-id)))
    (unless (cmap-get-node graph node-b-id)
      (cmap-add-node graph (cmap-node nil node-b-id)))

    (plist-put digraph :edges (add-to-list 'edges edge))
    graph))


(defun cmap-save (graph path)
  (with-temp-buffer
    (insert (format "%S" graph))
    (write-region nil nil path)
    path))


(defun cmap-load (path)
  "Read concept map data from PATH, and return concept map object."
  (with-temp-buffer
    (insert-file-contents path)
    (car (read-from-string (buffer-string)))))


(provide 'el-cmap-model)
