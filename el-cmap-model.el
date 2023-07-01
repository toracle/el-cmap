;; Model manipulation of ConceptMap

(require 'dash)


(defun cmap-model-node-id ()
  (let* ((digraph (plist-get *cmap-graph* :digraph))
         (nodes (plist-get digraph :nodes))
         (counter 1))
    (while (assoc (format "node_%d" counter)
                  nodes)
      (setq counter (+ counter 1)))
    (format "node_%d" counter)))


(defun cmap-model-node-label ()
  (let* ((digraph (plist-get *cmap-graph* :digraph))
         (nodes (plist-get digraph :nodes))
         (counter 1)
         )
    (while (-find '(lambda (node) (equal (plist-get (cdr node) :label)
                                         (format "New Node %d" counter)))
                  nodes)
      (setq counter (+ counter 1)))
    (format "New Node %d" counter)))


(defun cmap-model-edge-id ()
  (let* ((digraph (plist-get *cmap-graph* :digraph))
         (edges (plist-get digraph :edges))
         (counter 1))
    (while (assoc (format "edge_%d" counter)
                  edges)
      (setq counter (+ counter 1)))
    (format "edge_%d" counter)))


(defun cmap-override-plist (base-lst update-lst)
  (let ((new-base-lst (copy-sequence base-lst))
        (new-update-lst (copy-sequence update-lst)))
    (while new-update-lst
      (let ((key (pop new-update-lst))
            (val (pop new-update-lst)))
        (plist-put new-base-lst key val)))
    new-base-lst))


(defun cmap-model-node (&optional properties id)
  "Create a node. Can optionaly give PROPERTIES, especially for give node label. And ID also for debug use."
  (let ((node-id nil)
        (node-label nil)
        (default-node-property (list :label (cmap-model-node-label))))
   (if id (setq node-id id)
     (setq node-id (cmap-model-node-id)))
   (cons node-id (cmap-override-plist default-node-property properties))))


(defun cmap-model-edge (node-a-id node-b-id &optional properties id)
  (let ((edge-id nil))
    (if id (setq edge-id id)
      (setq edge-id (cmap-model-edge-id)))
    (cons edge-id (list node-a-id node-b-id properties))))


(defun cmap-model-init-graph ()
  (list :config nil :digraph (list :nodes nil :edges nil)))


(defun cmap-model-add-node (graph node)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (node-id (car node))
         (node-entry (assoc node-id nodes)))
    (if node-entry
        (setf (cdr node-entry) (cdr node))
      (plist-put digraph :nodes (add-to-list 'nodes node)))
    graph))


(defun cmap-model-get-node (graph node-id)
  (let* ((nodes (cmap-model-get-nodes graph))
         (node (assoc node-id nodes 'equal)))
    node))


(defun cmap-model-get-nodes (graph)
  (plist-get (plist-get graph :digraph) :nodes))


(defun cmap-model-add-edge (graph edge)
  (let* ((digraph (plist-get graph :digraph))
         (edges (plist-get digraph :edges))
         (node-a-id (cadr edge))
         (node-b-id (caddr edge)))

    (unless (cmap-model-get-node graph node-a-id)
      (cmap-model-add-node graph (cmap-model-node nil node-a-id)))
    (unless (cmap-model-get-node graph node-b-id)
      (cmap-model-add-node graph (cmap-model-node nil node-b-id)))

    (plist-put digraph :edges (add-to-list 'edges edge))
    graph))


(defun cmap-model-get-edges (graph)
  (plist-get (plist-get graph :digraph) :edges))


(defun cmap-model-get-directed-edges (graph focal-node-id &optional inward)
  "Returns edges of GRAPH, connected to a node FOCAL-NODE-ID. Outlink from FOCAL-NODE-ID if INWARD is nil. Inlink from FOCAL-NODE-ID if INWARD is t"
  (let* ((all-edges (cmap-model-get-edges graph))
         (node-func (if inward 'second 'first)))
    (-filter '(lambda (e)
                (equal focal-node-id (funcall node-func (cdr e))))
             all-edges)))


(defun cmap-model-remove-node (graph node-id)
  (let ((digraph (plist-get graph :digraph)))
    (plist-put digraph :nodes (assoc-delete-all node-id (cmap-model-get-nodes graph) 'equal)))

  (let ((edges (cmap-model-get-edges graph))
        (edge-id-list nil))
    (dolist (edge edges)
      (let ((src-node-id (cadr edge))
            (tgt-node-id (caddr edge)))
        (when (or (equal node-id src-node-id)
                  (equal node-id tgt-node-id))
          (add-to-list 'edge-id-list (car edge)))))

    (dolist (edge-id edge-id-list)
      (cmap-model-remove-edge graph edge-id))))


(defun cmap-model-remove-edge (graph edge-id)
  (let ((digraph (plist-get graph :digraph))
        (edges (cmap-model-get-edges graph)))
    (plist-put graph :digraph (plist-put digraph :edges (assoc-delete-all edge-id edges 'equal)))
    graph))


(defun cmap-model-get-node-labels (graph)
  "Returns a list of node labels of a GRAPH."
  (mapcar (lambda (node) (plist-get (cdr node) :label))
          (cmap-model-get-nodes graph)))


(defun cmap-model-get-node-id (graph label)
  "Returns the node id for a given node LABEL from a GRAPH."
  (car (seq-find (lambda (node) (equal label (plist-get (cdr node) :label)))
                 (cmap-model-get-nodes graph))))


(defun cmap-model-get-node-prop (node key)
  (plist-get (cdr node) key))


(defun cmap-model-get-edge-prop (edge key)
  (plist-get (cadddr edge) key))


(defun cmap-model-save (graph path)
  (with-temp-buffer
    (insert (format "%S" graph))
    (write-region nil nil path)
    path))


(defun cmap-model-load (path)
  "Read concept map data from PATH, and return concept map object."
  (with-temp-buffer
    (insert-file-contents path)
    (car (read-from-string (buffer-string)))))


(provide 'el-cmap-model)
