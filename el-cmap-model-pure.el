;; Pure functional versions of el-cmap-model functions with reduced side effects
;; These functions can be used as drop-in replacements for testing or to gradually
;; refactor the codebase to be more functional and testable.

(require 'dash)

;;; ID Generation Functions

(defun cmap-model-pure-node-id (graph)
  "Generate a unique node ID for GRAPH without side effects."
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (counter 1))
    (while (assoc (format "node_%d" counter) nodes)
      (setq counter (+ counter 1)))
    (format "node_%d" counter)))

(defun cmap-model-pure-node-label (graph)
  "Generate a unique node label for GRAPH without side effects."
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (counter 1))
    (while (-find (lambda (node) (equal (plist-get (cdr node) :label)
                                       (format "New Node %d" counter)))
                nodes)
      (setq counter (+ counter 1)))
    (format "New Node %d" counter)))

(defun cmap-model-pure-edge-id (graph)
  "Generate a unique edge ID for GRAPH without side effects."
  (let* ((digraph (plist-get graph :digraph))
         (edges (plist-get digraph :edges))
         (counter 1))
    (while (assoc (format "edge_%d" counter) edges)
      (setq counter (+ counter 1)))
    (format "edge_%d" counter)))

;;; Node and Edge Creation

(defun cmap-model-pure-node (graph &optional properties id)
  "Create a node for GRAPH. Can optionally give PROPERTIES and ID."
  (let ((node-id nil)
        (default-node-property (list :label (cmap-model-pure-node-label graph))))
    (if id (setq node-id id)
      (setq node-id (cmap-model-pure-node-id graph)))
    (cons node-id (cmap-override-plist default-node-property properties))))

(defun cmap-model-pure-edge (graph from-node-id to-node-id &optional properties id)
  "Create an edge for GRAPH from FROM-NODE-ID to TO-NODE-ID with PROPERTIES and ID."
  (let ((edge-id nil))
    (if id (setq edge-id id)
      (setq edge-id (cmap-model-pure-edge-id graph)))
    (cons edge-id (list from-node-id to-node-id properties))))

;;; Graph Operations

(defun cmap-model-pure-add-node (graph node)
  "Add NODE to GRAPH and return the updated graph."
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (node-id (car node))
         (node-entry (assoc node-id nodes))
         (result-graph (copy-tree graph)))
    (let ((result-digraph (plist-get result-graph :digraph)))
      (if node-entry
          (setf (cdr (assoc node-id (plist-get result-digraph :nodes))) (cdr node))
        (plist-put result-digraph :nodes (cons node (plist-get result-digraph :nodes))))
      result-graph)))

(defun cmap-model-pure-add-edge (graph edge)
  "Add EDGE to GRAPH and return the updated graph."
  (let* ((node-a-id (cadr edge))
         (node-b-id (caddr edge))
         (result-graph (copy-tree graph)))
    
    ;; Add nodes if they don't exist
    (unless (cmap-model-get-node result-graph node-a-id)
      (setq result-graph (cmap-model-pure-add-node 
                         result-graph 
                         (cmap-model-pure-node result-graph nil node-a-id))))
    
    (unless (cmap-model-get-node result-graph node-b-id)
      (setq result-graph (cmap-model-pure-add-node 
                         result-graph 
                         (cmap-model-pure-node result-graph nil node-b-id))))
    
    ;; Add the edge
    (let ((result-digraph (plist-get result-graph :digraph)))
      (plist-put result-digraph :edges 
                (cons edge (plist-get result-digraph :edges)))
      result-graph)))

(defun cmap-model-pure-remove-node (graph node-id)
  "Remove node with NODE-ID from GRAPH and return the updated graph."
  (let ((result-graph (copy-tree graph))
        (result-digraph nil))
    
    ;; Remove the node
    (setq result-digraph (plist-get result-graph :digraph))
    (plist-put result-digraph :nodes 
              (assoc-delete-all node-id (plist-get result-digraph :nodes) 'equal))
    
    ;; Find edges connected to this node
    (let ((edges (plist-get result-digraph :edges))
          (edge-ids-to-remove nil))
      (dolist (edge edges)
        (let ((src-node-id (cadr edge))
              (tgt-node-id (caddr edge)))
          (when (or (equal node-id src-node-id)
                    (equal node-id tgt-node-id))
            (push (car edge) edge-ids-to-remove))))
      
      ;; Remove each connected edge
      (dolist (edge-id edge-ids-to-remove)
        (setq result-graph (cmap-model-pure-remove-edge result-graph edge-id))))
    
    result-graph))

(defun cmap-model-pure-remove-edge (graph edge-id)
  "Remove edge with EDGE-ID from GRAPH and return the updated graph."
  (let ((result-graph (copy-tree graph))
        (result-digraph nil))
    (setq result-digraph (plist-get result-graph :digraph))
    (plist-put result-digraph :edges 
              (assoc-delete-all edge-id (plist-get result-digraph :edges) 'equal))
    result-graph))

;;; Node/Edge Property Operations

(defun cmap-model-pure-set-node-prop (graph node-id key val)
  "Set property KEY to VAL for node with NODE-ID in GRAPH."
  (let* ((result-graph (copy-tree graph))
         (node (cmap-model-get-node result-graph node-id)))
    (when node
      (let ((updated-node (copy-tree node)))
        (plist-put (cdr updated-node) key val)
        (setq result-graph (cmap-model-pure-add-node result-graph updated-node))))
    result-graph))

(defun cmap-model-pure-set-edge-prop (graph edge-id key val)
  "Set property KEY to VAL for edge with EDGE-ID in GRAPH."
  (let* ((result-graph (copy-tree graph))
         (digraph (plist-get result-graph :digraph))
         (edges (plist-get digraph :edges))
         (edge (assoc edge-id edges 'equal)))
    (when edge
      (let* ((updated-edge (copy-tree edge))
             (props (cadddr updated-edge)))
        ;; Update property
        (if props
            ;; If props exists, create a new plist with updated value
            (setf (cadddr updated-edge) (plist-put (copy-sequence props) key val))
          ;; If no props, create a new plist
          (setf (cadddr updated-edge) (list key val)))
        
        ;; Update edge in graph
        (plist-put digraph :edges 
                  (cons updated-edge (assoc-delete-all edge-id edges 'equal)))))
    result-graph))

;;; File Operations

(defun cmap-model-pure-save (graph path)
  "Save GRAPH to PATH and return the path."
  (cmap-model-save graph path)
  path)

(defun cmap-model-pure-load (path)
  "Load a graph from PATH and return it."
  (cmap-model-load path))

(provide 'el-cmap-model-pure)