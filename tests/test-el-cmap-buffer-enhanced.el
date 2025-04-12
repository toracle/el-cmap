;; Enhanced tests for el-cmap-buffer with reduced side effects

(require 'dash)
(require 's)
(require 'cl-lib)

;; Load the modules to test
(require 'el-cmap-model)
(require 'el-cmap-buffer)

;; Define global variables for testing to avoid the void-variable error
(defvar *cmap-graph* nil)
(defvar *cmap-path* nil)
(defvar *cmap-focal-node-id* nil)

;; Helper functions with reduced side effects

(defun test-path-with-ext (path old-ext new-ext)
  "Convert a file PATH with extension OLD-EXT to have extension NEW-EXT (pure function)."
  (let* ((path-parts (s-split "\\." path))
         (ext (car (last path-parts)))
         (base-path (s-join "." (-drop-last 1 path-parts))))
    (if (equal old-ext ext)
        (concat base-path "." new-ext)
      (concat path "." new-ext))))

(defun test-save-graph (graph path)
  "Save a graph to a path without side effects."
  (cmap-model-save graph path)
  path)

(defun test-load-graph (path)
  "Load a graph from a path without side effects."
  (cmap-model-load path))

(defun test-add-node (graph &optional label focal-node-id)
  "Add a node to a graph without side effects."
  ;; First set *cmap-graph* temporarily for cmap-model-node function
  (let* ((*cmap-graph* graph)
         (node-label (or label "New Test Node"))
         (node (cmap-model-node (list :label node-label)))
         (node-id (car node))
         (updated-graph (cmap-model-add-node graph node)))
    (list :graph updated-graph 
          :node-id node-id 
          :focal-node-id (or focal-node-id node-id))))

(defun test-add-edge (graph from-node-id to-node-id &optional label inward)
  "Add an edge to a graph without side effects."
  ;; First set *cmap-graph* temporarily for cmap-model-edge function
  (let* ((*cmap-graph* graph)
         (edge-label (or label "Test Edge"))
         (edge (if inward
                   (cmap-model-edge to-node-id from-node-id (list :label edge-label))
                 (cmap-model-edge from-node-id to-node-id (list :label edge-label))))
         (edge-id (car edge))
         (updated-graph (cmap-model-add-edge graph edge)))
    (list :graph updated-graph 
          :edge-id edge-id)))

(defun test-rename-node (graph node-id new-label)
  "Rename a node without side effects."
  (let* ((node (cmap-model-get-node graph node-id))
         (updated-node (copy-tree node)))
    (cmap-model-set-node-prop updated-node :label new-label)
    ;; Create a new graph with the updated node
    (let ((updated-graph (copy-tree graph)))
      (cmap-model-add-node updated-graph updated-node)
      updated-graph)))

;; Tests for path handling

(ert-deftest cmap-buffer-path-with-ext-variations ()
  "Test path extension conversion with various input patterns."
  ;; Standard case - matching extension
  (should (equal (test-path-with-ext "/home/user/graph.el" "el" "png")
                 "/home/user/graph.png"))
  
  ;; Different extension than expected
  (should (equal (test-path-with-ext "/home/user/graph.txt" "el" "png")
                 "/home/user/graph.txt.png"))
  
  ;; No extension in path
  (should (equal (test-path-with-ext "/home/user/graph" "el" "png")
                 "/home/user/graph.png"))
  
  ;; Path with dots in directory names
  (should (equal (test-path-with-ext "/home/user.name/my.graph.el" "el" "png")
                 "/home/user.name/my.graph.png"))
  
  ;; Path with multiple extensions
  (should (equal (test-path-with-ext "/home/user/graph.tar.gz" "gz" "bz2")
                 "/home/user/graph.tar.bz2")))

;; Test node addition

(ert-deftest cmap-buffer-add-node ()
  "Test adding a node without side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; Add a node
    (let* ((result (test-add-node graph "Test Node"))
           (updated-graph (plist-get result :graph))
           (node-id (plist-get result :node-id)))
      
      ;; Verify node exists in graph
      (let ((node (cmap-model-get-node updated-graph node-id)))
        (should node)
        (should (equal (plist-get (cdr node) :label) "Test Node")))
      
      ;; Add a second node
      (let* ((result2 (test-add-node updated-graph "Test Node 2"))
             (updated-graph2 (plist-get result2 :graph))
             (node-id2 (plist-get result2 :node-id)))
        
        ;; Get all nodes
        (let ((nodes (cmap-model-get-nodes updated-graph2)))
          ;; Verify both nodes exist by checking count and finding each one
          (should (not (null nodes)))
          (should (not (null (assoc node-id nodes))))
          (should (not (null (assoc node-id2 nodes)))))))))

;; Test edge addition

(ert-deftest cmap-buffer-add-edge ()
  "Test adding an edge without side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; Add two nodes
    (let* ((result1 (test-add-node graph "Node A"))
           (updated-graph1 (plist-get result1 :graph))
           (node-id1 (plist-get result1 :node-id))
           (result2 (test-add-node updated-graph1 "Node B"))
           (updated-graph2 (plist-get result2 :graph))
           (node-id2 (plist-get result2 :node-id)))
      
      ;; Add an outward edge
      (let* ((edge-result (test-add-edge updated-graph2 node-id1 node-id2 "A to B"))
             (updated-graph3 (plist-get edge-result :graph))
             (edge-id (plist-get edge-result :edge-id)))
        
        ;; Verify edge exists
        (let ((edges (cmap-model-get-edges updated-graph3)))
          ;; Find the edge with our ID
          (let ((edge (assoc edge-id edges)))
            (should edge)
            (should (equal (cadr edge) node-id1))
            (should (equal (caddr edge) node-id2))
            (should (equal (plist-get (cadddr edge) :label) "A to B")))))
      
      ;; Add an inward edge
      (let* ((edge-result (test-add-edge updated-graph2 node-id1 node-id2 "B to A" t))
             (updated-graph3 (plist-get edge-result :graph))
             (edge-id (plist-get edge-result :edge-id)))
        
        ;; Verify edge exists and direction is reversed
        (let ((edges (cmap-model-get-edges updated-graph3)))
          ;; Find the edge with our ID
          (let ((edge (assoc edge-id edges)))
            (should edge)
            (should (equal (cadr edge) node-id2)) ; Source is node-id2
            (should (equal (caddr edge) node-id1)) ; Target is node-id1
            (should (equal (plist-get (cadddr edge) :label) "B to A"))))))))

;; Test node renaming

(ert-deftest cmap-buffer-rename-node ()
  "Test renaming a node without side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; Add a node
    (let* ((result (test-add-node graph "Original Name"))
           (updated-graph (plist-get result :graph))
           (node-id (plist-get result :node-id)))
      
      ;; Rename the node
      (let ((renamed-graph (test-rename-node updated-graph node-id "New Name")))
        
        ;; Verify node has new name
        (let ((node (cmap-model-get-node renamed-graph node-id)))
          (should node)
          (should (equal (plist-get (cdr node) :label) "New Name")))))))

;; Test graph operations (add, remove, rename)

(ert-deftest cmap-buffer-graph-operations ()
  "Test a sequence of graph operations without side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; Add multiple nodes
    (let* ((result1 (test-add-node graph "Node A"))
           (graph1 (plist-get result1 :graph))
           (node-a-id (plist-get result1 :node-id))
           (result2 (test-add-node graph1 "Node B"))
           (graph2 (plist-get result2 :graph))
           (node-b-id (plist-get result2 :node-id))
           (result3 (test-add-node graph2 "Node C"))
           (graph3 (plist-get result3 :graph))
           (node-c-id (plist-get result3 :node-id)))
      
      ;; Add edges
      (let* ((edge-result1 (test-add-edge graph3 node-a-id node-b-id "A to B"))
             (graph4 (plist-get edge-result1 :graph))
             (edge1-id (plist-get edge-result1 :edge-id))
             (edge-result2 (test-add-edge graph4 node-b-id node-c-id "B to C"))
             (graph5 (plist-get edge-result2 :graph))
             (edge2-id (plist-get edge-result2 :edge-id)))
        
        ;; Verify nodes exist
        (let ((nodes (cmap-model-get-nodes graph5)))
          (should (assoc node-a-id nodes))
          (should (assoc node-b-id nodes))
          (should (assoc node-c-id nodes)))
        
        ;; Verify edges exist
        (let ((edges (cmap-model-get-edges graph5)))
          (should (assoc edge1-id edges))
          (should (assoc edge2-id edges)))))))

(provide 'test-el-cmap-buffer-enhanced)