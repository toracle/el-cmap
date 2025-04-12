;; Enhanced tests for el-cmap-model with reduced side effects

(require 'dash)
(require 's)
(require 'cl-lib)

;; Load the module to test
(require 'el-cmap-model)

;; Define a global variable for testing to avoid the void-variable error
(defvar *cmap-graph* nil)

;; Helper functions with reduced side effects

(defun test-generate-node-id (graph)
  "Generate a unique node ID for the graph without side effects."
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (counter 1))
    (while (assoc (format "node_%d" counter) nodes)
      (setq counter (+ counter 1)))
    (format "node_%d" counter)))

(defun test-generate-node-label (graph)
  "Generate a unique node label for the graph without side effects."
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (counter 1))
    (while (-find (lambda (node) (equal (plist-get (cdr node) :label)
                                        (format "New Node %d" counter)))
                 nodes)
      (setq counter (+ counter 1)))
    (format "New Node %d" counter)))

(defun test-generate-edge-id (graph)
  "Generate a unique edge ID for the graph without side effects."
  (let* ((digraph (plist-get graph :digraph))
         (edges (plist-get digraph :edges))
         (counter 1))
    (while (assoc (format "edge_%d" counter) edges)
      (setq counter (+ counter 1)))
    (format "edge_%d" counter)))

(defun test-create-node (graph &optional properties id)
  "Create a node with reduced side effects."
  (let ((*cmap-graph* graph))
    (let ((node-id nil)
          (node-label nil)
          (default-node-property (list :label (test-generate-node-label graph))))
      (if id (setq node-id id)
        (setq node-id (test-generate-node-id graph)))
      (cons node-id (cmap-override-plist default-node-property properties)))))

(defun test-create-edge (graph node-a-id node-b-id &optional properties id)
  "Create an edge with reduced side effects."
  (let ((*cmap-graph* graph))
    (let ((edge-id nil))
      (if id (setq edge-id id)
        (setq edge-id (test-generate-edge-id graph)))
      (cons edge-id (list node-a-id node-b-id properties)))))

;; Tests for node ID generation

(ert-deftest cmap-model-node-id-generation ()
  "Test node ID generation with reduced side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; First node ID should be "node_1"
    (should (equal (test-generate-node-id graph) "node_1"))
    
    ;; After adding a node, the next ID should be "node_2"
    (let* ((node (test-create-node graph nil "node_1"))
           (updated-graph (cmap-model-add-node graph node)))
      (should (equal (test-generate-node-id updated-graph) "node_2"))
      
      ;; After adding node_2, the next ID should be "node_3"
      (let* ((node2 (test-create-node updated-graph nil "node_2"))
             (updated-graph2 (cmap-model-add-node updated-graph node2)))
        (should (equal (test-generate-node-id updated-graph2) "node_3"))))))

;; Tests for node label generation

(ert-deftest cmap-model-node-label-generation ()
  "Test node label generation with reduced side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; First node label should be "New Node 1"
    (should (equal (test-generate-node-label graph) "New Node 1"))
    
    ;; After adding a node with that label, the next should be "New Node 2"
    (let* ((node (test-create-node graph '(:label "New Node 1") "node_1"))
           (updated-graph (cmap-model-add-node graph node)))
      (should (equal (test-generate-node-label updated-graph) "New Node 2"))
      
      ;; After adding node with label "New Node 2", the next should be "New Node 3"
      (let* ((node2 (test-create-node updated-graph '(:label "New Node 2") "node_2"))
             (updated-graph2 (cmap-model-add-node updated-graph node2)))
        (should (equal (test-generate-node-label updated-graph2) "New Node 3"))))))

;; Tests for edge ID generation

(ert-deftest cmap-model-edge-id-generation ()
  "Test edge ID generation with reduced side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; First edge ID should be "edge_1"
    (should (equal (test-generate-edge-id graph) "edge_1"))
    
    ;; Add two nodes and an edge
    (let* ((*cmap-graph* graph)
           (node1 (test-create-node graph nil "node_1"))
           (node2 (test-create-node graph nil "node_2"))
           (graph-with-nodes (-> graph
                               (cmap-model-add-node node1)
                               (cmap-model-add-node node2)))
           (edge (test-create-edge graph-with-nodes "node_1" "node_2" nil "edge_1"))
           (updated-graph (cmap-model-add-edge graph-with-nodes edge)))
      
      ;; Next edge ID should be "edge_2"
      (should (equal (test-generate-edge-id updated-graph) "edge_2"))
      
      ;; Add another edge and check for "edge_3"
      (let* ((edge2 (test-create-edge updated-graph "node_2" "node_1" nil "edge_2"))
             (updated-graph2 (cmap-model-add-edge updated-graph edge2)))
        (should (equal (test-generate-edge-id updated-graph2) "edge_3"))))))

;; Test node creation with custom properties

(ert-deftest cmap-model-create-node-with-properties ()
  "Test node creation with custom properties."
  (let ((graph (cmap-model-init-graph)))
    ;; Create a node with custom properties
    (let* ((node (test-create-node graph '(:label "Custom Node" :color "red") "custom_node")))
      ;; Check node structure
      (should (equal (car node) "custom_node"))
      (should (equal (plist-get (cdr node) :label) "Custom Node"))
      (should (equal (plist-get (cdr node) :color) "red")))))

;; Test edge creation with custom properties

(ert-deftest cmap-model-create-edge-with-properties ()
  "Test edge creation with custom properties."
  (let ((graph (cmap-model-init-graph)))
    ;; Create an edge with custom properties
    (let* ((edge (test-create-edge graph "node_1" "node_2" '(:label "Custom Edge" :style "dotted") "custom_edge")))
      ;; Check edge structure
      (should (equal (car edge) "custom_edge"))
      (should (equal (cadr (cdr edge)) "node_2"))
      (should (equal (car (cdr edge)) "node_1"))
      (should (equal (plist-get (caddr (cdr edge)) :label) "Custom Edge"))
      (should (equal (plist-get (caddr (cdr edge)) :style) "dotted")))))

;; Test graph state after multiple operations

(ert-deftest cmap-model-graph-state-after-operations ()
  "Test graph state after multiple operations."
  (let ((graph (cmap-model-init-graph))
        (*cmap-graph* (cmap-model-init-graph)))
    ;; Add nodes
    (let* ((node1 (test-create-node graph '(:label "Node A") "node_1"))
           (node2 (test-create-node graph '(:label "Node B") "node_2"))
           (node3 (test-create-node graph '(:label "Node C") "node_3"))
           (graph-with-nodes (-> graph
                               (cmap-model-add-node node1)
                               (cmap-model-add-node node2)
                               (cmap-model-add-node node3))))
      
      ;; Verify node count
      (should (= (length (cmap-model-get-nodes graph-with-nodes)) 3))
      
      ;; Add edges
      (let* ((edge1 (test-create-edge graph-with-nodes "node_1" "node_2" '(:label "Edge 1-2") "edge_1"))
             (edge2 (test-create-edge graph-with-nodes "node_2" "node_3" '(:label "Edge 2-3") "edge_2"))
             (graph-with-edges (-> graph-with-nodes
                                 (cmap-model-add-edge edge1)
                                 (cmap-model-add-edge edge2))))
        
        ;; Verify edge count
        (should (= (length (cmap-model-get-edges graph-with-edges)) 2))
      
        ;; Verify all edges and nodes exist
        (let ((nodes (cmap-model-get-nodes graph-with-edges))
              (edges (cmap-model-get-edges graph-with-edges)))
          (should (assoc "node_1" nodes))
          (should (assoc "node_2" nodes))
          (should (assoc "node_3" nodes))
          (should (assoc "edge_1" edges))
          (should (assoc "edge_2" edges)))))))

;; Test directed edges retrieval

(ert-deftest cmap-model-get-directed-edges ()
  "Test retrieval of directed edges."
  ;; Initialize the variables
  (let ((test-graph (cmap-model-init-graph)))
    (let ((*cmap-graph* test-graph))
      ;; Add nodes and edges in both directions
      (let* ((node1 (test-create-node test-graph '(:label "Node A") "node_1"))
             (node2 (test-create-node test-graph '(:label "Node B") "node_2"))
             (node3 (test-create-node test-graph '(:label "Node C") "node_3"))
             (graph-with-nodes (-> test-graph
                                 (cmap-model-add-node node1)
                                 (cmap-model-add-node node2)
                                 (cmap-model-add-node node3)))
             (edge1 (test-create-edge graph-with-nodes "node_1" "node_2" '(:label "Edge 1-2") "edge_1"))
             (edge2 (test-create-edge graph-with-nodes "node_2" "node_3" '(:label "Edge 2-3") "edge_2"))
             (edge3 (test-create-edge graph-with-nodes "node_3" "node_1" '(:label "Edge 3-1") "edge_3"))
             (graph-with-edges (-> graph-with-nodes
                                 (cmap-model-add-edge edge1)
                                 (cmap-model-add-edge edge2)
                                 (cmap-model-add-edge edge3))))
        
        ;; Test outward edges from node_1
        (let ((outward-edges (cmap-model-get-directed-edges graph-with-edges "node_1")))
          (should (= (length outward-edges) 1))
          (should (equal (car (car outward-edges)) "edge_1")))
        
        ;; Test inward edges to node_1
        (let ((inward-edges (cmap-model-get-directed-edges graph-with-edges "node_1" t)))
          (should (= (length inward-edges) 1))
          (should (equal (car (car inward-edges)) "edge_3")))))))

(provide 'test-el-cmap-model-enhanced)