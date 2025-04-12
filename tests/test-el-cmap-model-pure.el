;; Tests for pure functional versions of el-cmap-model

(require 'dash)
(require 's)
(require 'cl-lib)

;; Load the modules to test
(require 'el-cmap-model)
(require 'el-cmap-model-pure)

;; Define global variable for testing to avoid the void-variable error
(defvar *cmap-graph* nil)

;; Test pure node ID generation

(ert-deftest cmap-model-pure-node-id-generation ()
  "Test pure node ID generation function."
  (let ((graph (cmap-model-init-graph)))
    ;; First node ID should be "node_1"
    (should (equal (cmap-model-pure-node-id graph) "node_1"))
    
    ;; Add a node and check next ID
    (let* ((node-id "node_1")
           (node (cons node-id '(:label "Test Node")))
           (updated-graph (cmap-model-add-node graph node)))
      (should (equal (cmap-model-pure-node-id updated-graph) "node_2"))
      
      ;; Add another node and check next ID
      (let* ((node-id2 "node_2")
             (node2 (cons node-id2 '(:label "Test Node 2")))
             (updated-graph2 (cmap-model-add-node updated-graph node2)))
        (should (equal (cmap-model-pure-node-id updated-graph2) "node_3"))))))

;; Test pure node label generation

(ert-deftest cmap-model-pure-node-label-generation ()
  "Test pure node label generation function."
  (let ((graph (cmap-model-init-graph)))
    ;; First node label should be "New Node 1"
    (should (equal (cmap-model-pure-node-label graph) "New Node 1"))
    
    ;; Add a node with label "New Node 1" and check next label
    (let* ((node (cons "node_1" '(:label "New Node 1")))
           (updated-graph (cmap-model-add-node graph node)))
      (should (equal (cmap-model-pure-node-label updated-graph) "New Node 2"))
      
      ;; Add another node with label "New Node 2" and check next label
      (let* ((node2 (cons "node_2" '(:label "New Node 2")))
             (updated-graph2 (cmap-model-add-node updated-graph node2)))
        (should (equal (cmap-model-pure-node-label updated-graph2) "New Node 3"))))))

;; Test pure edge ID generation

(ert-deftest cmap-model-pure-edge-id-generation ()
  "Test pure edge ID generation function."
  (let ((graph (cmap-model-init-graph)))
    ;; First edge ID should be "edge_1"
    (should (equal (cmap-model-pure-edge-id graph) "edge_1"))
    
    ;; Add an edge and check next ID
    (let* ((edge (cons "edge_1" '("node_1" "node_2" nil)))
           (updated-graph (cmap-model-add-edge graph edge)))
      (should (equal (cmap-model-pure-edge-id updated-graph) "edge_2"))
      
      ;; Add another edge and check next ID
      (let* ((edge2 (cons "edge_2" '("node_2" "node_3" nil)))
             (updated-graph2 (cmap-model-add-edge updated-graph edge2)))
        (should (equal (cmap-model-pure-edge-id updated-graph2) "edge_3"))))))

;; Test pure node creation

(ert-deftest cmap-model-pure-node-creation ()
  "Test pure node creation function."
  (let ((graph (cmap-model-init-graph)))
    ;; Create a node with default properties
    (let ((node (cmap-model-pure-node graph)))
      (should (equal (plist-get (cdr node) :label) "New Node 1")))
    
    ;; Create a node with custom properties
    (let ((node (cmap-model-pure-node graph '(:label "Custom Node" :color "red"))))
      (should (equal (plist-get (cdr node) :label) "Custom Node"))
      (should (equal (plist-get (cdr node) :color) "red")))
    
    ;; Create a node with custom ID
    (let ((node (cmap-model-pure-node graph nil "custom_id")))
      (should (equal (car node) "custom_id")))))

;; Test pure edge creation

(ert-deftest cmap-model-pure-edge-creation ()
  "Test pure edge creation function."
  (let ((graph (cmap-model-init-graph)))
    ;; Create an edge with default properties
    (let ((edge (cmap-model-pure-edge graph "node_1" "node_2")))
      (should (equal (cadr edge) "node_1"))
      (should (equal (caddr edge) "node_2"))
      (should (null (cadddr edge))))
    
    ;; Create an edge with custom properties
    (let ((edge (cmap-model-pure-edge graph "node_1" "node_2" '(:label "Custom Edge"))))
      (should (equal (plist-get (cadddr edge) :label) "Custom Edge")))
    
    ;; Create an edge with custom ID
    (let ((edge (cmap-model-pure-edge graph "node_1" "node_2" nil "custom_edge")))
      (should (equal (car edge) "custom_edge")))))

;; Test pure node addition

(ert-deftest cmap-model-pure-add-node ()
  "Test pure node addition function."
  (let ((graph (cmap-model-init-graph)))
    ;; Add a node and check the result
    (let* ((node (cmap-model-pure-node graph '(:label "Test Node") "node_1"))
           (updated-graph (cmap-model-pure-add-node graph node)))
      
      ;; Original graph should be unchanged
      (should (null (plist-get (plist-get graph :digraph) :nodes)))
      
      ;; Updated graph should have the node
      (let ((nodes (plist-get (plist-get updated-graph :digraph) :nodes)))
        (should (= (length nodes) 1))
        (should (equal (caar nodes) "node_1"))
        (should (equal (plist-get (cdar nodes) :label) "Test Node")))
      
      ;; Update an existing node
      (let* ((updated-node (cons "node_1" '(:label "Updated Node")))
             (final-graph (cmap-model-pure-add-node updated-graph updated-node)))
        
        ;; Node should be updated in final graph
        (let* ((nodes (plist-get (plist-get final-graph :digraph) :nodes))
               (node (assoc "node_1" nodes)))
          (should (equal (plist-get (cdr node) :label) "Updated Node")))))))

;; Test pure edge addition

(ert-deftest cmap-model-pure-add-edge ()
  "Test pure edge addition function."
  (let ((graph (cmap-model-init-graph)))
    ;; Add an edge and check the result
    (let* ((edge (cmap-model-pure-edge graph "node_1" "node_2" '(:label "Test Edge") "edge_1"))
           (updated-graph (cmap-model-pure-add-edge graph edge)))
      
      ;; Original graph should be unchanged
      (should (null (plist-get (plist-get graph :digraph) :edges)))
      
      ;; Updated graph should have the edge
      (let ((edges (plist-get (plist-get updated-graph :digraph) :edges)))
        (should (= (length edges) 1))
        (should (equal (caar edges) "edge_1"))
        (should (equal (plist-get (cadddr (car edges)) :label) "Test Edge")))
      
      ;; Nodes should be created automatically
      (let ((nodes (plist-get (plist-get updated-graph :digraph) :nodes)))
        (should (= (length nodes) 2))
        (should (cl-some (lambda (node) (equal (car node) "node_1")) nodes))
        (should (cl-some (lambda (node) (equal (car node) "node_2")) nodes))))))

;; Test pure node removal

(ert-deftest cmap-model-pure-remove-node ()
  "Test pure node removal function."
  (let ((graph (cmap-model-init-graph)))
    ;; Setup graph with nodes and edges
    (let* ((node1 (cmap-model-pure-node graph '(:label "Node A") "node_1"))
           (node2 (cmap-model-pure-node graph '(:label "Node B") "node_2"))
           (node3 (cmap-model-pure-node graph '(:label "Node C") "node_3"))
           (graph-with-nodes (-> graph
                               (cmap-model-pure-add-node node1)
                               (cmap-model-pure-add-node node2)
                               (cmap-model-pure-add-node node3)))
           (edge1 (cmap-model-pure-edge graph-with-nodes "node_1" "node_2" '(:label "Edge 1-2") "edge_1"))
           (edge2 (cmap-model-pure-edge graph-with-nodes "node_2" "node_3" '(:label "Edge 2-3") "edge_2"))
           (graph-with-edges (-> graph-with-nodes
                               (cmap-model-pure-add-edge edge1)
                               (cmap-model-pure-add-edge edge2))))
      
      ;; Remove node_2 and check the result
      (let ((updated-graph (cmap-model-pure-remove-node graph-with-edges "node_2")))
        
        ;; Original graph should be unchanged
        (should (= (length (plist-get (plist-get graph-with-edges :digraph) :nodes)) 3))
        (should (= (length (plist-get (plist-get graph-with-edges :digraph) :edges)) 2))
        
        ;; Updated graph should have node_2 removed
        (let ((nodes (plist-get (plist-get updated-graph :digraph) :nodes)))
          (should (= (length nodes) 2))
          (should (null (assoc "node_2" nodes))))
        
        ;; All edges connected to node_2 should be removed
        (let ((edges (plist-get (plist-get updated-graph :digraph) :edges)))
          (should (null edges)))))))

;; Test pure edge removal

(ert-deftest cmap-model-pure-remove-edge ()
  "Test pure edge removal function."
  (let ((graph (cmap-model-init-graph)))
    ;; Setup graph with nodes and edges
    (let* ((node1 (cmap-model-pure-node graph '(:label "Node A") "node_1"))
           (node2 (cmap-model-pure-node graph '(:label "Node B") "node_2"))
           (graph-with-nodes (-> graph
                               (cmap-model-pure-add-node node1)
                               (cmap-model-pure-add-node node2)))
           (edge (cmap-model-pure-edge graph-with-nodes "node_1" "node_2" '(:label "Test Edge") "edge_1"))
           (graph-with-edge (cmap-model-pure-add-edge graph-with-nodes edge)))
      
      ;; Remove the edge and check the result
      (let ((updated-graph (cmap-model-pure-remove-edge graph-with-edge "edge_1")))
        
        ;; Original graph should be unchanged
        (should (= (length (plist-get (plist-get graph-with-edge :digraph) :edges)) 1))
        
        ;; Updated graph should have the edge removed
        (let ((edges (plist-get (plist-get updated-graph :digraph) :edges)))
          (should (null edges)))
        
        ;; Nodes should still exist
        (let ((nodes (plist-get (plist-get updated-graph :digraph) :nodes)))
          (should (= (length nodes) 2)))))))

;; Test pure node property update

(ert-deftest cmap-model-pure-set-node-prop ()
  "Test pure node property update function."
  (let ((graph (cmap-model-init-graph)))
    ;; Add a node
    (let* ((node (cmap-model-pure-node graph '(:label "Original Label") "node_1"))
           (graph-with-node (cmap-model-pure-add-node graph node)))
      
      ;; Update node property
      (let ((updated-graph (cmap-model-pure-set-node-prop graph-with-node "node_1" :label "Updated Label")))
        
        ;; Original graph should be unchanged
        (let* ((nodes (plist-get (plist-get graph-with-node :digraph) :nodes))
               (node (assoc "node_1" nodes)))
          (should (equal (plist-get (cdr node) :label) "Original Label")))
        
        ;; Updated graph should have the property changed
        (let* ((nodes (plist-get (plist-get updated-graph :digraph) :nodes))
               (node (assoc "node_1" nodes)))
          (should (equal (plist-get (cdr node) :label) "Updated Label")))))))

;; Test pure edge property update

(ert-deftest cmap-model-pure-set-edge-prop ()
  "Test pure edge property update function."
  (let ((graph (cmap-model-init-graph)))
    ;; Setup graph with an edge
    (let* ((node1 (cmap-model-pure-node graph nil "node_1"))
           (node2 (cmap-model-pure-node graph nil "node_2"))
           (graph-with-nodes (-> graph
                               (cmap-model-pure-add-node node1)
                               (cmap-model-pure-add-node node2)))
           (edge (cmap-model-pure-edge graph-with-nodes "node_1" "node_2" '(:label "Original Label") "edge_1"))
           (graph-with-edge (cmap-model-pure-add-edge graph-with-nodes edge)))
      
      ;; Update edge property
      (let ((updated-graph (cmap-model-pure-set-edge-prop graph-with-edge "edge_1" :label "Updated Label")))
        
        ;; Original graph should be unchanged
        (let* ((edges (plist-get (plist-get graph-with-edge :digraph) :edges))
               (edge (assoc "edge_1" edges)))
          (should (equal (plist-get (cadddr edge) :label) "Original Label")))
        
        ;; Updated graph should have the property changed
        (let* ((edges (plist-get (plist-get updated-graph :digraph) :edges))
               (edge (assoc "edge_1" edges)))
          (should (equal (plist-get (cadddr edge) :label) "Updated Label")))))))

(provide 'test-el-cmap-model-pure)