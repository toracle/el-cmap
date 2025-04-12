;; Enhanced tests for el-cmap-repr with reduced side effects

(require 'dash)
(require 's)
(require 'cl-lib)

;; Load the modules to test
(require 'el-cmap-model)
(require 'el-cmap-repr)

;; Define global variable for testing to avoid the void-variable error
(defvar *cmap-graph* nil)

;; Helper functions

(defun test-create-sample-graph ()
  "Create a sample graph for testing without side effects."
  (let ((graph (cmap-model-init-graph)))
    ;; Add nodes
    (let* ((node1 (cons "node_1" '(:label "Node A")))
           (node2 (cons "node_2" '(:label "Node B")))
           (node3 (cons "node_3" '(:label "Node C")))
           (graph-with-nodes (-> graph
                               (cmap-model-add-node node1)
                               (cmap-model-add-node node2)
                               (cmap-model-add-node node3)))
           ;; Add edges
           (edge1 (cons "edge_1" (list "node_1" "node_2" '(:label "A to B"))))
           (edge2 (cons "edge_2" (list "node_2" "node_3" '(:label "B to C"))))
           (edge3 (cons "edge_3" (list "node_3" "node_1" '(:label "C to A"))))
           (graph-with-edges (-> graph-with-nodes
                               (cmap-model-add-edge edge1)
                               (cmap-model-add-edge edge2)
                               (cmap-model-add-edge edge3))))
      graph-with-edges)))

;; Tests for representing nodes with various properties

(ert-deftest cmap-repr-node-properties ()
  "Test node representation with various properties."
  ;; Basic node
  (should (equal (cmap-repr-node '("node_1" :label "Basic Node"))
                 "node_1 [shape=record, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\", label=\"Basic Node\"];"))
  
  ;; Node with custom color
  (should (equal (cmap-repr-node '("node_2" :label "Colored Node" :fillcolor "#ffcccc"))
                 "node_2 [shape=record, fillcolor=\"#ffcccc\", style=\"rounded,filled\", fontname=\"Liberation Serif\", label=\"Colored Node\"];"))
  
  ;; Node with custom shape
  (should (equal (cmap-repr-node '("node_3" :label "Diamond Node" :shape diamond))
                 "node_3 [shape=diamond, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\", label=\"Diamond Node\"];"))
  
  ;; Node with multiple custom properties
  (should (equal (cmap-repr-node '("node_4" :label "Custom Node" :shape circle :fillcolor "#ccffcc" :style "filled"))
                 "node_4 [shape=circle, fillcolor=\"#ccffcc\", style=\"filled\", fontname=\"Liberation Serif\", label=\"Custom Node\"];")))

;; Tests for representing edges with various properties

(ert-deftest cmap-repr-edge-properties ()
  "Test edge representation with various properties."
  ;; Basic edge
  (should (equal (cmap-repr-edge '("edge_1" "node_1" "node_2" (:label "Basic Edge")))
                 "node_1 -> node_2 [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true, label=\"Basic Edge\"];"))
  
  ;; Edge with custom color
  (should (equal (cmap-repr-edge '("edge_2" "node_2" "node_3" (:label "Colored Edge" :color "red")))
                 "node_2 -> node_3 [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true, label=\"Colored Edge\", color=\"red\"];"))
  
  ;; Edge with style
  (should (equal (cmap-repr-edge '("edge_3" "node_3" "node_1" (:label "Styled Edge" :style "dotted")))
                 "node_3 -> node_1 [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true, label=\"Styled Edge\", style=\"dotted\"];"))
  
  ;; Edge with no properties (should use defaults)
  (should (equal (cmap-repr-edge '("edge_4" "node_4" "node_5" nil))
                 "node_4 -> node_5 [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true];")))

;; Test property formatting

(ert-deftest cmap-repr-properties-formatting ()
  "Test formatting of property lists for DOT format."
  ;; Empty properties
  (should (equal (cmap-repr-properties nil) ""))
  
  ;; Single property
  (should (equal (cmap-repr-properties '(:label "Test")) "[label=\"Test\"]"))
  
  ;; Multiple properties
  (should (equal (cmap-repr-properties '(:label "Test" :color "red")) 
                 "[label=\"Test\", color=\"red\"]"))
  
  ;; Properties with non-string values
  (should (equal (cmap-repr-properties '(:label "Test" :shape circle :splines t))
                 "[label=\"Test\", shape=circle, splines=t]"))
  
  ;; Properties with special characters
  (should (equal (cmap-repr-properties '(:label "Test \"quoted\" text"))
                 "[label=\"Test \\\"quoted\\\" text\"]")))

;; Test complete graph representation

(ert-deftest cmap-repr-complete-graph ()
  "Test representation of a complete graph."
  (let ((graph (test-create-sample-graph)))
    (let ((dot-output (cmap-repr-digraph graph)))
      ;; Basic structure checks
      (should (s-contains? "digraph {" dot-output))
      (should (s-contains? "K=1;" dot-output))
      (should (s-contains? "}" dot-output))
      
      ;; Check that all nodes are represented
      (should (s-contains? "node_1 [" dot-output))
      (should (s-contains? "node_2 [" dot-output))
      (should (s-contains? "node_3 [" dot-output))
      
      ;; Check that all edges are represented
      (should (s-contains? "node_1 -> node_2" dot-output))
      (should (s-contains? "node_2 -> node_3" dot-output))
      (should (s-contains? "node_3 -> node_1" dot-output))
      
      ;; Check for label inclusion
      (should (s-contains? "label=\"Node A\"" dot-output))
      (should (s-contains? "label=\"A to B\"" dot-output)))))

;; Test digraph representation with custom properties

(ert-deftest cmap-repr-digraph-custom-properties ()
  "Test representation of a graph with custom node and edge properties."
  (let* ((graph (cmap-model-init-graph))
         (node1 (cons "node_1" '(:label "Custom Node" :shape circle :fillcolor "#ccffcc")))
         (node2 (cons "node_2" '(:label "Another Node" :style "filled,dashed")))
         (graph-with-nodes (-> graph
                             (cmap-model-add-node node1)
                             (cmap-model-add-node node2)))
         (edge (cons "edge_1" (list "node_1" "node_2" '(:label "Custom Edge" :style "bold" :color "blue"))))
         (final-graph (cmap-model-add-edge graph-with-nodes edge))
         (dot-output (cmap-repr-digraph final-graph)))
    
    ;; Check for custom node properties
    (should (s-contains? "shape=circle" dot-output))
    (should (s-contains? "fillcolor=\"#ccffcc\"" dot-output))
    (should (s-contains? "style=\"filled,dashed\"" dot-output))
    
    ;; Check for custom edge properties
    (should (s-contains? "style=\"bold\"" dot-output))
    (should (s-contains? "color=\"blue\"" dot-output))))

;; Test handling of special characters

(ert-deftest cmap-repr-special-characters ()
  "Test representation with special characters in labels."
  ;; Node with special characters
  (let* ((node (cons "node_special" '(:label "Node with \"quotes\" and newlines")))
         (node-repr (cmap-repr-node node)))
    (should (s-contains? "label=\"Node with \\\"quotes\\\" and newlines\"" node-repr)))
  
  ;; Edge with special characters
  (let* ((edge (cons "edge_special" (list "node_1" "node_2" '(:label "Edge with \"quotes\" and newlines"))))
         (edge-repr (cmap-repr-edge edge)))
    (should (s-contains? "label=\"Edge with \\\"quotes\\\" and newlines\"" edge-repr))))

(provide 'test-el-cmap-repr-enhanced)