(require 'el-cmap-model)
(require 'el-cmap-repr)

(ert-deftest cmap-repr-node ()
  (should (equal (cmap-repr-node (cmap-model-node nil "node_a"))
                 "node_a [shape=record, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\"];"))
  (should (equal (cmap-repr-node '("node_a" :label "Node A"))
                 "node_a [shape=record, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\", label=\"Node A\"];")))


(ert-deftest cmap-repr-nodes ()
  (should (equal (cmap-repr-nodes '(("node_a") ("node_b")))
                 "node_a [shape=record, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\"];\nnode_b [shape=record, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\"];")))


(ert-deftest cmap-repr-edge ()
  (should (equal (cmap-repr-edge (cmap-model-edge "node_a" "node_b"
                                            nil
                                            "edge-1"))
                 "node_a -> node_b [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true];"))
  (should (equal (cmap-repr-edge (cmap-model-edge "node_a" "node_b"
                                            '(:label "edge_label")
                                            "edge-1"))
                 "node_a -> node_b [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true, label=\"edge_label\"];")))


(ert-deftest cmap-repr-edges ()
  (should (equal (cmap-repr-edges '((cmap-model-edge "node_a" "node_b" nil "edge-1")
                                    (cmap-model-edge "node_b" "node_c" nil "edge-2")))
                 "node_a -> node_b [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true];\nnode_b -> node_c [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true];"))
  (should (equal (cmap-repr-edges '((cmap-model-edge "node_a" "node_b" (:label "label1"))
                                    (cmap-model-edge "node_b" "node_c" (:label "label2"))))
                 "node_a -> node_b [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true, label=\"label1\"];\nnode_b -> node_c [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true, label=\"label2\"];")))


(ert-deftest cmap-repr-digraph ()
  (should (equal (cmap-repr-digraph nil)
                 "digraph {\nK=1;\n\n\n}\n")))


(ert-deftest repr-node-isolated-node-graph ()
  (should (equal (cmap-repr-digraph '(:digraph (:nodes (("node_1_id" . (:label "Node 1"))
                                                        ("node_2_id" . (:label "Node 2"))))))
                 "digraph {\nK=1;\nnode_1_id [shape=record, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\", label=\"Node 1\"];\nnode_2_id [shape=record, fillcolor=\"#eeeeee\", style=\"rounded,filled\", fontname=\"Liberation Serif\", label=\"Node 2\"];\n\n}\n")))


(ert-deftest repr-edge-only-graph ()
  (should (equal (cmap-repr-digraph '(:digraph (:edges (("edge-1" "node_1_id" "node_2_id")))))
                 "digraph {\nK=1;\n\nnode_1_id -> node_2_id [fontcolor=\"#777777\", fontname=\"Liberation Serif\", splines=true];\n}\n")))


(ert-deftest cmap-repr-properties ()
  (should (equal (cmap-repr-properties '(:label "Node A"))
                 "[label=\"Node A\"]")))


(provide 'test-el-cmap-repr)
