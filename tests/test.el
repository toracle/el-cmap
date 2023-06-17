(require 'ert)

(require 'el-cmap)

(setq fixture-graph
      '(:config (:graph nil
                        :node (:shape "record"
                                      :fillcolor "#eeeeee")
                        :edge nil)
                :digraph (:nodes (("Gerald Weinberg" "Gerald Weinberg")
                                  ("leadership" "leadership")
                                  ("process" "process")
                                  ("environment" "environment")
                                  ("people" "people")
                                  ("strength" "strength")
                                  ("motivator" "motivator")
                                  ("organizer" "organizer")
                                  ("problem-solver" "problem-solver"))
                                 :edges (("Gerald Weinberg" "leadership" "defines")
                                         ("leadership" "process" "is a")
                                         ("process" "environment" "creating an")
                                         ("environment" "people" "in which")
                                         ("people" "empowered" "become")
                                         ("people" "strength" "who has")
                                         ("strength" "motivator" "as a")
                                         ("strength" "organizer" "as a")
                                         ("strength" "problem-solver" "as a")
                                         )))
      )


(ert-deftest cmap-repr-node ()
  (should (equal (cmap-repr-node '("node_a"))
                 "node_a;"))
  (should (equal (cmap-repr-node '("node_a" "Node A"))
                 "node_a [label=\"Node A\"];")))


(ert-deftest cmap-repr-nodes ()
  (should (equal (cmap-repr-nodes '(("node_a") ("node_b")))
                 "node_a;\nnode_b;")))


(ert-deftest cmap-repr-edge ()
  (should (equal (cmap-repr-edge '("node_a" "node_b"))
                 "node_a -> node_b;"))
  (should (equal (cmap-repr-edge '("node_a" "node_b" "edge_label"))
                 "node_a -> node_b [label=\"edge_label\"];")))


(ert-deftest cmap-repr-edges ()
  (should (equal (cmap-repr-edges '(("node_a" "node_b")
                                      ("node_b" "node_c")))
                 "node_a -> node_b;\nnode_b -> node_c;"))
  (should (equal (cmap-repr-edges '(("node_a" "node_b" "label1")
                                      ("node_b" "node_c" "label2")))
                 "node_a -> node_b [label=\"label1\"];\nnode_b -> node_c [label=\"label2\"];")))


(ert-deftest cmap-repr-digraph ()
  (should (equal (cmap-repr-digraph nil)
                 "digraph {\n\n")))


(ert-deftest repr-node-isolated-node-graph ()
  (should (equal (cmap-repr-digraph '(:digraph (:nodes (("node_1_id" "Node 1")
                                                        ("node_2_id" "Node 2")))))
                 "digraph {\nnode_1_id [label=\"Node 1\"];\nnode_2_id [label=\"Node 2\"];\n}\n")))


(ert t)
