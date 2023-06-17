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


(ert-deftest dummy-graph ()
  (should (equal (cmap-graph-to-dot nil)
                 "digraph { }")))


(ert-deftest render-cmap-render-node ()
  (should (equal (cmap-render-node '("node_a"))
                 "node_a;"))
  (should (equal (cmap-render-node '("node_a" "Node A"))
                 "node_a [label=\"Node A\"];")))


(ert-deftest render-cmap-render-nodes ()
  (should (equal (cmap-render-nodes '(("node_a") ("node_b")))
                 "node_a;\nnode_b;")))


(ert-deftest render-cmap-render-edge ()
  (should (equal (cmap-render-edge '("node_a" "node_b"))
                 "node_a -> node_b;"))
  (should (equal (cmap-render-edge '("node_a" "node_b" "edge_label"))
                 "node_a -> node_b [label=\"edge_label\"];")))


(ert-deftest render-cmap-render-edges ()
  (should (equal (cmap-render-edges '(("node_a" "node_b")
                                      ("node_b" "node_c")))
                 "node_a -> node_b;\nnode_b -> node_c;"))
  (should (equal (cmap-render-edges '(("node_a" "node_b" "label1")
                                      ("node_b" "node_c" "label2")))
                 "node_a -> node_b [label=\"label1\"];\nnode_b -> node_c [label=\"label2\"];")))


;; (ert-deftest node-isolated-graph ()
;;   (should (equal (cmap-graph-to-dot '(:digraph (:nodes (("node_1_id" "Node 1")
;;                                                    ("node_2_id" "Node 2")))))
;;                  "digraph { node_1_id [label=\"Node 1\"];\n node_2_id [label=\"Node 2\"]; }")))


(ert t)
