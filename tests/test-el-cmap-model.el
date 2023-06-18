(require 'el-cmap-model)


(ert-deftest cmap-node ()
  (should (equal (cmap-node '(:label "Node A") "node-a-id")
                 '("node-a-id" . (:label "Node A"))))
  (should (equal (cdr (cmap-node '(:label "Node A")))
                 '(:label "Node A"))))


(ert-deftest cmap-init-graph ()
  (should (equal (cmap-init-graph)
                 '(:config nil :digraph (:nodes nil :edges nil)))))


(ert-deftest cmap-add-node ()
  (let ((graph (cmap-init-graph)))
    (should (equal (cmap-add-node graph (cmap-node '(:label "Node A") "node_a"))
                   (list :config nil
                         :digraph (list :nodes '(("node_a" . (:label "Node A")))
                                        :edges nil))))))


(ert-deftest cmap-add-edge ()
  (let ((graph (cmap-init-graph)))
    (should (equal (cmap-add-edge graph '("node_a" "node_b"))
                   (list :config nil
                         :digraph (list :nodes '(("node_b") ("node_a"))
                                        :edges '(("node_a" "node_b"))))))))


(provide 'test-el-cmap-model)
