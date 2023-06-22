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
    (should (equal (cmap-add-edge graph (cmap-edge "node_a" "node_b" nil "edge-a"))
                   (list :config nil
                         :digraph (list :nodes '(("node_b") ("node_a"))
                                        :edges '(("edge-a" "node_a" "node_b" nil)))))))
  (let ((graph (list :config nil
                     :digraph (list :nodes '(("node_id_a" (:label "Node A")))
                                    :edges nil))))
    (should (equal (cmap-add-edge graph (cmap-edge "node_id_a" "node_id_b"
                                                   nil "edge-1"))
                   (list :config nil
                         :digraph (list :nodes '(("node_id_a" (:label "Node A")) ("node_id_b"))
                                        :edges '(("edge-1" "node_id_a" "node_id_b"))))))))


(ert-deftest cmap-override-plist ()
  (let ((prop nil)
        (default-prop '(:fontname "Ubuntu")))
    (should (and (equal (cmap-override-plist default-prop prop)
                        '(:fontname "Ubuntu"))))))


(provide 'test-el-cmap-model)
