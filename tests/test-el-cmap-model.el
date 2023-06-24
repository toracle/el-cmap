(require 'el-cmap-model)


(ert-deftest cmap-model-node ()
  (should (equal (cmap-model-node '(:label "Node A") "node-a-id")
                 '("node-a-id" . (:label "Node A"))))
  (should (equal (cdr (cmap-model-node '(:label "Node A")))
                 '(:label "Node A"))))


(ert-deftest cmap-model-init-graph ()
  (should (equal (cmap-model-init-graph)
                 '(:config nil :digraph (:nodes nil :edges nil)))))


(ert-deftest cmap-add-node ()
  (let ((graph (cmap-model-init-graph)))
    (should (equal (cmap-add-node graph (cmap-model-node '(:label "Node A") "node_a"))
                   (list :config nil
                         :digraph (list :nodes '(("node_a" . (:label "Node A")))
                                        :edges nil))))))


(ert-deftest cmap-add-edge ()
  (let ((graph (cmap-model-init-graph)))
    (should (equal (cmap-add-edge graph (cmap-model-edge "node_a" "node_b"
                                                         nil "edge-a"))
                   (list :config nil
                         :digraph (list :nodes '(("node_b") ("node_a"))
                                        :edges '(("edge-a" "node_a" "node_b" nil)))))))
  (let ((graph (list :config nil
                     :digraph (list :nodes '(("node_id_a" (:label "Node A")))
                                    :edges nil))))
    (should (equal (cmap-add-edge graph (cmap-model-edge "node_id_a" "node_id_b"
                                                         nil "edge-1"))
                   (list :config nil
                         :digraph (list :nodes '(("node_id_b") ("node_id_a" (:label "Node A")))
                                        :edges '(("edge-1" "node_id_a" "node_id_b" nil))))))))


(ert-deftest cmap-override-plist ()
  (let ((prop nil)
        (default-prop '(:fontname "Ubuntu")))
    (should (and (equal (cmap-override-plist default-prop prop)
                        '(:fontname "Ubuntu"))))))


(provide 'test-el-cmap-model)
