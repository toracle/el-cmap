;; Load required libraries
(require 'dash)
(require 's)
(require 'cl-lib)

;; Load the module to test
(require 'el-cmap-model)

;; Define a global variable for testing to avoid the void-variable error
(defvar *cmap-graph* nil)

(ert-deftest cmap-model-node ()
  (let ((*cmap-graph* (cmap-model-init-graph)))
    (should (equal (cmap-model-node '(:label "Node A") "node-a-id")
                   '("node-a-id" . (:label "Node A"))))
    (should (equal (cdr (cmap-model-node '(:label "Node A")))
                   '(:label "Node A")))))

(ert-deftest cmap-model-init-graph ()
  (should (equal (cmap-model-init-graph)
                 '(:config nil :digraph (:nodes nil :edges nil)))))

(ert-deftest cmap-model-add-node ()
  (let ((graph (cmap-model-init-graph))
        (*cmap-graph* (cmap-model-init-graph)))
    (should (equal (cmap-model-add-node graph (cmap-model-node '(:label "Node A") "node_a"))
                   (list :config nil
                         :digraph (list :nodes '(("node_a" . (:label "Node A")))
                                        :edges nil))))))

(ert-deftest cmap-model-add-edge ()
  (let ((graph (cmap-model-init-graph))
        (*cmap-graph* (cmap-model-init-graph)))
    (let ((result (cmap-model-add-edge graph (cmap-model-edge "node_a" "node_b" nil "edge-a"))))
      ;; Check config
      (should (equal (plist-get result :config) nil))
      ;; Check that the edge was added
      (should (equal (plist-get (plist-get result :digraph) :edges)
                     '(("edge-a" "node_a" "node_b" nil))))
      ;; Check that both nodes exist (any format)
      (let ((nodes (plist-get (plist-get result :digraph) :nodes)))
        (should (= (length nodes) 2))
        (should (cl-some (lambda (node) (equal (car node) "node_a")) nodes))
        (should (cl-some (lambda (node) (equal (car node) "node_b")) nodes)))))
  
  (let ((graph (list :config nil
                     :digraph (list :nodes '(("node_id_a" . (:label "Node A")))
                                    :edges nil)))
        (*cmap-graph* (cmap-model-init-graph)))
    (let ((result (cmap-model-add-edge graph (cmap-model-edge "node_id_a" "node_id_b" nil "edge-1"))))
      ;; Check config
      (should (equal (plist-get result :config) nil))
      ;; Check that the edge was added
      (should (equal (plist-get (plist-get result :digraph) :edges)
                     '(("edge-1" "node_id_a" "node_id_b" nil))))
      ;; Check that both nodes exist (any format)
      (let ((nodes (plist-get (plist-get result :digraph) :nodes)))
        (should (= (length nodes) 2))
        (should (cl-some (lambda (node) (equal (car node) "node_id_a")) nodes))
        (should (cl-some (lambda (node) (equal (car node) "node_id_b")) nodes))))))

(ert-deftest cmap-override-plist ()
  (let ((prop nil)
        (default-prop '(:fontname "Ubuntu")))
    (should (and (equal (cmap-override-plist default-prop prop)
                        '(:fontname "Ubuntu"))))))

(provide 'test-el-cmap-model)