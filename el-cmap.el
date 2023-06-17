;; ConcepMap for Emacs

(require 's)

(defun cmap-graph-to-dot (graph)
  "digraph { }")


(defun cmap-node (id &optional label)
  `(,id ,label))


(defun cmap-render-node (node)
  (let ((result nil))
    (add-to-list 'result (format (car node) "%s"))
    (when (cdr node)
      (add-to-list 'result " ")
      (add-to-list 'result (format "[label=\"%s\"]" (car (cdr node)))))
    (add-to-list 'result ";")
    (setq result (reverse result))
    (apply 'concat result)))


(provide 'el-cmap)
