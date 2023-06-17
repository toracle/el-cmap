;; ConcepMap for Emacs

(require 's)

(defun cmap-graph-to-dot (graph)
  "digraph { }")


(defun cmap-node (id &optional label)
  `(,id ,label))


(defun cmap-render-digraph (graph)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (edges (plist-get digraph :edges)))
    (format "digraph {\n%s\n%s}\n"
            (cmap-render-nodes nodes)
            (cmap-render-edges edges))))


(defun cmap-render-node (node)
  (let ((result nil))
    (add-to-list 'result (format (car node) "%s"))
    (when (cdr node)
      (add-to-list 'result " ")
      (add-to-list 'result (format "[label=\"%s\"]" (car (cdr node)))))
    (add-to-list 'result ";")
    (setq result (reverse result))
    (apply 'concat result)))


(defun cmap-render-nodes (nodes)
  (s-join "\n" (mapcar 'cmap-render-node nodes)))


(defun cmap-render-edge (edge)
  (let ((result nil))
    (add-to-list 'result (format "%s -> %s"
                                 (car edge)
                                 (car (cdr edge))))
    (when (equal (length edge) 3)
      (add-to-list 'result " ")
      (add-to-list 'result (format "[label=\"%s\"]" (car (last edge)))))
    (add-to-list 'result ";")
    (setq result (reverse result))
    (apply 'concat result)))


(defun cmap-render-edges (edges)
  (s-join "\n" (mapcar 'cmap-render-edge edges)))


(provide 'el-cmap)
