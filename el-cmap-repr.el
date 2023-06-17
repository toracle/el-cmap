(defun cmap-repr-digraph (graph)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (edges (plist-get digraph :edges)))
    (format "digraph {\n%s\n%s\n}\n"
            (cmap-repr-nodes nodes)
            (cmap-repr-edges edges))))


(defun cmap-repr-node (node)
  (let ((result nil))
    (add-to-list 'result (format (car node) "%s"))
    (when (cdr node)
      (add-to-list 'result " ")
      (add-to-list 'result (format "[label=\"%s\"]" (car (cdr node)))))
    (add-to-list 'result ";")
    (setq result (reverse result))
    (apply 'concat result)))


(defun cmap-repr-nodes (nodes)
  (s-join "\n" (mapcar 'cmap-repr-node nodes)))


(defun cmap-repr-edge (edge)
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


(defun cmap-repr-edges (edges)
  (s-join "\n" (mapcar 'cmap-repr-edge edges)))

(provide 'el-cmap-repr)
