(defun cmap-repr-digraph (graph)
  (let* ((digraph (plist-get graph :digraph))
         (nodes (plist-get digraph :nodes))
         (edges (plist-get digraph :edges)))
    (format "digraph {\nK=1;\n%s\n%s\n}\n"
            (cmap-repr-nodes nodes)
            (cmap-repr-edges edges))))


(defun cmap-default-node-properties ()
  (list :shape 'record
        :fillcolor "#eeeeee"
        :style "rounded,filled"
        :fontname "Liberation Serif"))


(defun cmap-repr-node (node)
  (let ((result nil)
        (node-id (car node))
        (node-properties (cmap-override-plist (cmap-default-node-properties)
                                              (cdr node))))
    (add-to-list 'result (format "%s" node-id))
    (when node-properties
      (add-to-list 'result " ")
      (add-to-list 'result (cmap-repr-properties node-properties)))
    (add-to-list 'result ";")
    (setq result (reverse result))
    (apply 'concat result)))


(defun cmap-repr-nodes (nodes)
  (s-join "\n" (mapcar 'cmap-repr-node nodes)))


(defun cmap-default-edge-properties ()
  (list :fontcolor "#777777"
        :fontname "Liberation Serif"
        :splines 'true))


(defun cmap-repr-edge (edge)
  (let* ((result nil)
         (edge-id (car edge))
         (edge-extra (cdr edge))
         (src-node-id (car edge-extra))
         (tgt-node-id (cadr edge-extra))
         (properties (cmap-override-plist (cmap-default-edge-properties)
                                          (caddr edge-extra))))
    (add-to-list 'result (format "%s -> %s"
                                 src-node-id
                                 tgt-node-id))
    (when properties
      (add-to-list 'result " ")
      (add-to-list 'result (cmap-repr-properties properties)))
    (add-to-list 'result ";")
    (setq result (reverse result))
    (apply 'concat result)))


(defun cmap-repr-edges (edges)
  (s-join "\n" (mapcar 'cmap-repr-edge edges)))


(defun cmap-repr-properties (properties)
    "Convert a plist to a string in the format [key=value, ...]."
  (if properties ; check if properties is not nil
      (let ((result "[")
            (properties (copy-sequence properties))) ; make a copy of the properties
        (while properties
          (let ((key (pop properties))
                (value (pop properties)))
            ;; convert key to a string and remove the leading colon
            (setq key (substring (symbol-name key) 1))
            (setq result (concat result
                                 (format "%s=%S" key value)
                                 (if properties ", " "")))))
        (setq result (concat result "]"))
        result)
    "")) ; return empty string if properties is nil)


(provide 'el-cmap-repr)
