;; ConcepMap for Emacs

(require 's)
(require 'el-cmap-repr)


(defun cmap-graph-to-dot (graph)
  "digraph { }")


(defun cmap-node (id &optional label)
  `(,id ,label))




(provide 'el-cmap)
