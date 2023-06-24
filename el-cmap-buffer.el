(require 's)

(defun cmap-buffer-save ()
  (interactive)
  (unless *cmap-path*
    (setq-local *cmap-path* (read-file-name "Select a file to save: ")))
  (cmap-model-save *cmap-graph* *cmap-path*))


(defun cmap-buffer-load ()
  (interactive)
  (setq-local *cmap-path* (read-file-name "Select a file to load: "))
  (setq-local *cmap-graph* (cmap-model-load *cmap-path*))
  (cmap-buffer))


(defun cmap-buffer-add-node ()
  (interactive)
  (let ((node-label (read-string "Input node label: ")))
    (cmap-model-add-node *cmap-graph*
                         (cmap-model-node (list :label node-label)))
    (cmap-buffer)))


(defun cmap-buffer-add-edge-outward ()
  (cmap-buffer-add-edge))


(defun cmap-buffer-add-edge-inward ()
  (cmap-buffer-add-edge t))


(defun cmap-buffer-add-edge (&optional inward)
  (interactive)
  (let* ((node-id (cmap-buffer-get-node))
         (edge-label (read-string "Input edge label: "))
         (edge nil))

    (if inward
        (setq edge (cmap-model-edge node-id *cmap-focal-node-id*
                              (list :label edge-label)))
      (setq edge (cmap-model-edge *cmap-focal-node-id* node-id
                            (list :label edge-label))))

    (cmap-model-add-edge *cmap-graph* edge)
    (cmap-buffer)))


(defun cmap-buffer-select-focal-node ()
  (interactive)
  (let ((node-id (cmap-buffer-get-node)))
    (when node-id
      (setq-local *cmap-focal-node-id* node-id)
      (cmap-buffer))))


(defun cmap-buffer-export-graph ()
  (interactive)
  (let* ((dot-content (cmap-repr-digraph *cmap-graph*))
         (dot-path (cmap-path-with-ext *cmap-path* "el" "dot"))
         (image-path (cmap-path-with-ext *cmap-path* "el" "png")))
    (with-temp-buffer
      (insert dot-content)
      (write-region nil nil dot-path))
    (call-process "dot"
                  nil nil "*el-cmap-output*"
                  "-Kfdp" "-Tpng" dot-path (concat "-o" image-path))
    (cmap-mode-viewer image-path)))


;; define a function that returns a list of node labels
(defun cmap-model-get-node-labels (graph)
  (mapcar (lambda (node) (plist-get (cdr node) :label))
          (cmap-model-get-nodes graph)))


;; define a function that returns the node id for a given node label
(defun cmap-model-get-node-id (graph label)
  (car (seq-find (lambda (node) (equal label (plist-get (cdr node) :label)))
                 (cmap-model-get-nodes graph))))


;; define a function that invokes ido-completing-read with the node labels as candidates
;; and a lambda function that prints the node id as the action
(defun cmap-buffer-get-node ()
  (interactive)
  (setq-local ido-enable-flex-matching t)
  (let ((label (ido-completing-read "Select node: " (cmap-model-get-node-labels *cmap-graph*))))
    (message "Selected node id: %s" (cmap-model-get-node-id *cmap-graph* label))
    (cmap-model-get-node-id *cmap-graph* label)))


(defun cmap-buffer-new-graph ()
  (interactive)
  (setq-local *cmap-graph* (cmap-model-init-graph))
  (setq-local *cmap-path* nil)
  (cmap-buffer))


(defun cmap-buffer-toolbar ()
  (insert "[")
  (insert-button "New Graph"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-new-graph)))

  (insert "] [")
  (insert-button "Add Node"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-add-node)))
  (insert "] [")
  (insert-button "Add Edge"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-add-edge)))
  (insert "] [")
  (insert-button "Select Focal Node"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-select-focal-node)))
  (insert "]"))


(defun cmap-buffer ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)

  (cmap-buffer-toolbar)

  (newline)
  (newline)

  (insert "Focal Node: ")
  (when *cmap-focal-node-id*
      (insert (plist-get (cdr (cmap-model-get-node *cmap-graph*
                                                   *cmap-focal-node-id*))
                         :label)))

  (newline)
  (newline)
  (insert (format "%S" *cmap-graph*))
  
  (read-only-mode)
  (goto-char 1))


(defun cmap-path-with-ext (path old-ext new-ext)
  (let* ((path-parts (s-split "\\." path))
         (filename-parts (car (-drop-last 1 path-parts))))
    (when (equal old-ext (car (last path-parts)))
      (s-join "." (list filename-parts new-ext)))))
