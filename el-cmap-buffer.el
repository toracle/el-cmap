(defun cmap-buffer-save ()
  (interactive)
  (unless cmap-path
    (setq-local cmap-path (read-file-name "Select a file to save: ")))
  (cmap-save cmap-graph cmap-path))


(defun cmap-buffer-load ()
  (interactive)
  (setq-local cmap-path (read-file-name "Select a file to save: "))
  (setq-local cmap-graph (cmap-load cmap-path))
  (cmap-buffer))


(defun cmap-buffer-add-node ()
  (interactive)
  (read-string "Input node name: "))


(defun cmap-buffer-add-edge ()
  (interactive)
  (read-string "Input neighbor edge: ")
  (read-string "Input edge label: "))


(defun cmap-buffer-select-focal-node ()
  (interactive)
  (read-string "Select node: ")
  )


(defun cmap-buffer ()
  (interactive)
  (read-only-mode -1)
  (erase-buffer)

  (insert "[")
  (insert-button "Load"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-load)))
  (insert "] [")
  (insert-button "Save"
                 'follow-link "\C-m"
                 'action '(lambda (button)
                            (cmap-buffer-save)))
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
  (insert "]")

  (newline)
  (newline)

  (insert (format "%S" cmap-graph))
  
  (read-only-mode)
  (goto-char 1))
