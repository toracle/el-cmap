
(ert-deftest cmap-path-with-ext ()
  (should (equal (cmap-path-with-ext "/home/user/graph.el" "el" "png")
                 "/home/user/graph.png")))
