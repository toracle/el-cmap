
;; Required libraries
(require 'dash)
(require 's)

;; Load the module to test
(require 'el-cmap-model)
(require 'el-cmap-buffer)

(ert-deftest cmap-path-with-ext-standard-case ()
  "Test normal case of extension conversion."
  (should (equal (cmap-path-with-ext "/home/user/graph.el" "el" "png")
                 "/home/user/graph.png")))

(ert-deftest cmap-path-with-ext-different-ext ()
  "Test when file has a different extension than expected."
  (should (equal (cmap-path-with-ext "/home/user/graph.txt" "el" "png")
                 "/home/user/graph.txt.png")))

(ert-deftest cmap-path-with-ext-no-ext ()
  "Test when file has no extension."
  (should (equal (cmap-path-with-ext "/home/user/graph" "el" "png")
                 "/home/user/graph.png")))

(ert-deftest cmap-path-with-ext-multiple-dots ()
  "Test when file has multiple dots in the name."
  (should (equal (cmap-path-with-ext "/home/user/my.graph.el" "el" "png")
                 "/home/user/my.graph.png")))

(provide 'test-el-cmap-buffer)
