;; Setup load paths
(add-to-list 'load-path ".")
(add-to-list 'load-path "tests")

;; Load required libraries
(require 'ert)
(require 'dash)
(require 's)
(require 'cl-lib)

;; Load basic test modules
(require 'test-el-cmap-model)
(require 'test-el-cmap-repr)
(require 'test-el-cmap-buffer)

;; Load enhanced test modules with reduced side effects
(require 'test-el-cmap-model-enhanced)
(require 'test-el-cmap-repr-enhanced)
(require 'test-el-cmap-buffer-enhanced)

;; Load pure functional model tests
(require 'test-el-cmap-model-pure)

;; Run all tests
(ert t)
