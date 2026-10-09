;; -*- lexical-binding: t -*-

(use-package markdown-mode
  :commands markdown-mode)

(use-package dockerfile-mode
  :commands dockerfile-mode)

(use-package yaml-mode
  :commands yaml-mode)

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook
  (csv-mode . csv-align-mode))

(provide 'pkg-lang)
