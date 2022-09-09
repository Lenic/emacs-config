;; Python 开发主模式
(use-package python-mode
  :defer 3)

;; LSP 自动完成服务端
(use-package lsp-pyright
  :after python-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; Python 保存自动格式化工具
(use-package python-black
  :demand t
  :after python-mode
  :hook (python-mode . python-black-on-save-mode))

(provide 'pkg-python)
