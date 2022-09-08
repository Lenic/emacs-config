;; Python 开发主模式
(use-package python-mode
  :defer 3
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              ;; 开启 LSP 模式自动完成
              (lsp)
              (setq lsp-pylsp-plugins-flake8-enabled nil)
              ;; 设置 pylsp 文件搜索目录
              (setq lsp-clients-pylsp-library-directories "/Users/leixuewei/Library/Python/3.8/bin")
              ;; 设置自动缩进的宽度
              (setq-default indent-tabs-mode t)
              (setq-default tab-width 4)
              (setq-default py-indent-tabs-mode t)
              ;; (setq-default flycheck-disabled-checkers '(python-flake8 python-pylint python-pycompile python-pyright python-mypy))
              (setq-default flycheck-disabled-checkers '(lsp))
              (add-to-list 'write-file-functions 'delete-trailing-whitespace))))

;; Python 保存自动格式化工具
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(provide 'pkg-python)
