;; Python 开发主模式
(use-package python
  :ensure nil
  :commands python-mode
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'lsp-mode)
  (require 'lsp-pyright)
  ;; 第一次启动是肯定是打开了一个文件，所以需要手动拉起 LSP 服务
  (lsp-deferred)
  ;; 第一次启动是肯定是打开了一个文件，所以需要手动加载 DAP
  (require 'dap-mode)
  (require 'dap-python)
  (use-package with-venv
    :init
    (defun dap-python--pyenv-executable-find (command)
      (with-venv (executable-find "python"))))
  (defun my/python-loader ()
    ;; Python 保存自动格式化工具
    (python-black-on-save-mode 1)
    ;; 设置使用 Tree Sitter 语法高亮
    (tree-sitter-hl-mode t)
    ;; 设置关闭自动换行
    (setq truncate-lines t)
    ;; 启动 Flycheck 语法检查
    (flycheck-mode 1)
    ;; 开启显示行号
    (display-line-numbers-mode +1)
    ;; 启动行号左侧对齐，并且不随着宽度变化而变化
    (setq display-line-numbers-width-start t)
    ;; 开启代码折叠子模式
    (hs-minor-mode t)
    ;; 设置列参考线：120
    (setq display-fill-column-indicator-column 120)
    (display-fill-column-indicator-mode t)
    ;; 开启代码折叠快捷键
    (define-key hs-minor-mode-map (kbd "C-c C-f") 'hs-toggle-hiding))
  (add-hook 'python-mode-hook 'my/python-loader))

;; LSP 自动完成服务端
(use-package lsp-pyright
  :commands python-mode)

;; Python 保存自动格式化工具
(use-package python-black
  :commands python-mode)

(provide 'pkg-python)
