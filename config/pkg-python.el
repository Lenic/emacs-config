;; Python 开发主模式
(use-package python
  :ensure nil
  :commands python-ts-mode
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  (setq python-indent-offset 4)
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
    ;; 连接 LSP 服务
    (lsp-deferred)
    ;; 自动补全括号
    (electric-pair-mode t)
    ;; 设置关闭自动换行
    (setq truncate-lines t)
    ;; 启动 Flycheck 语法检查
    (flycheck-mode 1)
    ;; 开启显示行号
    (display-line-numbers-mode +1)
    ;; 开启文件左侧 Git 变更状态标识
    (git-gutter-mode 1)
    ;; 启动行号左侧对齐，并且不随着宽度变化而变化
    (setq display-line-numbers-width-start t)
    ;; 启动代码折叠功能
    (yafolding-mode 1)
    ;; 设置列参考线：120
    (setq display-fill-column-indicator-column 120)
    (display-fill-column-indicator-mode t))
  (add-hook 'python-ts-mode-hook 'my/python-loader))

;; LSP 自动完成服务端
(use-package lsp-pyright
  :commands python-ts-mode)

;; Python 保存自动格式化工具
(use-package python-black
  :commands python-ts-mode)

(provide 'pkg-python)
