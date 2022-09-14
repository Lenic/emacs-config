;; Python 开发主模式
(use-package python-mode
  :demand t
  :after (lsp-mode dap-mode)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
  (use-package with-venv
    :init
    (defun dap-python--pyenv-executable-find (command)
      (with-venv (executable-find "python"))))
  (add-hook 'python-mode-hook
            (lambda ()
              ;; 设置关闭自动换行
              (setq truncate-lines t)
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
              (define-key hs-minor-mode-map (kbd "C-c C-f") 'hs-toggle-hiding))))

;; LSP 自动完成服务端
(use-package lsp-pyright
  :after python-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

;; Python 保存自动格式化工具
(use-package python-black
  :demand t
  :after python-mode
  :hook (python-mode . python-black-on-save-mode))

(provide 'pkg-python)
