;; Python 开发主模式
(use-package python
  :ensure nil
  :commands python-mode
  :custom
  (setq python-indent-offset 4)
  :config
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
    ;; 开启文件左侧 Git 变更状态标识
    (git-gutter-mode 1)
    ;; 启动行号左侧对齐，并且不随着宽度变化而变化
    (setq display-line-numbers-width-start t)
    ;; 启动代码折叠功能
    (yafolding-mode 1)
    ;; 设置列参考线：120
    (setq display-fill-column-indicator-column 120)
    (display-fill-column-indicator-mode t))
  (add-hook 'python-mode-hook 'my/python-loader))

;; Python 保存自动格式化工具
(use-package python-black
  :commands python-mode)

(provide 'pkg-python)
