;; 设置保存后自动格式化代码
(use-package prettier-js
  :ensure t
  :hook ((css-mode web-mode typescript-mode js-mode rjsx-mode json-mode) . prettier-js-mode))

;; 自动补全括号
(use-package autopair
  :ensure t
  :hook ((css-mode web-mode typescript-mode js-mode rjsx-mode json-mode) . autopair-mode))

;; 设置自动完成
(use-package company
  :ensure t
  :custom (company-tooltip-align-annotations t "aligns annotation to the right hand side")
  :hook ((css-mode web-mode typescript-mode js-mode rjsx-mode json-mode emacs-lisp-mode) . company-mode))

;; 设置 CSS 及其它 CSS 预处理语言
(add-hook 'css-mode-hook
          (lambda ()
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)
            ;; 设置关闭自动换行
            (setq truncate-lines t)
            ;; 开启显示行号
            (display-line-numbers-mode +1)))

;; 开启代码折叠子模式
(use-package hs-minor-mode
  :hook ((css-mode web-mode typescript-mode js-mode rjsx-mode json-mode) . hs-minor-mode)
  :bind
  ;; 开启代码折叠快捷键
  ("s-/" . hs-toggle-hiding))

;; LSP 模式的 JS 自动完成配置
(use-package tide
  :ensure t
  :config (tide-hl-identifier-mode +1)
  :hook
  ((web-mode typescript-mode js-mode rjsx-mode) .
   (lambda ()
     ;; Tide 安装
     (tide-setup)
     ;; 当 tsserver 服务没有启动时自动重新启动
     (unless (tide-current-server) (tide-restart-server)))))

;; 快速编写 HTML 代码
(use-package emmet-mode
  :ensure t
  :custom (emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode rjsx-mode))

;; 设置打开 NeoTree
(use-package neotree
  :ensure t
  :bind ("C-x o" . neotree-dir))

;; 设置 Git 管理快捷键
(use-package magit
  :ensure t
  :bind ("C-x m" . magit-status))

;; 附加 Web 开发的各种插件
(defun web-dev-attached ()
  ;; 设置关闭自动换行
  (setq truncate-lines t)
  ;; 开启显示行号
  (display-line-numbers-mode +1))

;; 加载 Web-Mode
(require 'pkg-web-mode)

;; 加载 TypeScript-Mode
(require 'pkg-typescript-mode)

;; 加载 JSON 格式：js-mode
(require 'pkg-js-mode)

(provide 'pkg-web)
