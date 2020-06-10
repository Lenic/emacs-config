;; 设置保存后自动格式化代码
(use-package prettier-js
  :hook ((css-mode web-mode vue-mode typescript-mode js-mode rjsx-mode json-mode) . prettier-js-mode))

;; 自动补全括号
(use-package autopair
  :hook ((css-mode web-mode vue-mode typescript-mode js-mode rjsx-mode json-mode) . autopair-mode))

;; 设置自动完成
(use-package company
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  :hook ((css-mode web-mode vue-mode typescript-mode js-mode rjsx-mode json-mode emacs-lisp-mode) . company-mode))

;; LSP 模式的 JS 自动完成配置
(use-package tide
  :config (tide-hl-identifier-mode +1)
  :hook
  ((web-mode typescript-mode rjsx-mode) .
   (lambda ()
     ;; Tide 安装
     (tide-setup)
     ;; 当 tsserver 服务没有启动时自动重新启动
     (unless (tide-current-server) (tide-restart-server)))))

;; 快速编写 HTML 代码
(use-package emmet-mode
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode vue-mode typescript-mode js-mode rjsx-mode))

;; 设置打开 NeoTree
(use-package neotree
  :bind ("C-x o" . neotree-dir))

;; 设置 Git 管理快捷键
(use-package magit
  :bind ("C-x m" . magit-status))

;; 附加 Web 开发的各种插件
(defun web-dev-attached ()
  ;; 设置关闭自动换行
  (setq truncate-lines t)
  ;; 开启显示行号
  (display-line-numbers-mode +1)
  ;; 开启代码折叠子模式
  (hs-minor-mode +1)
  ;; 开启代码折叠快捷键
  (global-set-key (kbd "s-/") 'hs-toggle-hiding))

;; 设置 CSS 及其它 CSS 预处理语言
(add-hook 'css-mode-hook
          (lambda ()
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)
            ;; 其它开发设置
            (web-dev-attached)))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 设置自动缩进的宽度
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)
              ;; 其它开发设置
              (web-dev-attached))))

(use-package web-mode
  :mode "\\.jsx?\\'"
  :init
  ;; 设置语法高亮模式
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  ;; 设置默认的缩进
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2)
  :config
  (add-hook 'web-mode-hook 'web-dev-attached))

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (setq javascript-indent-level 2)
  (setq js-indent-level 2)
  (add-hook 'vue-mode-hook 'web-dev-attached))

;; 加载 TypeScript-Mode
;; (use-package rjsx-mode
;;   :mode "\\.tsx\\'"
;;   :init
;;   ;; 设置缩进两个空格
;;   (setq js2-basic-offset 2)
;;   :config
;;   (add-hook 'rjsx-mode-hook 'web-dev-attached))

(use-package typescript-mode
  :mode "\\.ts[x]?\\'"
  :init
  ;; 设置缩进两个空格
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook 'web-dev-attached))

(provide 'pkg-web)
