;; 设置打开 NeoTree
(use-package neotree
  :config
  (setq neo-theme 'ascii)
  :bind ("C-c o" . neotree-dir))

(use-package git-gutter
  :config
  ;; 设置全局 Git 状态显示
  (global-git-gutter-mode t))

;; 设置 Git 管理快捷键
(use-package magit
  :bind ("C-x m" . magit-status)
  :hook ((magit-post-commit-hook) . 'git-gutter:update-all-windows))

;; 设置自动完成
(use-package company
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  :hook ((css-mode web-mode typescript-mode js-mode json-mode emacs-lisp-mode java-mode) . company-mode))

;; 设置自动完成时显示图标
;; (use-package company-box
;;   :after company
;;   :hook (company-mode . company-box-mode))

;; 指定符号高亮
(use-package symbol-overlay
  :bind
  (("C-c i" . symbol-overlay-put)
   ("C-c q" . symbol-overlay-remove-all)))

;; 自动补全括号
(use-package autopair
  :hook ((css-mode web-mode typescript-mode js-mode json-mode java-mode) . autopair-mode))

;; 加载 Web 开发配置
(require 'pkg-web)

;; 加载 Java 开发配置
;; (require 'pkg-java)

(provide 'pkg-dev)
