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

(use-package company-quickhelp
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 1)
  :config
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

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

;; LSP 模式配置
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-snippet nil
        lsp-eldoc-enable-hover t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-diagnostic-package :none
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-enable-additional-text-edit nil))

;; LSP 模式的帮助文档相关
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil))

;; 加载 Web 开发配置
(require 'pkg-web)

;; 加载 Java 开发配置
;; (require 'pkg-java)

(provide 'pkg-dev)
