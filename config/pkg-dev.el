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
  :hook ((css-mode web-mode typescript-mode js-mode json-mode emacs-lisp-mode java-mode) . company-mode)
  :config
  (electric-pair-mode +1)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
  (setq company-backends
        '((company-files company-keywords company-capf)
          (company-abbrev company-dabbrev))))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (add-to-list  (make-local-variable 'company-backends) '(company-elisp))))

;; 设置自动完成快捷键
;; (with-eval-after-load 'company
;;   (define-key company-active-map (kbd "\C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "\C-p") #'company-select-previous)
;;   (define-key company-active-map (kbd "M-n") nil)
;;   (define-key company-active-map (kbd "M-p") nil))

;; (use-package company-quickhelp
;;   :init
;;   (setq company-dabbrev-downcase nil)
;;   :config
;;   (add-hook 'company-mode-hook 'company-quickhelp-mode))

;; 设置自动完成时显示图标
;; (use-package company-box
;;   :after company
;;   :hook (company-mode . company-box-mode))

;; 指定符号高亮
(use-package symbol-overlay
  :bind
  (("C-c i" . symbol-overlay-put)
   ("C-c q" . symbol-overlay-remove-all)))

;; LSP 模式配置
(use-package lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-snippet nil
        lsp-eldoc-enable-hover t
        lsp-disabled-clients '(eslint)
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
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-enable t))

;; 加载代码折叠配置
(use-package origami)

;; 代码片断自动补全工具
(use-package yasnippet
  :hook ((css-mode web-mode typescript-mode js-mode json-mode java-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-reload-all))

;; 加载 Web 开发配置
(require 'pkg-web)

;; 加载 Java 开发配置
(require 'pkg-java)

(provide 'pkg-dev)
