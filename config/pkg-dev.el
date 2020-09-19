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
  :hook ((css-mode web-mode vue-mode typescript-mode js-mode json-mode emacs-lisp-mode java-mode) . company-mode))

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
  :hook ((css-mode web-mode vue-mode typescript-mode js-mode json-mode java-mode) . autopair-mode))

;; ;; 语法检查包
;; (use-package flycheck
;;   :hook ((css-mode web-mode typescript-mode js-mode json-mode java-mode) . flycheck-mode)
;;   :config
;;   (defun my/use-eslint-from-node-modules ()
;;     (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
;;     (let* ((root (locate-dominating-file
;;                   (or (buffer-file-name) default-directory)
;;                   "node_modules"))
;;            (eslint
;;             (and root
;;                  (expand-file-name "node_modules/.bin/eslint"
;;                                    root))))
;;       (when (and eslint (file-executable-p eslint))
;;         (setq-local flycheck-javascript-eslint-executable eslint))))
;;   (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

;; LSP 模式配置
(use-package lsp-mode
  :commands lsp
  :hook ((java-mode vue-mode) . lsp)
  :config
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-completion-enable-additional-text-edit nil))

;; LSP 模式的帮助文档相关
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-enable-symbol-highlighting nil))

;; 加载 Web 开发配置
(require 'pkg-web)

;; 加载 Java 开发配置
;; (require 'pkg-java)

(provide 'pkg-dev)
