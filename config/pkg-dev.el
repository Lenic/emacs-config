;; 设置打开 NeoTree
(use-package neotree
  :after projectile
  :config
  (setq neo-theme 'ascii           ; NeoTree 图标的样式
        neo-window-fixed-size nil) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整
  :bind ("C-c o" . projectile-switch-project))

(use-package git-gutter
  :defer 3
  :config
  ;; 设置全局 Git 状态显示
  (global-git-gutter-mode t))

(use-package git-timemachine
  :defer 10)

;; 设置 Git 管理快捷键
(use-package magit
  :defer 3
  :bind ("C-x m" . magit-status)
  :config
  (setq magit-diff-refine-hunk (quote all))
  :hook ((magit-post-commit-hook) . 'git-gutter:update-all-windows))

;; 设置自动完成
(use-package company
  :defer 3
  :hook ((css-mode web-mode typescript-mode js-mode json-mode emacs-lisp-mode java-mode) . company-mode)
  :config
  (electric-pair-mode +1)
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
  (setq company-backends '((company-keywords company-files))))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (with-eval-after-load 'company
                                    (add-to-list  (make-local-variable 'company-backends) '(company-elisp)))))

;; 指定符号高亮
(use-package symbol-overlay
  :defer 3
  :bind
  (("C-c i" . symbol-overlay-put)
   ("C-c q" . symbol-overlay-remove-all)))

;; LSP 模式配置
(use-package lsp-mode
  :commands lsp
  :config
  ;; 自动清理 lsp-mode 中的内存泄露
  (defun my/lsp-client-clear-leak-handlers (lsp-client)
    "Clear leaking handlers in LSP-CLIENT."
    (let ((response-handlers (lsp--client-response-handlers lsp-client))
          to-delete-keys)
      (maphash (lambda (key value)
                 (when (> (time-convert (time-since (nth 3 value)) 'integer)
                          (* 2 lsp-response-timeout))
                   (push key to-delete-keys)))
               response-handlers)
      (when to-delete-keys
        (message "Deleting %d handlers in %s lsp-client..."
                 (length to-delete-keys)
                 (lsp--client-server-id lsp-client))
        (mapc (lambda (k) (remhash k response-handlers))
              to-delete-keys))))
  (defun my/lsp-clear-leak ()
    "Clear all leaks"
    (maphash (lambda (_ client)
               (my/lsp-client-clear-leak-handlers client))
             lsp-clients))
  (setq my/lsp-clear-leak-timer
        (run-with-timer 5 5 #'my/lsp-clear-leak))
  (add-to-list 'lsp-language-id-configuration '(".*\\.less$" . "css"))
  (setq lsp-enable-snippet nil
        lsp-eldoc-enable-hover t
        lsp-disabled-clients '(eslint)
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        ;; lsp-diagnostic-package :none
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-enable-additional-text-edit nil))

;; LSP 模式的帮助文档相关
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-enable t))

;; 加载代码折叠配置
(use-package origami
  :defer 10)

;; 代码片断自动补全工具
(use-package yasnippet
  :defer 10
  :hook ((css-mode web-mode typescript-mode js-mode json-mode java-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-reload-all))

;; 注释编辑工具
(use-package separedit
  :defer 5
  :config
  (setq separedit-default-mode 'markdown-mode))

;; 添加选区扩展功能插件
(use-package expand-region
  :defer 10
  :bind ("C-o" . er/expand-region))

;; 加载 Web 开发配置
(require 'pkg-web)

;; 加载 Java 开发配置
;; (require 'pkg-java)

;; ediff 结束后恢复到原来的布局
(use-package ediff
  :defer 10
  :ensure nil
  :hook (ediff-quit . winner-undo)
  :config
  ;; ediff 文件比对设置
  (defmacro csetq (variable value)
    `(funcall (or (get ',variable 'custom-set)
                  'set-default)
              ',variable ,value))
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)
  (csetq ediff-split-window-function 'split-window-horizontally))

(provide 'pkg-dev)
