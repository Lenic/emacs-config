;; 项目列表选择工具
(use-package projectile
  :commands (projectile-switch-project projectile-discover-projects-in-search-path)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/workspace/")
        projectile-require-project-root nil
        projectile-completion-system 'ivy
        projectile-switch-project-action 'neotree-projectile-action
        projectile-mode-line-function '(lambda () " Projectile"))
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "npm ci"
                                    :test "npm test"
                                    :run "npm run serve"
                                    :test-suffix ".spec"))

;; 在 swiper 中仍然可以输入中文，只不过换成了 M-i 这个快捷键
(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "M-i") 'pyim-convert-string-at-point))

;; 设置打开 NeoTree 树形列表展示
(use-package neotree
  :commands projectile-switch-project
  :config
  (setq neo-theme 'ascii           ; NeoTree 图标的样式
        neo-window-fixed-size nil) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整
  :bind ("C-c o" . projectile-switch-project))

;; 在文件左侧显示 Git 状态
(use-package git-gutter
  :commands git-gutter-mode)

;; 当前文件的修改历史展示
(use-package git-timemachine
  :commands git-timemachine)

;; 设置 Git 管理快捷键
(use-package magit
  :commands magit-status
  :bind ("C-x m" . magit-status)
  :config
  (setq magit-diff-refine-hunk (quote all))
  :hook ((magit-post-commit-hook) . 'git-gutter:update-all-windows))

;; 设置自动完成
(use-package corfu
  :after orderless
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)        ;; Quit, if there is no match
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; 添加结构化 AST 语法高亮
(use-package tree-sitter
  :commands (tree-sitter-mode tree-sitter-hl-mode)
  :hook
  (js-mode . tree-sitter-hl-mode)
  (typescript-mode . tree-sitter-hl-mode)
  (python-mode . tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx)))

;; 指定符号高亮
(use-package symbol-overlay
  :commands symbol-overlay-put
  :bind
  (("C-c i" . symbol-overlay-put)
   ("C-c q" . symbol-overlay-remove-all)))

(use-package cape)

;; LSP 模式配置
(use-package lsp-mode
  :after orderless
  :commands (lsp lsp-deferred)
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
  (setq lsp-enable-snippet nil
        lsp-lens-enable nil
        ;; 关闭 lsp 服务退出时的重启提示
        lsp-restart 'ignore
        lsp-eldoc-enable-hover t
        lsp-disabled-clients '(eslint)
        lsp-signature-auto-activate t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-signature-render-documentation t
        lsp-completion-show-detail nil
        lsp-completion-show-kind t
        ;; lsp-diagnostic-package :none
        lsp-diagnostic-package :flycheck
        ;; 关闭文件监视
        lsp-enable-file-watchers nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-dap-auto-configure nil
        ;; 关闭 flycheck 实时语法检查
        lsp-flycheck-live-reporting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-enable-additional-text-edit nil)
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; LSP 模式的帮助文档相关
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-enable t))

;; 加载代码折叠配置：支持 HTML 标签的折叠
(use-package yafolding
  :commands (yafolding-mode))

;; 代码片断自动补全工具
(use-package yasnippet
  :commands yas-minor-mode
  ;; :hook ((css-mode web-mode typescript-mode js-mode json-mode java-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))

;; 注释编辑工具
(use-package separedit
  :commands separedit
  :config
  (setq separedit-default-mode 'markdown-mode))

;; 添加选区扩展功能插件
(use-package expand-region
  :commands (er/expand-region er/mark-word)
  :bind ("C-o" . er/expand-region))

;; DAP
(use-package dap-mode
  :commands (dap-debug dap-breakpoint-toggle)
  :config
  (dap-auto-configure-mode -1)
  (setq dap-auto-configure-features '(locals breakpoints controls))
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-many-windows-mode 1)
  (require 'dap-hydra))

;;;###autoload
(defun +dap-debug-a (&rest _)
  (dap-hydra))
(advice-add #'dap-debug :after #'+dap-debug-a)

;; 加载 Web 开发配置
(require 'pkg-web)

;; 加载 Python 开发配置
(require 'pkg-python)

;; 加载 Java 开发配置
;; (require 'pkg-java)

;; ediff 结束后恢复到原来的布局
(use-package ediff
  :commands ediff
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

;; Elisp 模式的必要设置
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq-local corfu-auto t)
                                  (corfu-mode)
                                  ;; 在文件左侧显示 Git 状态
                                  (git-gutter-mode 1)
                                  ;; 设置关闭自动换行
                                  (setq truncate-lines t)
                                  ;; 显示行号
                                  (display-line-numbers-mode 1)
                                  ;; 启动代码折叠功能
                                  (yafolding-mode 1)))

(provide 'pkg-dev)
