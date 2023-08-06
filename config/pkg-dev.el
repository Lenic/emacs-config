;; 添加 treesit 语言配置
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (vue "https://github.com/ikatyang/tree-sitter-vue")))

(setq major-mode-remap-alist
      '((js-mode . js-ts-mode)
        (typescript-mode . tsx-ts-mode)
        (typescript-ts-mode . tsx-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

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
        neo-window-width 35
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
(use-package company
  :commands company-mode
  :config
  (electric-pair-mode +1)
  (setq company-idle-delay 0.5)
  (setq company-format-margin-function nil) ;; 移除自动补全时最前面的图标
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
  (setq company-backends '((company-keywords company-files))))

;; 指定符号高亮
(use-package symbol-overlay
  :commands symbol-overlay-put
  :bind
  (("C-c i" . symbol-overlay-put)
   ("C-c q" . symbol-overlay-remove-all)))

;; LSP 模式配置
(use-package lsp-mode
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
  ;; (add-to-list 'lsp-language-id-configuration '(".*\\.less$" . "css"))
  (setq lsp-enable-snippet nil
        lsp-lens-enable nil
        ;; 显示完整的 eldoc 信息
        lsp-eldoc-render-all t
        ;; 关闭 lsp 服务退出时的重启提示
        lsp-restart 'ignore
        lsp-eldoc-enable-hover t
        lsp-disabled-clients '(eslint)
        lsp-signature-auto-activate t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-signature-render-documentation t
        lsp-completion-show-detail t
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
        lsp-completion-enable-additional-text-edit nil))

;; LSP 模式的帮助文档相关
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-delay 1)
  (setq lsp-ui-sideline-enable t))

;; 加载代码折叠配置：支持 HTML 标签的折叠
(use-package yafolding
  :commands (yafolding-mode))

;; 代码片断自动补全工具
(use-package yasnippet
  :commands yas-minor-mode
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
                                  ;; 加载 Company 显示自动完成列表
                                  (company-mode 1)
                                  ;; 在文件左侧显示 Git 状态
                                  (git-gutter-mode 1)
                                  ;; 设置关闭自动换行
                                  (setq truncate-lines t)
                                  ;; 显示行号
                                  (display-line-numbers-mode 1)
                                  ;; 启动代码折叠功能
                                  (yafolding-mode 1)
                                  ;; 为 company 的自动完成列表添加 Elisp 自身的配置
                                  (add-to-list  (make-local-variable 'company-backends) '(company-elisp))))

(provide 'pkg-dev)
