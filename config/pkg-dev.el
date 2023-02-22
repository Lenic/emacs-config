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
;; (require 'pkg-python)

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
                                  ;; 在文件左侧显示 Git 状态
                                  (git-gutter-mode 1)
                                  ;; 设置关闭自动换行
                                  (setq truncate-lines t)
                                  ;; 显示行号
                                  (display-line-numbers-mode 1)
                                  ;; 启动代码折叠功能
                                  (yafolding-mode 1)))

(provide 'pkg-dev)

