;; -*- lexical-binding: t -*-

;; 添加 treesit 语言配置
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode)
  :init
  ;; 设置代码高亮力度和现代编辑器相同，比如 VSCode
  (setq treesit-font-lock-level 4))

;; 设置 Major Mode 的自动映射
(setq major-mode-remap-alist
      '((js-mode . js-ts-mode)
        (typescript-mode . tsx-ts-mode)
        (typescript-ts-mode . tsx-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (less-css-mode . css-ts-mode)
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
  :bind ("C-o" . er/expand-region)
  :config
  (when (treesit-available-p)
    (defun my/treesit-mark-bigger-node ()
      "https://emacs-china.org/t/treesit-expand-region-el/23406"
      (let* ((root (treesit-buffer-root-node))
             (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
             (node-start (treesit-node-start node))
             (node-end (treesit-node-end node)))
        ;; Node fits the region exactly. Try its parent node instead.
        (when (and (= (region-beginning) node-start) (= (region-end) node-end))
          (when-let ((node (treesit-node-parent node)))
            (setq node-start (treesit-node-start node)
                  node-end (treesit-node-end node))))
        (set-mark node-end)
        (goto-char node-start)))

    (add-to-list 'er/try-expand-list 'my/treesit-mark-bigger-node)))

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
                                  ;; (company-mode 1)
                                  ;; 在文件左侧显示 Git 状态
                                  (git-gutter-mode 1)
                                  ;; 设置关闭自动换行
                                  (setq truncate-lines t)
                                  ;; 显示行号
                                  (display-line-numbers-mode 1)
                                  ;; 启动代码折叠功能
                                  (yafolding-mode 1)
                                  ;; 为 company 的自动完成列表添加 Elisp 自身的配置
                                  ;; (add-to-list  (make-local-variable 'company-backends) '(company-elisp))
                                  ))

(use-package csharp-mode
  :ensure nil
  :commands csharp-mode
  :config
  (use-package dap-unity
    :ensure nil))

;; 加载 lsp 配置
(require 'pkg-lsp)

(provide 'pkg-dev)
