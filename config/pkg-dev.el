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
        (python-mode . python-ts-mode)))

;; 项目列表选择工具
(use-package projectile
  :commands (projectile-switch-project projectile-discover-projects-in-search-path)
  :bind ("C-c o" . projectile-switch-project)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/workspace/")
        projectile-require-project-root nil
        projectile-completion-system 'ivy-completing-read
        projectile-switch-project-action 'neotree-projectile-action
        projectile-mode-line-function '(lambda () " Projectile"))
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "npm ci"
                                    :test "npm test"
                                    :run "npm run serve"
                                    :test-suffix ".spec"))

;; 设置打开 NeoTree 树形列表展示
(use-package neotree
  :commands neotree-dir
  :config
  (setq neo-theme 'ascii           ; NeoTree 图标的样式
        neo-window-width 35
        neo-window-fixed-size nil)) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整

;; 在文件左侧显示 Git 状态
(use-package git-gutter
  :commands git-gutter-mode)

;; 当前文件的修改历史展示
(use-package git-timemachine
  :commands git-timemachine)

;; 设置 Git 管理快捷键
(use-package magit
  :bind ("C-x m" . magit-status)
  :config
  (setq magit-diff-refine-hunk (quote all))
  ;; use-package 的 :hook 会自动补 `-hook' 后缀，这里写 magit-post-commit 即可；
  ;; 写成 magit-post-commit-hook 会挂到并不存在的 magit-post-commit-hook-hook 上
  :hook (magit-post-commit . git-gutter:update-all-windows))

;; 指定符号高亮
(use-package symbol-overlay
  :commands symbol-overlay-put
  :bind
  (("C-c i" . symbol-overlay-put)
   ("C-c q" . symbol-overlay-remove-all)))

;; 加载代码折叠配置：支持 HTML 标签的折叠
(use-package yafolding
  :commands (yafolding-mode)
  :bind (("M-RET" . yafolding-toggle-element)))

;; 代码片断自动补全工具
(use-package yasnippet
  :commands yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
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
          (when-let* ((node (treesit-node-parent node)))
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
  ;; 调试暂停时 dap-ui-many-windows-mode 弹出哪些窗口：只显示局部变量和断点。
  ;; 这个变量名带 auto-configure，但 dap-ui-many-windows-mode 也读取它，
  ;; 关闭 dap-auto-configure-mode 后仍然生效
  (setq dap-auto-configure-features '(locals breakpoints controls))
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-many-windows-mode 1)
  (require 'dap-hydra)
  ;; 启动调试后自动弹出 dap-hydra 操作面板
  (defun my/dap-show-hydra (&rest _)
    "在 `dap-debug' 之后显示 `dap-hydra'。"
    (dap-hydra))
  (advice-add #'dap-debug :after #'my/dap-show-hydra))

;; 变量命名转换
(use-package string-inflection
  :commands
  (string-inflection-kebab-case
   string-inflection-lower-camelcase
   string-inflection-camelcase
   string-inflection-underscore
   string-inflection-upcase
   string-inflection-all-cycle))

;; 加载 Web 开发配置
(require 'pkg-web)

;; ediff 结束后恢复到原来的布局
(use-package ediff
  :commands ediff
  :ensure nil
  :config
  ;; 必须挂在 ediff-after-quit-hook-internal 上：ediff-quit-hook 里的
  ;; ediff-cleanup-mess 会在之后继续清理窗口，提前恢复的布局会被打乱
  (add-hook 'ediff-after-quit-hook-internal #'winner-undo)
  ;; ediff 文件比对设置
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally))

;; Elisp 模式的必要设置
(defun my/elisp-mode-setup ()
  "Elisp 模式的 buffer 局部设置。"
  ;; 在文件左侧显示 Git 状态
  (git-gutter-mode 1)
  ;; 设置关闭自动换行
  (setq truncate-lines t)
  ;; 显示行号
  (display-line-numbers-mode 1)
  ;; 启动代码折叠功能
  (yafolding-mode 1))
(add-hook 'emacs-lisp-mode-hook #'my/elisp-mode-setup)

;; 加载 lsp 配置
(require 'pkg-lsp)

(provide 'pkg-dev)
