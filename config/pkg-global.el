;; 窗口快捷跳转操作
(use-package ace-window
  :commands ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x i" . ace-window))

;; 和系统剪切板相关设置
(use-package xclip
  :defer 5
  :config
  (xclip-mode))

;; 全局基础配置
(use-package counsel
  :commands (swiper-isearch counsel-M-x counsel-ibuffer counsel-find-file counsel-rg counsel-fzf counsel-file-jump ivy)
  :config
  ;; 设置 counsel-fzf 命令使用 rg 作为核心输出端
  (setq counsel-fzf-cmd "rg -l -L --glob '!.git' --hidden . | fzf -f \"%s\"")
  ;; 设置 counsel-rg 命令同样搜索快捷方式内部的内容
  ;; (push "--follow" (cdr (nthcdr 0 counsel-rg-base-command)))
  (setq counsel-rg-base-command
        "rg --max-columns 300 --with-filename --no-heading --line-number --color never --follow %s")
  ;; 设置输入两个字符后就开始执行匹配
  (custom-set-variables
   '(ivy-more-chars-alist '((counsel-grep . 2) (t . 2))))
  :bind
  ;; swiper 配置
  ("C-s" . swiper-isearch)
  ;; 替换命令执行
  ("M-x" . counsel-M-x)
  ;; 替换 Buffer 界面
  ("C-x C-b" . counsel-ibuffer)
  ;; 替换打开文件
  ("C-x C-f" . counsel-find-file)
  ;; 设置 RG 全文搜索
  ("C-c k" . counsel-rg)
  ;; 设置项目下的文件名查找
  ("C-c p" . counsel-fzf)
  ;; 设置查找特定目录下的文件名查找
  ("C-c f" . counsel-file-jump))

;; 命令使用最近使用方式排序
(use-package amx
  :after counsel)

;; 项目内使用 rg 快速查找
(use-package rg
  :commands rg-menu
  :config
  (rg-enable-default-bindings)
  :bind ("C-c s" . rg-menu))

;; 对于查找结果的快速编辑功能
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

;; 多光标编辑功能
(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-all-like-this)
  :defer 10)

;; 自动撤销树
(use-package undo-tree
  :defer 3
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/undo-tree")))
  (undo-tree-visualizer-timestamps t))

;; 设置 Ace-Jump
(use-package ace-jump-mode
  :commands (ace-jump-word-mode ace-jump-line-mode)
  :bind
  ("C-c j" . ace-jump-word-mode)
  ("C-c l" .  ace-jump-line-mode)
  :config
  (setq ace-jump-mode-scope 'window))

;; 显示行尾空白字符
(use-package whitespace
  :defer 10
  :ensure nil
  :config
  (setq whitespace-style '(face trailing)
        whitespace-global-modes '(not markdown-mode))
  :hook (((web-mode typescript-mode emacs-lisp-mode) . whitespace-mode)
         (before-save . (lambda () (progn
                                     (untabify (point-min) (point-max))
                                     (whitespace-cleanup))))))

;; 处理特别长的行，避免带来一些性能问题
(use-package so-long
  :defer 10
  :ensure nil
  :config (global-so-long-mode 1))

;; 可以正常处理驼峰单词了：使用 M-f/b 时在每个驼峰单词之间停顿
(use-package subword
  :defer 10
  :ensure nil
  :hook (after-init . global-subword-mode))

;; 光标定位高亮
(use-package beacon
  :defer 10
  :init
  (beacon-mode t))

;; 开启全局窗口变动记录
(use-package winner-mode
  :defer 1
  :ensure nil
  :hook (after-init . winner-mode)
  :config (setq winner-dont-bind-my-keys nil))

;; 拷贝当前 Buffer 到剪切板
(defun copy-buffer-path ()
  (interactive)
  (if (equal buffer-file-name nil)
      (message "没有文件名")
    (setq path (vc-find-root buffer-file-name ".git"))
    (if (equal path nil)
        (kill-new (message buffer-file-name))
      (setq absolutePath (expand-file-name path))
      (setq targetPath (substring buffer-file-name (length absolutePath)))
      (kill-new (message targetPath)))))
(global-set-key (kbd "C-c C-p") 'copy-buffer-path)

(provide 'pkg-global)
