;; -*- lexical-binding: t -*-

;; 窗口快捷跳转操作
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x i" . ace-window))

;; 和系统剪切板相关设置
(use-package xclip
  :defer 5
  :config
  (xclip-mode))

;; 自定义一个找项目根目录的函数
(defun my/counsel-fzf-project-root ()
  "优先寻找 package.json，找不到则使用 project.el 识别的项目根目录，作为 counsel-fzf 的搜索根目录。"
  (interactive)
  (let* ((project (project-current))
         (root (or (locate-dominating-file default-directory "package.json")
                   (and project (project-root project))
                   default-directory))) ; 如果都找不到，则使用当前目录
    (counsel-fzf nil root)))

;; 全局基础配置
(use-package counsel
  :commands (swiper-isearch counsel-M-x counsel-ibuffer counsel-find-file counsel-rg counsel-fzf counsel-file-jump)
  :config
  ;; 设置 counsel-fzf 命令使用 rg 作为核心输出端
  (setq counsel-fzf-cmd "rg -l -L --glob '!.git' --hidden . | fzf -f \"%s\"")
  ;; 设置 counsel-rg 命令同样搜索快捷方式内部的内容
  ;; (push "--follow" (cdr (nthcdr 0 counsel-rg-base-command)))
  (setq counsel-rg-base-command
        '("rg" "--max-columns" "500" "--with-filename" "--no-heading" "--line-number" "--color" "never" "--follow" "%s"))
  :custom
  ;; 设置输入两个字符后就开始执行匹配
  (ivy-more-chars-alist '((counsel-grep . 2) (t . 2)))
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
  ("C-c p" . my/counsel-fzf-project-root)
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

;; 自动撤销树：启动 3 秒后再加载
(use-package undo-tree
  :defer 3
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "var/undo-tree/" user-emacs-directory))))
  (undo-tree-visualizer-timestamps t)
  :config
  (make-directory (expand-file-name "var/undo-tree/" user-emacs-directory) t) ; 确保目录存在
  (global-undo-tree-mode))

;; Jump to arbitrary positions
(use-package avy
  ;; integrate with isearch and others
  :bind (("C-c l" . avy-goto-line)
         ("C-c j" . avy-goto-char-timer))
  :config
  ;; change the highlight font color and background color
  ;; (set-face-attribute 'avy-lead-face nil :background "black" :foreground "red")
  ;; (set-face-attribute 'avy-lead-face-0 nil :background "black" :foreground "red")
  :custom
  (avy-background t)
  (avy-all-windows t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  ;; overlay is used during isearch, `pre' style makes avy keys evident.
  (avy-styles-alist '((avy-isearch . pre))))

;;;; 保存前格式化
;;
;; 所有格式化都由同一个 buffer 局部的 `before-save-hook' 函数统一执行，
;; 一次保存完成全部格式化，执行顺序固定由 `my/save-formatters' 决定。
;; 每个格式化是否执行，看当前 buffer 里对应的开关变量是否开启：
;; ESLint、Prettier 的开关就是它们的 minor mode，可以用 M-x 随时手动切换。

(defvar my/save-formatters
  '((my/whitespace-cleanup-on-save . whitespace-cleanup)
    (eslintd-fix-on-save-mode      . eslintd-fix-buffer)
    (prettier-js-mode              . prettier-js-prettify))
  "保存前格式化列表，每一项是 (开关变量 . 格式化函数)。
按列表顺序执行：先清理行尾空白，再 ESLint 修复，最后 Prettier 排版。")

(defvar-local my/whitespace-cleanup-on-save nil
  "非 nil 时，保存前清理当前 buffer 的行尾空白。")

(defun my/run-save-formatters ()
  "按 `my/save-formatters' 的顺序，执行当前 buffer 中已开启的格式化。
单个格式化失败只提示，不中断后续格式化，也不阻止保存。"
  (pcase-dolist (`(,switch . ,formatter) my/save-formatters)
    (when (and (boundp switch) (symbol-value switch))
      (condition-case err
          (funcall formatter)
        (error (message "%s 执行失败：%s" formatter (error-message-string err)))))))

(defun my/enable-save-formatters ()
  "在当前 buffer 挂上统一的保存前格式化。
只挂到当前 buffer：Makefile、Go 等其它文件不受影响。"
  (add-hook 'before-save-hook #'my/run-save-formatters nil t))

;; 显示行尾空白字符，并在保存前清理
(defun my/enable-whitespace-cleanup ()
  "开启 `whitespace-mode'，并在保存前清理行尾空白。"
  (whitespace-mode 1)
  (setq my/whitespace-cleanup-on-save t)
  (my/enable-save-formatters))

(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-style '(face trailing))
  :hook ((web-mode tsx-ts-mode emacs-lisp-mode) . my/enable-whitespace-cleanup))

;; 处理特别长的行，避免带来一些性能问题
(use-package so-long
  :defer 10
  :ensure nil
  :config (global-so-long-mode 1))

;; 可以正常处理驼峰单词了：使用 M-f/b 时在每个驼峰单词之间停顿
(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))

;; 光标定位高亮：启动 10 秒后再加载
(use-package beacon
  :defer 10
  :config
  (beacon-mode t))

;; 开启全局窗口变动记录
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

;; ERC 配置
(use-package erc
  :commands erc
  :ensure nil
  :config
  ;; Interpret mIRC-style color commands in IRC chats
  (setq erc-interpret-mirc-color t)
  ;; Kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)
  ;; Kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)
  ;; Kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t))

;; 拷贝当前 Buffer 到剪切板
(defun my/copy-buffer-path ()
  "把当前文件相对于项目根目录的路径拷贝到剪贴板，不在项目中时拷贝绝对路径。"
  (interactive)
  (if (not buffer-file-name)
      (message "没有文件名")
    (let* ((project (project-current))
           (target-path (if project
                            (file-relative-name buffer-file-name
                                                (project-root project))
                          buffer-file-name)))
      (kill-new target-path)
      ;; 必须走 %s：路径里出现 % 时，直接把它当格式串会报 format 错误
      (message "%s" target-path))))
(global-set-key (kbd "C-c C-p") #'my/copy-buffer-path)

(provide 'pkg-global)
