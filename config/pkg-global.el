;; -*- lexical-binding: t -*-

;; 窗口快捷跳转操作
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x i" . ace-window))

;; 和系统剪切板相关设置
;; 通过 SSH 登录时（如连接到容器），使用 OSC 52 把复制内容回传给本地终端；
;; 本地直接运行时仍使用 xclip（macOS 下走 pbcopy）。
(use-package xclip
  :unless (getenv "SSH_TTY")
  :defer 5
  :config
  (xclip-mode))

(use-package clipetty
  :if (getenv "SSH_TTY")
  :hook (after-init . global-clipetty-mode))

;; 终端下启用 Kitty Keyboard Protocol（Ghostty / iTerm2 均支持）：
;; Emacs 进入终端时开启、退出时恢复，所以只影响 Emacs，不影响 shell。
;; 修饰键映射与 GUI 下（early-init.el 中的 mac-*-modifier）保持一致：
;;   Command(super) → Meta，Option(alt) → Super
(use-package kkp
  :defer 3
  :hook (after-init . global-kkp-mode)
  :custom
  (kkp-super-modifier 'meta)
  (kkp-alt-modifier 'super))

;; 自定义一个找项目根目录的函数
(defun my/counsel-fzf-project-root (&optional arg)
  "优先寻找 package.json，找不到则使用 project.el 识别的项目根目录，作为 counsel-fzf 的搜索根目录。
使用 C-u 前缀时，手动选择搜索根目录。"
  (interactive "P")
  (let* ((project (project-current))
         (root (or (locate-dominating-file default-directory "package.json")
                   (and project (project-root project))
                   default-directory)) ; 如果都找不到，则使用当前目录
         (dir (if arg
                  (read-directory-name "fzf in directory: " root nil t)
                root)))
    (counsel-fzf nil dir)))

;; 全局基础配置
(use-package counsel
  :commands (swiper-isearch counsel-M-x counsel-ibuffer counsel-find-file counsel-rg counsel-fzf counsel-file-jump counsel-yank-pop)
  :config
  ;; 设置 counsel-fzf 命令使用 rg 作为核心输出端
  (setq counsel-fzf-cmd "rg -l -L --glob '!.git' --hidden . | fzf -f \"%s\"")
  ;; 设置 counsel-rg 命令同样搜索快捷方式内部的内容
  ;; (push "--follow" (cdr (nthcdr 0 counsel-rg-base-command)))
  (setq counsel-rg-base-command
        '("rg" "--max-columns" "500" "--with-filename" "--no-heading" "--line-number" "--color" "never" "--follow" "%s"))
  ;; 设置 swiper 默认按字面字符串搜索，搜索时可用 M-r 切换回正则
  ;; 注：M-r 交换 `ivy--regex-function' 与 `ivy--regexp-quote'，所以不能直接把 re-builder 设为 regexp-quote
  (defvar my/ivy-literal-by-default-callers '(swiper-isearch counsel-rg)
    "默认按字面搜索的 ivy 命令列表，搜索时可用 M-r 切换为正则。")
  (defun my/ivy-literal-by-default (state)
    "让 `my/ivy-literal-by-default-callers' 中的命令默认按字面搜索，STATE 为当前的 ivy 状态。"
    (when (memq (ivy-state-caller state) my/ivy-literal-by-default-callers)
      (setq ivy--regexp-quote ivy--regex-function)
      (setq ivy--regex-function #'regexp-quote)))
  (advice-add 'ivy--reset-state :after #'my/ivy-literal-by-default)

  ;; counsel-rg 原本固定使用 ivy-state 中的 re-builder，导致 M-r 无效，这里改为跟随 `ivy--regex-function'
  ;; 字面模式下直接把原始输入交给 rg 并附加 -F，避免 | { } \ 等字符在转换为 PCRE 时被当作正则
  (defun my/counsel-rg-literal-p ()
    "当前是否为字面模式的 `counsel-rg'。"
    (and (eq (ivy-state-caller ivy-last) 'counsel-rg)
         (eq ivy--regex-function #'regexp-quote)))
  (defun my/counsel-rg-grep-regex (orig str)
    "为 `counsel-rg' 生成搜索串，支持 M-r 切换；其他命令沿用 ORIG 处理 STR。"
    (cond ((not (eq (ivy-state-caller ivy-last) 'counsel-rg))
           (funcall orig str))
          ((my/counsel-rg-literal-p)
           (setq ivy--old-re (regexp-quote str))
           str)
          (t
           (counsel--elisp-to-pcre
            (setq ivy--old-re (funcall ivy--regex-function str))
            counsel--regex-look-around))))
  (advice-add 'counsel--grep-regex :around #'my/counsel-rg-grep-regex)
  (defun my/counsel-rg-fixed-strings-switch (switches)
    "字面模式的 `counsel-rg' 在 SWITCHES 中追加 rg 的 -F 参数。"
    (if (my/counsel-rg-literal-p)
        (concat switches " -F ")
      switches))
  (advice-add 'counsel--ag-extra-switches :filter-return #'my/counsel-rg-fixed-strings-switch)
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
  ;; 替换 kill-ring 粘贴选择
  ("M-y" . counsel-yank-pop)
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

;; 撤销：使用 Emacs 原生 undo / undo-redo，vundo 提供树状可视化
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-diff-quit 'kill)               ; 退出 vundo 时一并关闭 diff 窗口
  :config
  ;; 和 undo-tree-visualizer-diff 一样：打开撤销树、每次移动节点后自动显示 diff
  (defun my/vundo-auto-diff (&rest _)
    (when (derived-mode-p 'vundo-mode)
      (vundo-diff)))
  (dolist (cmd '(vundo vundo-forward vundo-next vundo-stem-root vundo-stem-end
                 vundo-next-root vundo-goto-last-saved vundo-goto-next-saved))
    (advice-add cmd :after #'my/vundo-auto-diff)))

;; 持久化撤销历史：只保存原生 undo-list，体积受 `undo-limit' 约束，保存很快
(use-package undo-fu-session
  :defer 3
  :custom
  (undo-fu-session-directory
   (expand-file-name "var/undo-fu-session/" user-emacs-directory))
  (undo-fu-session-file-limit 200)      ; 最多保留 200 个文件的历史，超出自动清理最旧的
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode))

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
;;
;; 每次保存都会记录各步耗时，写盘完成后（`after-save-hook'）判断是否汇报：
;; - 正常且不慢：什么都不输出；
;; - 超过 `my/save-formatters-slow-threshold' 或有步骤失败：在回显区显示
;;   （同时留在 *Messages* 里，只在内存中，不写磁盘）。
;; 某一步失败不会中断后续步骤，也不会阻止保存。错误要等写盘后再提示，
;; 否则会被写盘时的「Wrote ...」覆盖掉。

(defvar my/save-formatters
  '((my/whitespace-cleanup-on-save whitespace-cleanup   "空白")
    (eslintd-fix-on-save-mode      eslintd-fix-buffer   "ESLint")
    (prettier-js-mode              my/prettier-js-prettify "Prettier"))
  "保存前格式化列表，每一项是 (开关变量 格式化函数 显示名)。
按列表顺序执行：先清理行尾空白，再 ESLint 修复，最后 Prettier 排版。")

(defvar my/save-formatters-slow-threshold 0.5
  "整次保存超过这么多秒时，在回显区显示各步耗时。")

(defvar-local my/whitespace-cleanup-on-save nil
  "非 nil 时，保存前清理当前 buffer 的行尾空白。")

(defvar-local my/save-formatters--report nil
  "本次保存的记录：(开始时间 各步耗时列表 错误列表)，供 `after-save-hook' 汇报。")

(defun my/run-save-formatters ()
  "按 `my/save-formatters' 的顺序，执行当前 buffer 中已开启的格式化。
每一步都单独计时；失败只记录下来，不中断后续格式化，也不阻止保存。"
  (let ((start (float-time)) timings errors)
    (pcase-dolist (`(,switch ,formatter ,label) my/save-formatters)
      (when (and (boundp switch) (symbol-value switch))
        (let ((t0 (float-time)))
          (condition-case err
              (funcall formatter)
            (error (push (format "%s：%s" label (error-message-string err)) errors)))
          (push (cons label (- (float-time) t0)) timings))))
    (setq my/save-formatters--report
          (list start (nreverse timings) (nreverse errors)))))

(defun my/report-save-formatters ()
  "写盘完成后，保存太慢或有步骤失败时汇报：各步耗时、写盘及其它钩子的耗时、失败的步骤。
正常且不慢的保存什么都不输出。"
  (pcase my/save-formatters--report
    (`(,start ,timings ,errors)
     (setq my/save-formatters--report nil)
     (let ((total (- (float-time) start)))
       (when (or errors (> total my/save-formatters-slow-threshold))
         (let* ((rest (- total (apply #'+ (mapcar #'cdr timings))))
                (summary (format "保存 %s 用时 %.2fs：%s"
                                 (buffer-name) total
                                 (mapconcat (lambda (it) (format "%s %.2fs" (car it) (cdr it)))
                                            (append timings `(("写盘及其它" . ,rest)))
                                            " · "))))
           (if errors
               (message "%s\n%s"
                        summary
                        (propertize (format "已保存，但有 %d 步失败：%s"
                                            (length errors) (string-join errors "；"))
                                    'face 'error))
             (message "%s" summary))))))))

(defun my/enable-save-formatters ()
  "在当前 buffer 挂上统一的保存前格式化，以及写盘后的汇报。
只挂到当前 buffer：Makefile、Go 等其它文件不受影响。
汇报挂在 `after-save-hook' 的最后（深度 100），这样「写盘及其它」
也包含了其它同步执行的保存后钩子。"
  (add-hook 'before-save-hook #'my/run-save-formatters nil t)
  (add-hook 'after-save-hook #'my/report-save-formatters 100 t))

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
  :hook ((web-mode vue-ts-mode js-ts-mode tsx-ts-mode emacs-lisp-mode) . my/enable-whitespace-cleanup))

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

;; RSS 订阅
(use-package elfeed
  :ensure t
  :commands (elfeed)
  :custom
  ;; 默认只显示最近 7 天的未读文章
  (elfeed-search-filter "@7-days-ago +unread")

  ;; 示例订阅源：后续可以替换成自己的
  (elfeed-feeds
   '(("https://sspai.com/feed"
      :fetch-link t
      :readable t
      news)
     ("https://www.ifanr.com/feed"
      :fetch-link t
      :readable t
      news tech)
     ("http://feeds.feedburner.com/ruanyifeng"
      :fetch-link t
      :readable t
      blog tech china)
     ("https://emacs-china.org/latest.rss"
      :fetch-link t
      :readable t
      emacs china)))
  :config
  ;; 部分站点的图片 CDN 有防盗链：不带 Referer 的请求直接返回 403。
  ;; 而 shr 在 elfeed-show 中加载图片时没有 `url-current-lastloc'，不会发送 Referer。
  (defvar my/url-referer-alist
    '(("\\(?:^\\|\\.\\)sspai\\.com\\'" . "https://sspai.com/"))
    "图片主机名正则到所需 Referer 的映射。")

  (defun my/url-http-referer-fallback (referer url)
    "当 URL 本身没有 Referer 时，按 `my/url-referer-alist' 补一个。"
    (or referer
        (let ((host (url-host url)))
          (and host
               (cdr (seq-find (lambda (pair) (string-match-p (car pair) host))
                              my/url-referer-alist))))))

  (advice-add 'url-http--get-referer :around
              (lambda (orig url)
                (my/url-http-referer-fallback (funcall orig url) url))
              '((name . my/url-http-referer-fallback))))

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
