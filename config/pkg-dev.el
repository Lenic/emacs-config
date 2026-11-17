;; -*- lexical-binding: t -*-

;; 添加 treesit 语言配置
(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  ;; 添加 Vue 文件的 treesit 映射
  (setq treesit-language-source-alist
      '((vue "https://github.com/ikatyang/tree-sitter-vue")))
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
  ;; 开着 `global-subword-mode' 时，expand-region 一加载就给每个 buffer 拷一份局部的
  ;; `er/try-expand-list'，之后新开的 buffer 也在 subword-mode-hook 里从全局值拷一份。
  ;; 直接 `add-to-list' 只会改到第一次按 C-o 的那个 buffer 的局部值，其它 Vue 文件
  ;; 都缺下面的扩展函数，所以要同时改全局值和所有已有的局部值
  (defun my/er-add-expansion (fn)
    "把 FN 加到 `er/try-expand-list' 的全局值和所有 buffer 的局部值的最前面。"
    (unless (memq fn (default-value 'er/try-expand-list))
      (set-default 'er/try-expand-list (cons fn (default-value 'er/try-expand-list))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (local-variable-p 'er/try-expand-list)
          (add-to-list 'er/try-expand-list fn)))))

  (when (treesit-available-p)
    ;; 思路来自 https://emacs-china.org/t/treesit-expand-region-el/23406
    ;; vue-ts-mode 里 `:style="..."'、`{{ ... }}' 是独立的 local typescript parser，
    ;; 只用主 parser 的根节点会把整个属性值当成一个节点，一次跳过表达式内部的所有层级，
    ;; 所以这里从最内层的嵌入 parser 开始找，找不到更大的节点再交给外层 parser
    (defun my/treesit-mark-bigger-node ()
      "选中比当前选区大的最小 tree-sitter 节点，优先使用最内层的嵌入语法。"
      (let ((beg (region-beginning))
            (end (region-end)))
        (cl-loop
         for parser in (treesit-parsers-at beg)
         for node = (treesit-node-descendant-for-range
                     (treesit-parser-root-node parser) beg end)
         ;; 节点和选区一样大（或没包住选区）时继续往上找父节点
         do (while (and node
                        (or (> (treesit-node-start node) beg)
                            (< (treesit-node-end node) end)
                            (and (= (treesit-node-start node) beg)
                                 (= (treesit-node-end node) end))))
              (setq node (treesit-node-parent node)))
         ;; 嵌入 parser 的节点不能超出它自己的解析范围，否则交给外层 parser
         if (and node
                 (cl-some (lambda (range)
                            (and (<= (car range) (treesit-node-start node))
                                 (>= (cdr range) (treesit-node-end node))))
                          (or (treesit-parser-included-ranges parser)
                              (list (cons (point-min) (point-max))))))
         return (progn (set-mark (treesit-node-end node))
                       (goto-char (treesit-node-start node))))))
    (my/er-add-expansion 'my/treesit-mark-bigger-node)

    ;; `hover:bg-gray-100'、`w-[200px]' 里的 `:'、`[' 不是符号字符，mark-symbol 选不全，
    ;; 下一步就直接跳到整个属性值，这里补上「以空白为界的整个 class 名」这一步
    (defun my/treesit-mark-class-token ()
      "在属性值或字符串里选中当前选区所在、以空白为界的整段文本。"
      (let* ((beg (region-beginning))
             (end (region-end))
             (parser (car (treesit-parsers-at beg)))
             (node (and parser
                        (treesit-node-descendant-for-range
                         (treesit-parser-root-node parser) beg end))))
        (when (and node
                   (member (treesit-node-type node) '("attribute_value" "string_fragment"))
                   (<= (treesit-node-start node) beg)
                   (>= (treesit-node-end node) end))
          (let ((token-beg (save-excursion
                             (goto-char beg)
                             (skip-chars-backward "^ \t\n" (treesit-node-start node))
                             (point)))
                (token-end (save-excursion
                             (goto-char end)
                             (skip-chars-forward "^ \t\n" (treesit-node-end node))
                             (point))))
            (when (or (< token-beg beg) (> token-end end))
              (set-mark token-end)
              (goto-char token-beg))))))
    (my/er-add-expansion 'my/treesit-mark-class-token)))

;; C-l：智能居中，连按在下面几步之间循环，画面和前面某一步相同的步骤直接跳过：
;; 1. 代码块居中：编程模式下从光标所在位置逐层向外找代码块（最大到函数级别），
;;    选能放进窗口的最大那一层居中；一层都放不下时，把光标放在离窗口顶部 1/4 高度的位置
;; 2. 光标行居中  3. 光标行到顶部  4. 光标行到底部
;; 不是编程模式或者不在任何多行代码块里时，没有第 1 步。
;; 每一步之后高亮：第 1 步高亮整个代码块，其他步骤高亮光标行；1 秒后或执行下一个命令时清除。
;; 高度按屏幕上实际显示的行数算，折叠后显示几行就算几行
(defun my/smart-recenter--defun-node-p (node)
  "NODE 是否是函数级别的 tree-sitter 节点。"
  (or (ignore-errors (treesit-node-match-p node 'defun t))
      (let ((type (treesit-node-type node)))
        (pcase treesit-defun-type-regexp
          ((and (pred stringp) re) (string-match-p re type))
          (`(,re . ,pred) (and (string-match-p re type) (funcall pred node)))))))

(defun my/smart-recenter--treesit-ranges ()
  "从最内层的嵌入语法开始，返回包住光标的 tree-sitter 节点范围，由内到外，到函数级别为止。"
  (let ((pos (point))
        ranges done)
    (cl-loop
     for parser in (treesit-parsers-at pos)
     until done
     do (let ((node (treesit-node-descendant-for-range
                     (treesit-parser-root-node parser) pos pos))
              (limits (or (treesit-parser-included-ranges parser)
                          (list (cons (point-min) (point-max))))))
          ;; 根节点就是整个文件或整个嵌入区域，不算代码块
          (while (and node (treesit-node-parent node) (not done))
            ;; 嵌入 parser 的节点不能超出它自己的解析范围
            (when (cl-some (lambda (range)
                             (and (<= (car range) (treesit-node-start node))
                                  (>= (cdr range) (treesit-node-end node))))
                           limits)
              (push (cons (treesit-node-start node) (treesit-node-end node)) ranges)
              (when (my/smart-recenter--defun-node-p node)
                (setq done t)))
            (setq node (treesit-node-parent node)))))
    (nreverse ranges)))

(defun my/smart-recenter--syntax-ranges ()
  "没有 tree-sitter 时，用括号层级和 defun 返回包住光标的范围，由内到外。"
  (let* ((pos (point))
         (defun-range (ignore-errors
                        (save-excursion
                          (end-of-defun)
                          (let ((end (point)))
                            (beginning-of-defun)
                            (when (<= (point) pos end)
                              (cons (point) end))))))
         (ranges (delq nil
                       (mapcar (lambda (open)
                                 (let ((close (ignore-errors (scan-lists open 1 0))))
                                   (when (and close
                                              (or (null defun-range)
                                                  (>= open (car defun-range))))
                                     (cons open close))))
                               ;; 第 9 项是外层在前的左括号位置
                               (reverse (nth 9 (syntax-ppss pos)))))))
    (if defun-range
        (append ranges (list defun-range))
      ranges)))

(defun my/smart-recenter--ranges ()
  "返回按整行对齐、由内到外且逐层变大的多行代码块范围。"
  (let ((raw (if (and (treesit-available-p) (treesit-parser-list))
                 (my/smart-recenter--treesit-ranges)
               (my/smart-recenter--syntax-ranges)))
        result)
    (save-excursion
      (dolist (range raw)
        (let ((beg (progn (goto-char (car range)) (line-beginning-position)))
              (end (progn (goto-char (cdr range))
                          ;; 节点结束在下一行行首时，不把那一行算进来
                          (when (and (bolp) (> (point) (car range)))
                            (backward-char))
                          (line-end-position))))
          ;; 只保留多行的块，且每层都要比上一层大（去掉行范围相同的层）
          (when (and (> (count-lines beg end) 1)
                     (or (null result)
                         (< beg (caar result))
                         (> end (cdar result))))
            (push (cons beg end) result)))))
    (nreverse result)))

(defun my/smart-recenter--block ()
  "返回要居中的代码块范围：能放进窗口的最大一层，一层都放不下就用最内层。
不是编程模式或者没有多行代码块时返回 nil。"
  (when (derived-mode-p 'prog-mode)
    (let ((win-h (window-body-height))
          (ranges (my/smart-recenter--ranges)))
      (when ranges
        ;; 外层一定不比内层矮，所以取前面能放下的那几层里的最后一层
        (or (car (last (seq-take-while
                        (lambda (r) (<= (count-screen-lines (car r) (cdr r)) win-h))
                        ranges)))
            (car ranges))))))

(defun my/smart-recenter--line-up (n)
  "从光标所在屏幕行往上 N 行的位置，到 buffer 开头为止。"
  (save-excursion
    (vertical-motion 0)
    (vertical-motion (- n))
    (point)))

(defun my/smart-recenter--views (block)
  "按 代码块居中、光标行居中、到顶部、到底部 的顺序返回各步的 (步骤 . 窗口起点)。
BLOCK 为 nil 时没有代码块居中这一步。窗口起点相同的步骤只保留第一个，
这样光标靠近文件开头、几步画面一样时，连按不会出现看不出变化的步骤。"
  (let* ((win-h (window-body-height))
         ;; 和 `recenter-top-bottom' 一样，顶部和底部留出 `scroll-margin'
         (margin (min (max 0 scroll-margin) (truncate (/ win-h 4.0))))
         (views
          (append
           (when block
             (let ((height (count-screen-lines (car block) (cdr block))))
               (list (cons 'block
                           (if (> height win-h)
                               ;; 块比窗口高：光标行放在离顶部 1/4 高度的位置
                               (my/smart-recenter--line-up (/ win-h 4))
                             (save-excursion
                               (goto-char (car block))
                               (vertical-motion (- (/ (- win-h height) 2)))
                               (point)))))))
           (list (cons 'middle (my/smart-recenter--line-up (/ win-h 2)))
                 (cons 'top (my/smart-recenter--line-up margin))
                 (cons 'bottom (my/smart-recenter--line-up (- win-h 1 margin)))))))
    (cl-remove-duplicates views :key #'cdr :from-end t)))

;; 高亮用静态 overlay，不做渐隐：pulse 渐隐每一帧都改全局面孔，终端里会让整个 frame
;; 重画、整屏和光标狂闪。静态高亮只在加上和删除时各重画一次被高亮的那几行。
(defvar my/smart-recenter--flash-overlay nil
  "当前的高亮 overlay。")

(defvar my/smart-recenter--flash-timer nil
  "清除高亮的定时器。")

(defun my/smart-recenter--flash-stop ()
  "删除高亮。"
  (remove-hook 'pre-command-hook #'my/smart-recenter--flash-stop)
  (when (timerp my/smart-recenter--flash-timer)
    (cancel-timer my/smart-recenter--flash-timer))
  (when (overlayp my/smart-recenter--flash-overlay)
    (delete-overlay my/smart-recenter--flash-overlay))
  (setq my/smart-recenter--flash-timer nil
        my/smart-recenter--flash-overlay nil))

(defun my/smart-recenter--flash (beg end)
  "把 BEG 到 END 静态高亮，1 秒后或者执行下一个命令时清除。"
  ;; 颜色沿用 pulse 的起始色：zenburn 里 `highlight' 和 hl-line 同色、又接近背景，看不出来
  (require 'pulse)
  (my/smart-recenter--flash-stop)
  (let ((ov (make-overlay beg end)))
    ;; 盖过 hl-line（优先级 -50）
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face '((:extend t) pulse-highlight-start-face))
    (setq my/smart-recenter--flash-overlay ov
          my/smart-recenter--flash-timer
          (run-with-timer 1 nil #'my/smart-recenter--flash-stop))
    ;; 本次 C-l 的 pre-command-hook 已经跑过，这里挂上只会在下一个命令前触发
    (add-hook 'pre-command-hook #'my/smart-recenter--flash-stop)))

(defvar my/smart-recenter--state nil
  "上一次 `my/smart-recenter' 的状态：(光标位置 各步画面 当前序号 代码块范围)。")

(defun my/smart-recenter ()
  "智能居中：代码块居中 → 光标行居中 → 光标行到顶部 → 光标行到底部，连按循环。"
  (interactive)
  (let* ((state my/smart-recenter--state)
         (repeat (and (eq last-command this-command)
                      state
                      (eql (car state) (point))))
         (block (if repeat (nth 3 state) (my/smart-recenter--block)))
         (views (if repeat (nth 1 state) (my/smart-recenter--views block)))
         (index (cond (repeat (mod (1+ (nth 2 state)) (length views)))
                      ;; 第一次按：第一步的画面和现在一样，就直接用下一步
                      ((and (cdr views) (= (cdar views) (window-start))) 1)
                      (t 0)))
         (view (nth index views)))
    (set-window-start nil (cdr view))
    (setq my/smart-recenter--state (list (point) views index block))
    (if (eq (car view) 'block)
        (my/smart-recenter--flash (car block) (min (1+ (cdr block)) (point-max)))
      (my/smart-recenter--flash (line-beginning-position)
                                (min (1+ (line-end-position)) (point-max))))))

(global-set-key (kbd "C-l") #'my/smart-recenter)

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
