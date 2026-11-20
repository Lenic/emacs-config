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

  ;; 缓存 `treesit-auto--ready-p' 的结果。
  ;; treesit-auto 在每次 `set-auto-mode-0' 前都会重建 `major-mode-remap-alist'，
  ;; 对 `treesit-auto-langs' 里的 60 多种语言逐个检查 grammar 是否可用；没装的 grammar
  ;; 每次都要到磁盘上找动态库，打开一个文件会重复 3 遍，约 2 秒（Claude Code 的 Ctrl+G
  ;; 打开 .md 就卡在这里）。结果只在安装新 grammar 后才会变化，所以安装后清空缓存。
  ;; 注意：`treesit-language-available-p' 是 primitive，native-comp 代码直接调用它，
  ;; 在它上面加 advice 不生效，只能缓存这一层。
  (defvar my/treesit-auto-ready-cache (make-hash-table :test #'eq)
    "缓存 `treesit-auto--ready-p' 的结果：major mode -> t 或 :no。")

  (defun my/treesit-auto-ready-cached (orig mode)
    "带缓存的 `treesit-auto--ready-p'。"
    (let ((v (gethash mode my/treesit-auto-ready-cache)))
      (if v
          (eq v t)
        (let ((r (funcall orig mode)))
          (puthash mode (if r t :no) my/treesit-auto-ready-cache)
          r))))

  (defun my/treesit-auto-ready-cache-clear (&rest _)
    "安装新的 grammar 后清空缓存。"
    (clrhash my/treesit-auto-ready-cache))

  (advice-add 'treesit-auto--ready-p :around #'my/treesit-auto-ready-cached)
  (advice-add 'treesit-install-language-grammar :after #'my/treesit-auto-ready-cache-clear)

  ;; daemon 启动时预热缓存，第一次打开文件就不用再等
  (when (daemonp)
    (add-hook 'after-init-hook #'treesit-auto--build-major-mode-remap-alist))
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

;; M-o：把光标所在的代码块在窗口里上下居中，连按效果不变。
;; 代码块是从光标往外找到的第一个跨多行的结构（对象、参数列表、语句块等），
;; 找到的是函数体时用整个函数，一直到函数级别都没有跨多行的结构时也用函数。
;; 不在任何代码块里时把光标行当作代码块。代码块比窗口高时，把开头放在离窗口顶部
;; 1/4 高度的位置。居中后高亮这段内容 300 毫秒。
;; 高度按屏幕上实际显示的行数算，折叠后显示几行就算几行。
(defun my/center-block--lines (beg end)
  "把 BEG 到 END 扩展成整行，返回 (开始 . 结束)。"
  (save-excursion
    (cons (progn (goto-char beg) (line-beginning-position))
          (progn (goto-char end)
                 ;; 结束在下一行行首时，不把那一行算进来
                 (when (and (bolp) (> (point) beg))
                   (backward-char))
                 (line-end-position)))))

(defun my/center-block--multiline-p (beg end)
  "BEG 到 END 扩展成整行后是否跨多行。"
  (let ((range (my/center-block--lines beg end)))
    (> (cdr range)
       (save-excursion (goto-char (car range)) (line-end-position)))))

(defun my/center-block--start-pos ()
  "查找的起点：光标在行首缩进里时，用这一行第一个字符的位置。"
  (if (<= (current-column) (current-indentation))
      (save-excursion (back-to-indentation) (point))
    (point)))

(defun my/center-block--defun-p (node)
  "NODE 是否是函数级别的 tree-sitter 节点。"
  (or (ignore-errors (treesit-node-match-p node 'defun t))
      (let ((type (treesit-node-type node)))
        (pcase treesit-defun-type-regexp
          ((and (pred stringp) re) (string-match-p re type))
          (`(,re . ,pred) (and (string-match-p re type) (funcall pred node)))))))

(defun my/center-block--treesit-node (node)
  "从 NODE 往外找第一个跨多行的节点，是函数体时返回函数，找不到返回 nil。
根节点是整个文件或整个嵌入区域，不算代码块。"
  (cl-loop
   for parent = (and node (treesit-node-parent node))
   while parent
   when (my/center-block--defun-p node)
   return node
   when (my/center-block--multiline-p (treesit-node-start node) (treesit-node-end node))
   return (if (my/center-block--defun-p parent) parent node)
   do (setq node parent)))

(defun my/center-block--treesit-range ()
  "用 tree-sitter 找光标所在的代码块，返回 (开始 . 结束)。
从最内层的嵌入语法开始找，像 Vue 文件里 script 中的代码也能找到。"
  (let ((pos (my/center-block--start-pos)))
    (cl-loop
     for parser in (treesit-parsers-at pos)
     for node = (my/center-block--treesit-node
                 (treesit-node-descendant-for-range
                  (treesit-parser-root-node parser) pos pos))
     when node
     return (cons (treesit-node-start node) (treesit-node-end node)))))

(defun my/center-block--syntax-range ()
  "没有 tree-sitter 时，用括号层级找第一个跨多行的括号，找不到就用所在的函数。"
  (let ((pos (my/center-block--start-pos)))
    (or (cl-loop
         ;; 第 9 项是外层在前的左括号位置，反过来从最内层开始找；
         ;; 起点正好是左括号时，从这个括号开始
         for open in (append (when (eq (syntax-class (syntax-after pos)) 4)
                               (list pos))
                             (reverse (nth 9 (syntax-ppss pos))))
         for close = (ignore-errors (scan-lists open 1 0))
         when (and close (my/center-block--multiline-p open close))
         return (cons open close))
        (ignore-errors
          (save-excursion
            (goto-char pos)
            (end-of-defun)
            (let ((end (point)))
              (beginning-of-defun)
              (when (<= (point) pos end)
                (cons (point) end))))))))

(defun my/center-block--range ()
  "返回光标所在代码块按整行对齐的范围 (开始 . 结束)，找不到时返回 nil。"
  (when-let* ((range (and (derived-mode-p 'prog-mode)
                          (if (and (treesit-available-p) (treesit-parser-list))
                              (my/center-block--treesit-range)
                            (my/center-block--syntax-range)))))
    (my/center-block--lines (car range) (cdr range))))

(defvar my/center-block--overlay nil
  "居中后的高亮 overlay。")

(defun my/center-block--unflash ()
  "删除高亮。"
  (when (overlayp my/center-block--overlay)
    (delete-overlay my/center-block--overlay)
    (setq my/center-block--overlay nil)))

(defun my/center-block--flash (beg end)
  "高亮 BEG 到 END 所在的整行，300 毫秒后清除。"
  ;; zenburn 里 `highlight' 和 hl-line 同色，看不出来，所以借用 pulse 的颜色
  (require 'pulse)
  (my/center-block--unflash)
  (let ((ov (make-overlay beg (min (1+ end) (point-max)))))
    ;; 盖过 hl-line（优先级 -50）
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face '((:extend t) pulse-highlight-start-face))
    (setq my/center-block--overlay ov)
    (run-with-timer 0.3 nil #'my/center-block--unflash)))

(defun my/center-block ()
  "把光标所在的代码块在窗口里上下居中，代码块太长时把开头放在离顶部 1/4 的位置。"
  (interactive)
  (let* ((range (or (my/center-block--range)
                    (cons (line-beginning-position) (line-end-position))))
         (height (count-screen-lines (car range) (cdr range)))
         (win-h (window-body-height))
         (above (if (<= height win-h)
                    (/ (- win-h height) 2)
                  (/ win-h 4))))
    (set-window-start nil (save-excursion
                            (goto-char (car range))
                            (vertical-motion (- above))
                            (point)))
    (my/center-block--flash (car range) (cdr range))))

(global-set-key (kbd "M-o") #'my/center-block)

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
