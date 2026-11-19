;; -*- lexical-binding: t -*-

;; 设置光标颜色：同步兼容 PYIM 的光标颜色设置，颜色随 pkg-theme 的日夜主题变化
(defun my/pyim-indicator-with-cursor-color (input-method chinese-input-p)
  "Set cursor color according to current input method and theme."
  (if (not (equal input-method "pyim"))
      ;; pyim 关闭时的颜色
      (if my/is-day-theme
          (set-cursor-color "#100a14")
        (set-cursor-color "#e3dedd"))
    (if chinese-input-p
        ;; pyim 输入中文时的颜色
        (if my/is-day-theme
            (set-cursor-color "purple")
          (set-cursor-color "#ff72ff"))
      ;; pyim 输入英文时的颜色
      (if my/is-day-theme
          (set-cursor-color "#100a14")
        (set-cursor-color "#e3dedd")))))

;; 同 `pyim-probe-dynamic-english'，但在行首（缩进之前）不再向上一行查找字符，
;; 即换行后不继承上一行末尾的中英文状态，强制使用英文
(defun my/pyim-probe-dynamic-english ()
  "Like `pyim-probe-dynamic-english', but force English at line beginning."
  (if (<= (point) (save-excursion (back-to-indentation) (point)))
      (not (> (length (pyim-process-get-entered 'point-before)) 0))
    (pyim-probe-dynamic-english)))

;; 修复 pyim 连续选词时与 M-b（光标后退一个音节）配合的问题：
;; 1. 选中的词正好覆盖光标前全部拼音时，原实现 (insert nil) 报错被吞掉，
;;    光标停在开头，而 `pyim-entered-get' 在开头时前后都返回整串，导致重复上屏；
;; 2. 选了较短的词后，原实现把光标推到末尾，把 M-b 留在光标后的拼音又并回来转换。
;; 修复后：光标前还有未转换的拼音时，光标停在它们之后，光标后的拼音继续保留；
;; 光标前都转换完了，光标只向后移动一个词（逐词确认），而不是直接移到末尾，
;; 避免把后面整段拼音拼成的整句首选一次性上屏。
(defun my/pyim-next-word-imelem-count (string)
  "返回 STRING 中第一个词占的 imelem 数，与 pyim 拼整句首选时的分词一致。
非全拼/双拼方案或无法分词时返回 nil。"
  (let ((scheme (pyim-scheme-current)))
    (when (pyim-scheme-quanpin-p scheme)
      (let* ((imobjs (pyim-imobjs-create string scheme))
             (word (car (pyim-candidates--quanpin imobjs scheme))))
        (when (and word (> (length word) 0))
          (length word))))))

(defun my/pyim-process--select-word-in-next-step ()
  "Like `pyim-process--select-word-in-next-step', but respect text after point."
  (let ((rest (or (pyim-process--entered-to-be-translated) "")))
    (pyim-process-with-entered-buffer
      (delete-region (point-min) (point))
      (insert rest)
      (goto-char
       (if (not (string-empty-p rest))
           (1+ (length rest))
         (let ((n (ignore-errors (my/pyim-next-word-imelem-count (buffer-string)))))
           (if n
               (pyim-process-next-imelem-position n t (point-min))
             (point-max))))))
    (pyim-process-run)))

;; pyim 会在当前 buffer 里按拼音前缀搜索词条（候选后标 (buf)），
;; 比如 bmli 会把 buffer 中的「变量」(bian-liang) 也找出来。
;; 这里只保留拼音与输入完全一致的词条（多音字任一读音匹配即可）。
(defun my/pyim-candidates-exact-buffer-words (orig imobjs scheme)
  "Filter buffer-searched words from ORIG to those whose pinyin match IMOBJS exactly."
  (let ((words (funcall orig imobjs scheme))
        (code (string-join (pyim-codes-create (car imobjs) (pyim-scheme-get 'quanpin)) "-")))
    (cl-remove-if-not
     (lambda (word) (member code (pyim-cstring-to-pinyin word nil "-" t)))
     words)))

;; pyim 的删词只清理个人词库缓存（icode2word 等），基础词库 basedict、
;; mine.pyim 以及 buffer 搜索出来的词条仍会出现在候选中。
;; 这里维护一份删词黑名单：删过的词从所有候选中过滤掉，并立即落盘。
(defvar my/pyim-blocked-words-file
  (expand-file-name "pyim/blocked-words.el" user-emacs-directory)
  "保存 pyim 删词黑名单的文件。")

(defvar my/pyim-blocked-words nil
  "pyim 删词黑名单（哈希表），首次使用时从文件加载。")

(defun my/pyim-blocked-words ()
  "返回黑名单哈希表，必要时从 `my/pyim-blocked-words-file' 加载。"
  (or my/pyim-blocked-words
      (let ((table (make-hash-table :test #'equal)))
        (when (file-exists-p my/pyim-blocked-words-file)
          (with-temp-buffer
            (insert-file-contents my/pyim-blocked-words-file)
            (dolist (word (ignore-errors (read (current-buffer))))
              (puthash word t table))))
        (setq my/pyim-blocked-words table))))

(defun my/pyim-save-blocked-words ()
  "将黑名单写入 `my/pyim-blocked-words-file'。"
  (let ((words (sort (hash-table-keys (my/pyim-blocked-words)) #'string<))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file my/pyim-blocked-words-file
      (insert ";; -*- coding: utf-8-unix; -*-\n;; pyim 删词黑名单，由 pkg-input.el 维护\n")
      (prin1 words (current-buffer))
      (insert "\n"))))

(defun my/pyim-block-deleted-word (word)
  "`pyim-process-delete-word' 之后：把 WORD 加入黑名单，并将个人词库缓存落盘。"
  (puthash word t (my/pyim-blocked-words))
  (my/pyim-save-blocked-words)
  (pyim-process-save-dcaches t))

(defun my/pyim-filter-blocked-words (orig &rest args)
  "从 ORIG 返回的候选中去掉黑名单里的词条。"
  (let ((words (apply orig args))
        (blocked (my/pyim-blocked-words)))
    (if (zerop (hash-table-count blocked))
        words
      (cl-remove-if (lambda (word) (gethash word blocked)) words))))

(defun my/pyim-unblock-word (word)
  "把 WORD 从 pyim 删词黑名单中移除，让它重新出现在候选中。"
  (interactive
   (list (completing-read "恢复词条: " (hash-table-keys (my/pyim-blocked-words)) nil t)))
  (remhash word (my/pyim-blocked-words))
  (my/pyim-save-blocked-words)
  (message "已将词条 %S 移出黑名单。" word))

;; pyim 选词上屏时会立刻把整段结果当成新词记入个人词库，误选的组合（比如「我县在」）
;; 也会被学进去。这里改成延迟记录：选词时先不记，`my/pyim-create-word-delay' 秒后
;; 检查这个词是否还在原来插入的位置，还在才记入词库；期间被删掉或改掉就不记录。
;; 只影响选词上屏；手动造词（pyim-create-word-at-point 等）仍然立即生效。
(defvar my/pyim-create-word-delay 2
  "选词后延迟多少秒再决定是否把词条记入个人词库。")

(defvar my/pyim--deferring-create-word nil
  "非 nil 时，`pyim-process-create-word' 改为延迟记录。")

(defun my/pyim--defer-create-word-in-select (orig &rest args)
  "在 `pyim-process-select-word' 期间，让新词延迟记录。"
  (let ((my/pyim--deferring-create-word t))
    (apply orig args)))

(defun my/pyim--commit-deferred-word (marker word args)
  "若 MARKER 处仍是 WORD，则用 ARGS 调用 `pyim-process-create-word' 记录它。"
  (let ((buf (marker-buffer marker)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((end (+ marker (length word))))
          (when (and (<= end (point-max))
                     (equal (buffer-substring-no-properties marker end) word))
            (apply #'pyim-process-create-word args))))
      (set-marker marker nil))))

(defun my/pyim--deferred-create-word (orig word &optional prepend wordcount-handler criteria)
  "选词期间把 `pyim-process-create-word' 推迟 `my/pyim-create-word-delay' 秒执行。"
  (if (not my/pyim--deferring-create-word)
      (funcall orig word prepend wordcount-handler criteria)
    (let ((word (substring-no-properties (or word "")))
          ;; 选词时的编码在之后会被清掉，先保存下来用于多音字矫正。
          (criteria (or criteria pyim-process--code-criteria)))
      (when (> (length word) 0)
        ;; 预览文字（overlay）稍后会被删除，词条插入到预览的起点；没有预览时就是 point。
        (run-with-timer my/pyim-create-word-delay nil
                        #'my/pyim--commit-deferred-word
                        (copy-marker (if (and (overlayp pyim-preview--overlay)
                                              (overlay-start pyim-preview--overlay))
                                         (overlay-start pyim-preview--overlay)
                                       (point)))
                        word
                        (list word prepend wordcount-handler criteria))))))

;; 输入法设置
(use-package pyim
  :commands pyim-convert-string-at-point
  :custom
  ;; 个人词库
  (pyim-dicts `((:name "mine" :file ,(expand-file-name "pyim/mine.pyim" user-emacs-directory))))
  :config
  (advice-add 'pyim-process--select-word-in-next-step
              :override #'my/pyim-process--select-word-in-next-step)
  (advice-add 'pyim-candidates-create-limit-time
              :around #'my/pyim-candidates-exact-buffer-words)
  ;; 删词黑名单：删过的词不再出现在候选中，删词后立即保存词库缓存
  (advice-add 'pyim-process-delete-word :after #'my/pyim-block-deleted-word)
  (advice-add 'pyim-process-select-word :around #'my/pyim--defer-create-word-in-select)
  (advice-add 'pyim-process-create-word :around #'my/pyim--deferred-create-word)
  (advice-add 'pyim-candidates-create :around #'my/pyim-filter-blocked-words)
  (advice-add 'pyim-candidates-create-limit-time :around #'my/pyim-filter-blocked-words)
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  ;; 设置使用拼音输入法
  (setq default-input-method "pyim")
  ;; 使用微软双拼
  (setq pyim-default-scheme 'microsoft-shuangpin)
  ;; 设置不使用模糊拼音
  (setq pyim-pinyin-fuzzy-alist '())
  ;; 关闭联想词（前缀补全），避免 bmli 出现「变量」这类候选
  (setq pyim-enable-shortcode nil)
  ;; 设置光标颜色
  (setq pyim-indicator-list (list #'my/pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))
  ;; 设置 pyim 探针：只按光标前一个字符决定中英文，前面是中文就继续输入中文。
  ;; 不加 pyim-probe-program-mode：它会在代码区域强制英文，即使前面是中文
  (setq-default pyim-english-input-switch-functions
                '(my/pyim-probe-dynamic-english
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)
  ;; 百度输入法的云输入配置：会把输入的拼音发送到百度服务器，目前用不上，先注释掉
  ;; (setq pyim-cloudim 'baidu)
  ;; 设置选词框的绘制方式
  ;; (setq pyim-page-tooltip 'popup)
  (setq pyim-page-tooltip nil)
  ;; 指示弹窗只显示一行
  (setq pyim-page-style 'one-line)
  :bind
  ("M-j" . pyim-convert-string-at-point))

;; 在 swiper 中仍然可以输入中文，只不过换成了 M-i 这个快捷键
(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "M-i") 'pyim-convert-string-at-point))

(provide 'pkg-input)
