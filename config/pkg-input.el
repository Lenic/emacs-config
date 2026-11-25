;; -*- lexical-binding: t -*-

;; 设置光标颜色：同步兼容 PYIM 的光标颜色设置
(defun my/pyim-indicator-with-cursor-color (input-method chinese-input-p)
  "Set cursor color according to current input method."
  (set-cursor-color
   (if (and (equal input-method "pyim") chinese-input-p)
       ;; pyim 输入中文时的颜色
       "#88E098"
     ;; pyim 关闭或输入英文时的颜色
     "#6CA8F0")))

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

;; 基础词库：优先用雾凇拼音词库（8105 单字 + base + ext），没有时退回 pyim-basedict。
;;
;; pyim/rime-ice.pyim 不在仓库里（已加入 .gitignore），所以新机器 clone 下来后先用
;; basedict，开箱即可输入。执行 M-x my/pyim-use-rime-ice 会在后台运行
;; bin/rime-ice-to-pyim.py（只依赖 python3 标准库，从 GitHub 下载约 28 MB 词库并转换），
;; 成功后当前 Emacs 立即切到雾凇词库，之后启动时检测到文件存在也会自动使用它。
;; 想更新雾凇词库时，再执行一次这个命令即可。
;; 每个 daemon 各自切换：在一个 daemon 里执行命令，其他 daemon 要重启或各自执行一次。
(defvar my/pyim-rime-ice-file
  (expand-file-name "pyim/rime-ice.pyim" user-emacs-directory)
  "雾凇拼音转换出的 pyim 词库文件。")

(defvar my/pyim-rime-ice-script
  (expand-file-name "bin/rime-ice-to-pyim.py" user-emacs-directory)
  "把雾凇拼音词库转换成 pyim 词库的脚本。")

(defvar my/pyim--rime-ice-process nil
  "正在生成雾凇词库的进程。")

(defun my/pyim-enable-rime-ice ()
  "让 pyim 使用雾凇词库并停用 basedict（只改词库列表，不刷新缓存）。"
  (setq pyim-extra-dicts
        (seq-remove (lambda (dict) (equal (plist-get dict :name) "Basedict-elpa"))
                    pyim-extra-dicts))
  (add-to-list 'pyim-dicts `(:name "rime-ice" :file ,my/pyim-rime-ice-file) t))

(defun my/pyim-setup-dicts ()
  "雾凇词库存在就用它，否则退回 pyim-basedict。"
  (if (file-exists-p my/pyim-rime-ice-file)
      (my/pyim-enable-rime-ice)
    (require 'pyim-basedict)
    (pyim-basedict-enable)
    (message "pyim: 还没有雾凇词库，暂用 basedict；M-x my/pyim-use-rime-ice 生成并切换")))

(defun my/pyim--rime-ice-sentinel (proc _event)
  "生成雾凇词库的进程 PROC 结束后，成功就切换词库，失败就提示。"
  (when (memq (process-status proc) '(exit signal))
    (if (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc)))
        (progn
          ;; pyim 还没加载时什么都不用做：加载时 `my/pyim-setup-dicts' 会选上雾凇词库
          (when (featurep 'pyim)
            (my/pyim-enable-rime-ice)
            (pyim-process-init-dcaches :force))
          (message "pyim: 雾凇词库已生成，词库缓存在后台重建，几秒后生效"))
      (message "pyim: 生成雾凇词库失败，仍使用原来的词库，详见 %s"
               (buffer-name (process-buffer proc))))))

(defun my/pyim-use-rime-ice ()
  "在后台生成（或更新）雾凇词库，成功后把 pyim 切换到雾凇词库。"
  (interactive)
  (when (process-live-p my/pyim--rime-ice-process)
    (user-error "雾凇词库正在生成中"))
  (let ((python (or (executable-find "python3") (user-error "找不到 python3")))
        (buffer (get-buffer-create "*rime-ice-to-pyim*")))
    (with-current-buffer buffer
      (erase-buffer))
    (setq my/pyim--rime-ice-process
          (make-process
           :name "rime-ice-to-pyim"
           :buffer buffer
           :command (list python "-I" my/pyim-rime-ice-script "-o" my/pyim-rime-ice-file)
           :sentinel #'my/pyim--rime-ice-sentinel))
    (message "pyim: 正在下载并生成雾凇词库，完成后自动切换……")))

;; 只负责安装，需要时由 `my/pyim-setup-dicts' 加载
(use-package pyim-basedict
  :defer t)

;; 输入法设置
(use-package pyim
  :commands pyim-convert-string-at-point
  :custom
  ;; 个人词库；基础词库由 `my/pyim-setup-dicts' 添加
  (pyim-dicts `((:name "mine" :file ,(expand-file-name "pyim/mine.pyim" user-emacs-directory))))
  :config
  (my/pyim-setup-dicts)
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
