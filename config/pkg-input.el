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
;; 光标前都转换完了，才把光标移到末尾，开始转换之前留在后面的拼音。
(defun my/pyim-process--select-word-in-next-step ()
  "Like `pyim-process--select-word-in-next-step', but respect text after point."
  (let ((rest (or (pyim-process--entered-to-be-translated) "")))
    (pyim-process-with-entered-buffer
      (delete-region (point-min) (point))
      (insert rest)
      (goto-char (if (string-empty-p rest) (point-max) (1+ (length rest)))))
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
