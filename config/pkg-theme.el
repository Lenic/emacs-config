(use-package spacemacs-theme
  :defer t
  :init
  ;; 自动切换编辑器主题
  ;; 控制台下只使用一种主题不设置定时器
  (if (equal nil (getenv "DISPLAY"))
      (load-theme 'spacemacs-dark t)
    (setq day-theme 'spacemacs-light)
    (setq dark-theme 'spacemacs-dark)
    (setq previous-theme-name "")
    (setq previous-theme nil)
    (defun synchronize-theme ()
      (setq hour (string-to-number (substring (current-time-string) 11 13)))
      (setq current-theme nil)
      (if (member hour (number-sequence 6 16))
          (setq current-theme day-theme)
        (setq current-theme dark-theme))
      (setq current-theme-name (symbol-name current-theme))
      (unless (string= previous-theme-name current-theme-name)
        (setq previous-theme-name current-theme-name)
        (unless (equal nil previous-theme)
          (disable-theme previous-theme)
          (setq previous-theme nil))
        (load-theme current-theme t)
        (setq previous-theme current-theme)))
    ;; 每 10 分钟运行一次检查
    (run-with-timer 0 600 'synchronize-theme)))

;; 输入法设置
(use-package pyim
  :demand t
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  ;; 设置使用拼音输入法
  (setq default-input-method "pyim")
  ;; 我使用全拼
  (setq pyim-default-scheme 'microsoft-shuangpin)
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)
  ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
  ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
  ;; 手动安装 posframe 包。
  (setq pyim-page-tooltip 'popup)
  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)
  :bind
  (("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

(provide 'pkg-theme)
