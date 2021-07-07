;; 设置 mode-line
(defun load-spaceline ()
  (use-package spaceline
    :config
    (spaceline-emacs-theme)
    (spaceline-toggle-buffer-size-off)
    ;; 延迟一秒后编译 spaceline
    (run-with-timer 1 nil 'spaceline-compile)
    :init
    (setq powerline-default-separator 'slant)))

;; 自动切换编辑器主题
(setq day-theme nil)
(setq dark-theme nil)
(setq previous-theme-name "")
(setq previous-theme nil)
(defun synchronize-theme ()
  (setq hour (string-to-number (substring (current-time-string) 11 13)))
  (setq current-theme nil)
  (if (member hour (number-sequence 6 17))
      (setq current-theme day-theme)
    (setq current-theme dark-theme))
  (setq current-theme-name (symbol-name current-theme))
  (unless (string= previous-theme-name current-theme-name)
    (setq previous-theme-name current-theme-name)
    (unless (equal nil previous-theme)
      (disable-theme previous-theme)
      (setq previous-theme nil))
    ;; 更新主题
    (load-theme current-theme t)
    ;; 更改中文时光标的色值
    (if (string= current-theme-name (symbol-name day-theme))
        (setq pyim-indicator-cursor-color (list "purple"))
      (setq pyim-indicator-cursor-color (list "#ff72ff")))
    ;; 重新编译 Spaceline
    (if (spaceline-compile)
        (spaceline-compile))
    ;; 设置高亮列的背景色
    ;; (set-face-attribute 'col-highlight nil :inherit 'hl-line :background)
    (setq previous-theme current-theme)))

(use-package spacemacs-theme
  :defer t
  :init
  (setq is-theme-running nil)
  (setq day-theme 'spacemacs-light)
  (setq dark-theme 'spacemacs-dark)
  ;; 当前活跃 frame 的数量，默认的数量是 0
  (setq current-frame-count 0)
  ;; 当前正在执行的 interval token，默认值为 nil，开始运行主题切换检查后置为 t
  (setq token-of-interval nil)
  ;; 执行主题切换检查
  (defun run-theme()
    ;; 加载 spaceline
    (load-spaceline)
    ;; 设置启动后全屏
    (if (display-graphic-p)
        (run-with-timer 1 nil 'toggle-frame-fullscreen))
    ;; 设置自动主题更换已经运行
    (setq is-theme-running t)
    ;; GUI 模式下才自动运行主题切换：每 10 分钟运行一次检查
    (if (display-graphic-p)
        (setq token-of-interval (run-with-timer 0 600 'synchronize-theme))
      ;; Terminal 下永久使用 dark-theme 主题
      (load-theme dark-theme t)))
  ;; 启动时不是 daemon 模式就执行主题设置
  (if (eq (daemonp) nil)
      (progn
        ;; 初始不是后台的方式启动，设置当前活跃的 frame 数量 +1
        (setq current-frame-count (+ current-frame-count 1))
        (run-theme)))
  (add-hook 'after-make-frame-functions
            (lambda (new-frame)
              (select-frame new-frame)
              ;; 每次新建 frame 设置当前活跃的 frame 数量 +1
              (setq current-frame-count (+ current-frame-count 1))
              (if (eq is-theme-running nil)
                  (run-theme))))
  (add-hook 'after-delete-frame-functions
            (lambda (deleted-frame)
              ;; 每次销毁 frame 设置当前活跃的 frame 数量 -1
              (setq current-frame-count (- current-frame-count 1))
              ;; 销毁了全部的活跃 frame 后停止主题切换检查，同时复位 is-theme-running
              (if (eq current-frame-count 0)
                  (progn
                    (setq is-theme-running nil)
                    (cancel-timer token-of-interval))))))

;; 输入法设置
(use-package popup)
(use-package pyim
  :after popup
  :demand t
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  ;; 设置使用拼音输入法
  (setq default-input-method "pyim")
  ;; 我使用全拼
  (setq pyim-default-scheme 'microsoft-shuangpin)
  ;; 设置不使用模糊拼音
  (setq pyim-pinyin-fuzzy-alist '())
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 开启拼音搜索功能
  ;; (pyim-isearch-mode 1)
  ;; 设置选词框的绘制方式
  (setq pyim-page-tooltip nil)
  ;; (if (posframe-workable-p)
  ;;     (setq pyim-page-tooltip 'posframe)
  ;;   (setq pyim-page-tooltip 'popup))
  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)
  ;; 指示弹窗只显示一行，非两行的拼音和候选词显示
  (setq pyim-page-style 'one-line)
  :bind
  ("M-j" . pyim-convert-string-at-point)) ;与 pyim-probe-dynamic-english 配合

(provide 'pkg-theme)
