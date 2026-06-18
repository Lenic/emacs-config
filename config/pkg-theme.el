;; -*- lexical-binding: t -*-

(defvar my/spaceline-loaded nil
  "Indicator whether spaceline has been loaded.")

;; 设置日间主题和夜间主题
(defvar my/day-theme nil
  "The light theme to use during the day.")

(defvar my/dark-theme nil
  "The dark theme to use at night.")

;; 指示当前主题的类别：t 表示日间主题，nil 表示夜间主题
(defvar my/is-day-theme nil
  "Non-nil if currently using the day theme.")

;; 当前主题检查循环是否正在运行
(defvar my/is-theme-running nil
  "Non-nil if the theme syncer is running.")

;; 当前正在执行的 timer 对象，默认值为 nil
(defvar my/theme-timer nil
  "The timer object for theme synchronization.")

;; 当前活跃 frame 的数量，默认数量为 0
(defvar my/current-frame-count 0
  "Count of currently active frames.")

;; 获取当前主题
(defun my/get-current-theme ()
  "Get the first custom enabled theme."
  (car custom-enabled-themes))

;; 切换编辑器主题
(defun my/switch-theme (&optional target-theme)
  "Switch the editor theme to TARGET-THEME."
  (let ((target (or target-theme (my/get-current-theme))))
    (when target
      (let ((current (my/get-current-theme)))
        (when current
          (disable-theme current))
        ;; 是明亮主题就切换为明亮主题；否则就反过来
        (if (eq target my/day-theme)
            (progn
              (load-theme my/day-theme t)
              (setq my/is-day-theme t))
          (load-theme my/dark-theme t)
          (setq my/is-day-theme nil))))))

;; 自动切换编辑器主题
(defun my/synchronize-theme ()
  "Synchronize the theme based on the current hour."
  (let* ((hour (nth 2 (decode-time)))
         ;; 判断当前是否处于白天：从早上 6 点开始到晚上 17 点
         (is-in-day (and (>= hour 6) (<= hour 16)))
         (current-theme (my/get-current-theme))
         (theme-changed nil))
    ;; 处于白天并且当前主题不是明亮主题时执行
    (if (and is-in-day (not (eq current-theme my/day-theme)))
        (progn
          (my/switch-theme my/day-theme)
          (setq theme-changed t))
      ;; 处于晚上并且当前主题不是暗黑主题时执行
      (if (and (not is-in-day) (not (eq current-theme my/dark-theme)))
          (progn
            (my/switch-theme my/dark-theme)
            (setq theme-changed t))))
    theme-changed))

;; 执行主题切换检查
(defun my/run-theme ()
  "Start the theme synchronization timer if not already running."
  (unless my/is-theme-running
    ;; 设置自动主题更换已经运行
    (setq my/is-theme-running t)
    ;; 设置启动后全屏
    (if (display-graphic-p)
        (run-with-timer 1 nil #'toggle-frame-fullscreen))
    ;; GUI 模式下才自动运行主题切换：每 10 分钟运行一次检查
    (if (display-graphic-p)
        (setq my/theme-timer (run-with-timer 0 600 #'my/synchronize-theme))
      ;; Terminal 下永久使用 dark-theme 主题
      (load-theme my/dark-theme t))))

;; 设置光标颜色：同步兼容 PYIM 的光标颜色设置
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

(defun my/theme-handle-new-frame (new-frame)
  "Handle theme configuration when a new frame NEW-FRAME is created."
  (select-frame new-frame)
  ;; 每次新建 frame 设置当前活跃的 frame 数量 +1
  (setq my/current-frame-count (1+ my/current-frame-count))
  ;; 执行设置主题功能
  (my/run-theme))

(defun my/theme-handle-deleted-frame (_deleted-frame)
  "Handle theme cleanup when a frame is deleted."
  ;; 每次销毁 frame 设置当前活跃的 frame 数量 -1
  (setq my/current-frame-count (1- my/current-frame-count))
  ;; 销毁了全部 of the 活跃 frame 后停止主题切换检查，同时复位 my/is-theme-running
  (when (<= my/current-frame-count 0)
    (setq my/is-theme-running nil)
    (when my/theme-timer
      (cancel-timer my/theme-timer)
      (setq my/theme-timer nil))))

(use-package spacemacs-theme
  :defer t
  :init
  ;; 设置明亮主题
  (setq my/day-theme 'spacemacs-light)
  ;; 设置暗黑主题
  (setq my/dark-theme 'spacemacs-dark)
  ;; 启动时不是 daemon 模式就执行主题设置
  (unless (daemonp)
    ;; 初始不是后台的方式启动，设置当前活跃的 frame 数量 +1
    (setq my/current-frame-count (1+ my/current-frame-count))
    ;; 执行设置主题功能
    (my/run-theme))
  (add-hook 'after-make-frame-functions #'my/theme-handle-new-frame)
  (add-hook 'after-delete-frame-functions #'my/theme-handle-deleted-frame))

;; 输入法设置
(use-package pyim
  :commands pyim-convert-string-at-point
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  ;; 设置使用拼音输入法
  (setq default-input-method "pyim")
  (custom-set-variables
   '(pyim-dicts '((:name "mine" :file "~/.emacs.d/pyim/mine.pyim"))))
  ;; 我使用全拼
  (setq pyim-default-scheme 'microsoft-shuangpin)
  ;; 设置不使用模糊拼音
  (setq pyim-pinyin-fuzzy-alist '())
  ;; 设置光标颜色
  (setq pyim-indicator-list (list #'my/pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))
  ;; 设置 pyim 探针设置
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)
  ;; 百度输入法的云输入配置
  (setq pyim-cloudim 'baidu)
  ;; 设置选词框的绘制方式
  ;; (setq pyim-page-tooltip 'popup)
  (setq pyim-page-tooltip nil)
  ;; 指示弹窗只显示一行
  (setq pyim-page-style 'one-line)
  :bind
  ("M-j" . pyim-convert-string-at-point))

(provide 'pkg-theme)
