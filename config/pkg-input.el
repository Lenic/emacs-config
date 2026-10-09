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

;; 输入法设置
(use-package pyim
  :commands pyim-convert-string-at-point
  :custom
  ;; 个人词库
  (pyim-dicts `((:name "mine" :file ,(expand-file-name "pyim/mine.pyim" user-emacs-directory))))
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  ;; 设置使用拼音输入法
  (setq default-input-method "pyim")
  ;; 使用微软双拼
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
