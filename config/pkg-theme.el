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

(provide 'pkg-theme)
