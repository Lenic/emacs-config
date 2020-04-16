(use-package counsel
  :ensure t
  :bind
  ;; swiper 配置
  ("C-s" . swiper-isearch)
  ;; 替换命令执行
  ("M-x" . counsel-M-x)
  ;; 替换 Buffer 界面
  ("C-x C-b" . counsel-ibuffer)
  ;; 替换打开文件
  ("C-x C-f" . counsel-find-file)
  ;; 设置 AG 项目文件查找
  ("C-c k" . counsel-ag))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :config
  ;; 设置显示可视化撤销树形结构
  (global-undo-tree-mode))

(use-package git-gutter+
  :ensure t
  :config
  ;; 设置全局 Git 状态显示
  (global-git-gutter+-mode))

(use-package projectile
  :ensure t
  :custom
  (project-enable-caching t)
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'alien)
  :bind
  ;; 在项目中快速查找文件
  ("C-c p" . projectile-find-file))

;; 设置 Ace-Jump
(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-c j" . ace-jump-char-mode))

;; 拷贝当前 Buffer 到剪切板
(defun copy-buffer-path ()
  (interactive)
  (if (equal buffer-file-name nil)
      (message "没有文件名")
    (setq path (vc-find-root buffer-file-name ".git"))
    (if (equal path nil)
	(kill-new (message buffer-file-name))
      (setq absolutePath (expand-file-name path))
      (setq targetPath (substring buffer-file-name (length absolutePath)))
      (kill-new (message targetPath)))))
(global-set-key (kbd "C-c C-p") 'copy-buffer-path)

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
  (run-with-timer 0 600 'synchronize-theme))

(provide 'pkg-global)
