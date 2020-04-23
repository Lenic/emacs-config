(use-package counsel
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

(use-package amx)
(use-package wgrep)

(use-package multiple-cursors)

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  ;; 设置显示可视化撤销树形结构
  (global-undo-tree-mode))

(use-package git-gutter+
  :config
  ;; 设置全局 Git 状态显示
  (global-git-gutter+-mode t))

(use-package projectile
  :init
  (setq project-enable-caching t
        projectile-completion-system 'ivy
        projectile-indexing-method 'alien)
  :bind
  ;; 在项目中快速查找文件
  ("C-c p" . projectile-find-file))

;; 设置 Ace-Jump
(use-package ace-jump-mode
  :bind
  ("C-c j" . ace-jump-word-mode)
  ("C-c l" .  ace-jump-line-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 设置自动缩进的宽度
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)
              ;; 设置关闭自动换行
              (setq truncate-lines t)
              ;; 开启显示行号
              (display-line-numbers-mode +1)
              ;; 开启代码折叠子模式
              (hs-minor-mode +1)
              ;; 开启代码折叠快捷键
              (global-set-key (kbd "s-/") 'hs-toggle-hiding))))

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

(provide 'pkg-global)
