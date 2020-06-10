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
  ;; 设置 AG 全文搜索
  ("C-c k" . counsel-ag)
  ;; 设置 Git 控制下的文件名查找
  ("C-c p" . counsel-git)
  ;; 设置查找特定目录下的文件名查找
  ("C-c f" . counsel-file-jump))

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

;; 设置 Ace-Jump
(use-package ace-jump-mode
  :bind
  ("C-c j" . ace-jump-word-mode)
  ("C-c l" .  ace-jump-line-mode))

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
