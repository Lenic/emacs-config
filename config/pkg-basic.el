;; 设置字体
;; (set-default-font "Ubuntu Mono 14")
(set-default-font "文泉驿等宽正黑 14")

;; 设置自动加载已修改文件
(global-auto-revert-mode t)

;; 设置 Mac 上的缺省按键映射
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; 设置缩进使用空格而非 Tab
(setq-default indent-tabs-mode nil)

;; 设置自动备份
(setq auto-save-default t)

;; 设置 yes 和 no 的输入使用简写
(defalias 'yes-or-no-p 'y-or-n-p)

;; 设置平滑滚动
(setq scroll-step            1
      scroll-conservatively  10000)

;; 备份设置
(setq backup-by-copying t ; 自动备份
      backup-directory-alist '(("." . "~/.em_backup")) ; 自动备份在目录"~/.em_backup"下
      delete-old-versions t ; 自动删除旧的备份文件
      kept-new-versions 3 ; 保留最近的3个备份文件
      kept-old-versions 1 ; 保留最早的1个备份文件
      version-control t) ; 多次备份

;; 显示光标所在列数
(column-number-mode 1)

;; 设置行号根据右侧对齐
(setq display-line-numbers-width-start t)

;; 设置窗口位置为屏库左上角(0,0)
(set-frame-position (selected-frame) 0 0)

;; 隐藏工具栏
(tool-bar-mode 0)

;; 隐藏菜单栏
(menu-bar-mode 0)

;;关闭启动画面
(setq inhibit-startup-message t)

;; 隐藏滚动条
(scroll-bar-mode 0)

;; 设置 Emacs 窗口的宽和高
(unless (equal nil (getenv "DISPLAY"))
  (set-frame-width (selected-frame) 135)
  (set-frame-height (selected-frame) 35))

;; 设置选中时编辑直接删除选中值
(delete-selection-mode t)

;; 高亮当前行
(global-hl-line-mode t)

;; 高亮匹配括号
(show-paren-mode t)

;; 设置光标样式
(setq-default cursor-type 'box)

;; 设置 Ace-Jump
(global-set-key (kbd "C-.") 'ace-jump-word-mode)

;; swiper 配置
;; (global-set-key (kbd "C-s") 'swiper-isearch)

;; 设置每次前进或者后退搜索后将目标位置放置在屏幕垂直居中
(defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate) (recenter))
(defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate) (recenter))

(ad-activate 'isearch-repeat-forward)
(ad-activate 'isearch-repeat-backward)

;; 替换命令执行
(global-set-key (kbd "M-x") 'counsel-M-x)

;; 替换 Buffer 界面
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)

;; 替换打开文件
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; 设置 AG 项目文件查找
(global-set-key (kbd "C-c k") 'counsel-ag)

;; 设置 Undo-Tree
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; 设置全局 Git 状态显示
(global-git-gutter+-mode)

;; 在项目中快速查找文件
(setq project-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-indexing-method 'alien)
(global-set-key (kbd "C-c p") 'projectile-find-file)

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
(setq day-theme 'solarized-light)
(setq dark-theme 'solarized-dark)
(setq previous-theme-name "")
(defun synchronize-theme ()
  (setq hour (string-to-number (substring (current-time-string) 11 13)))
  (setq current-theme nil)
  (if (member hour (number-sequence 6 16))
      (setq current-theme day-theme)
    (setq current-theme dark-theme))
  (setq current-theme-name (symbol-name current-theme))
  (unless (string= previous-theme-name current-theme-name)
    (setq previous-theme-name current-theme-name)
    (load-theme current-theme)))
;; 每 10 分钟运行一次检查
(run-with-timer 0 600 'synchronize-theme)

(provide 'pkg-basic)
