;; 设置字体
(set-default-font "Ubuntu Mono 14")

;; 设置缺省主题
(load-theme 'spacemacs-dark t)

;; 设置 Mac 上的缺省按键映射
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; 设置自动备份
(setq auto-save-default t)

;; 备份设置
(setq
     backup-by-copying t ; 自动备份
     backup-directory-alist
     '(("." . "~/.em_backup")) ; 自动备份在目录"~/.em_backup"下
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

;;关闭启动画面
(setq inhibit-startup-message t)

;; 隐藏滚动条
(scroll-bar-mode 0)

;; 设置 Emacs 窗口的宽和高
(set-frame-width (selected-frame) 135)
(set-frame-height (selected-frame) 40)

;; 设置选中时编辑直接删除选中值
(delete-selection-mode t)

;; 高亮当前行
(global-hl-line-mode t)

;; 高亮匹配括号
(show-paren-mode t)

;; 设置光标样式
(setq-default cursor-type 'bar)

;; swiper 配置
(global-set-key (kbd "C-s") 'swiper-isearch)

;; 替换命令执行
(global-set-key (kbd "M-x") 'counsel-M-x)

;; 替换 Buffer 界面
(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)

;; 替换打开文件
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; 设置 AG 项目文件查找
(global-set-key (kbd "C-c k") 'counsel-ag)

(provide 'pkg-basic)
