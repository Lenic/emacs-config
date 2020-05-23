;; 设置字体
;; (set-default-font "Ubuntu Mono 14")
;; (set-default-font "文泉驿等宽正黑 12")
;; (set-default-font "Noto Sans Mono CJK SC 14") ;; 思源黑体的等宽字体
(setq default-frame-alist '((font . "Noto Sans Mono CJK SC 14")))

;; 设置自动加载已修改文件
(global-auto-revert-mode t)

;; 高亮当前行
(global-hl-line-mode t)

;; 编码设置
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Always load newest byte code
(setq load-prefer-newer t)

;; 设置 Mac 上的缺省按键映射
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; 设置缩进使用空格而非 Tab，同时设置 Tab 宽度是 4 个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; 设置自动备份
(setq auto-save-default t)

;; 设置 yes 和 no 的输入使用简写
(defalias 'yes-or-no-p 'y-or-n-p)

;; 设置平滑滚动
(setq scroll-step            1
      scroll-conservatively  10000)

;; 保持鼠标所在行数不变屏幕向下滚动一行
(global-set-key (kbd "M-n") 'scroll-up-line)

;; 保持鼠标所在行数不变屏幕向上滚动一行
(global-set-key (kbd "M-p") 'scroll-down-line)

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
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; 隐藏菜单栏
(menu-bar-mode 0)

;;关闭启动画面
(setq inhibit-startup-message t)

;; 隐藏滚动条
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; 设置 Emacs 窗口的宽和高
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 140))

;; 设置选中时编辑直接删除选中值
(delete-selection-mode t)

;; 高亮匹配括号
(show-paren-mode t)

;; 设置光标样式
(setq-default cursor-type 'box)

;; 设置每次前进或者后退搜索后将目标位置放置在屏幕垂直居中
(defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate) (recenter))
(defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate) (recenter))
(ad-activate 'isearch-repeat-forward)
(ad-activate 'isearch-repeat-backward)

(provide 'pkg-basic)
