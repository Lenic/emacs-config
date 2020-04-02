;; 设置特定文件直接进入该模式
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; JSON 设置
(add-hook 'js-mode-hook
	  (lambda ()
	    ;; 自动不全括号
	    (autopair-mode +1)
	    ;; 设置缩进两个空格
	    (setq js2-basic-offset 2)
	    ;; 设置保存后自动格式化代码
	    (prettier-js-mode +1)
	    ;; 设置 CSS 的自动完成
	    (company-mode +1)
	    ;; 设置关闭自动换行
	    (setq truncate-lines t)
	    ;; 开启显示行号
	    (display-line-numbers-mode +1)
	    ;; 开启代码折叠子模式
	    (hs-minor-mode +1)
	    ;; 开启代码折叠快捷键
	    (global-set-key (kbd "s-/") 'hs-toggle-hiding)))

(provide 'pkg-js-mode)
