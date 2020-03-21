;; 设置特定文件直接进入该模式
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

;; 设置语法高亮模式
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; 格式化代码
(add-hook 'web-mode-hook
	  (lambda ()
	    ;; 附加 Web 开发的插件
	    (web-dev-attached)
	    ;; 设置默认的缩进
	    (setq web-mode-markup-indent-offset 2)
	    (setq web-mode-css-indent-offset 2)
	    (setq web-mode-code-indent-offset 2)))

(provide 'pkg-web-mode)

