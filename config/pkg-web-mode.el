;; 设置特定文件直接进入该模式
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

;; 设置语法高亮模式
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; 格式化代码
(add-hook 'web-mode-hook
	  (lambda ()
	    ;; 自动不全括号
	    (autopair-mode +1)
	    ;; Tide 安装
	    (tide-setup)
	    ;;(tide-hl-identifier-mode +1)
	    ;; 当 tsserver 服务没有启动时自动重新启动
	    (unless (tide-current-server) (tide-restart-server))
	    ;; 设置默认的缩进
	    (setq web-mode-markup-indent-offset 2)
	    (setq web-mode-css-indent-offset 2)
	    (setq web-mode-code-indent-offset 2)
	    ;; emmet
	    (emmet-mode +1)
	    (setq emmet-expand-jsx-className? t)
	    ;; 设置关闭自动换行
	    (setq truncate-lines t)
	    ;; 设置 Company
	    (company-mode +1)
	    ;; 开启显示行号
	    (display-line-numbers-mode +1)
	    ;; 格式化代码
	    (prettier-js-mode +1)
	    ;; 开启 Git 变更提示
	    (diff-hl-mode +1)
	    ;; 开启 JSX 文件的代码折叠
	    (hs-minor-mode +1)))

(provide 'pkg-web-mode)

