;; 设置特定文件直接进入 TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))

;; TSX 设置
(add-hook 'rjsx-mode-hook
          (lambda ()
	    ;; 附加 Web 开发的插件
	    (web-dev-attached)
	    ;; 设置缩进两个空格
	    (setq js2-basic-offset 2)))

;; 设置特定文件直接进入 TS
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; TS 设置
(add-hook 'typescript-mode-hook
          (lambda ()
	    ;; 附加 Web 开发的插件
	    (web-dev-attached)
	    ;; 设置缩进两个空格
	    (setq typescript-indent-level 2)))

(provide 'pkg-typescript-mode)

