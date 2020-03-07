;; 设置特定文件直接进入该模式
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))

;; 格式化代码
(add-hook 'rjsx-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

;; 设置 Company
(add-hook 'rjsx-mode-hook  'global-company-mode)

;; 设置默认显示行号
(add-hook 'rjsx-mode-hook (lambda () (display-line-numbers-mode t)))

;; 快速编写 HTML 代码
(add-hook 'rjsx-mode-hook  'emmet-mode)
(setq emmet-expand-jsx-className? t)

;; 设置默认缩进
(add-hook 'rjsx-mode-hook (lambda () (setq js2-basic-offset 2)))

;; 设置关闭自动换行
(add-hook 'rjsx-mode-hook (lambda () (setq truncate-lines t)))

(provide 'pkg-rjsx-mode)
