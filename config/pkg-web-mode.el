;; 设置特定文件直接进入该模式
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))

;; 格式化代码
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))

;; 设置 Company
(add-hook 'web-mode-hook  'global-company-mode)

;; 设置默认显示行号
(add-hook 'web-mode-hook (lambda () (display-line-numbers-mode t)))

;; 快速编写 HTML 代码
(add-hook 'web-mode-hook  'emmet-mode)
(setq emmet-expand-jsx-className? t)

;; 设置默认的缩进
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (display-line-numbers-mode t)) ;; 设置默认显示行号
(add-hook 'web-mode-hook  'web-mode-init-hook)

;; 设置语法高亮模式
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(provide 'pkg-web-mode)
