;; 设置特定文件直接进入该模式
(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))

;; 格式化代码
;; (add-hook 'rjsx-mode-hook #'(lambda ()
;;                             (enable-minor-mode
;;                              '("\\.jsx?\\'" . prettier-js-mode))))

;; 设置 Company
(add-hook 'rjsx-mode-hook  'global-company-mode)

;; 设置默认显示行号
(add-hook 'rjsx-mode-hook (lambda () (display-line-numbers-mode t)))

;; 全局开启 Git 更新显示
(global-diff-hl-mode)

;; 快速编写 HTML 代码
(add-hook 'rjsx-mode-hook  'emmet-mode)
(setq emmet-expand-jsx-className? t)

;; 设置默认缩进
(add-hook 'rjsx-mode-hook (lambda () (setq js2-basic-offset 2)))

;; 设置关闭自动换行
(add-hook 'rjsx-mode-hook (lambda () (setq truncate-lines t)))

;; TypeScript 设置
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; (add-hook 'rjsx-mode-hook #'(lambda ()
;;                             (enable-minor-mode
;;                              '("\\.tsx?\\'" . prettier-js-mode))))

(provide 'pkg-rjsx-mode)
