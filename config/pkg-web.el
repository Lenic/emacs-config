;; 设置 CSS 的自动完成
(add-hook 'css-mode-hook  'global-company-mode)

;; 设置保存后自动格式化代码
(add-hook 'css-mode-hook  'prettier-js-mode)

;; 设置自动缩进的宽度
(add-hook 'css-mode-hook (lambda () (setq css-indent-offset 2)))

;; 添加匹配某条件时才开启的辅模式
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

;; 加载 Web-Mode
;; (require 'pkg-web-mode)

;; 加载 rjsx-mode
(require 'pkg-rjsx-mode)

(provide 'pkg-web)
