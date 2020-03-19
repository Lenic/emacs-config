;; 设置 CSS 及其它 CSS 预处理语言
(add-hook 'css-mode-hook
	  (lambda ()
	    ;; 设置自动缩进的宽度
	    (setq css-indent-offset 2)
	    ;; 设置保存后自动格式化代码
	    (prettier-js-mode +1)
	    ;; 设置 CSS 的自动完成
	    (company-mode +1)
	    ;; 开启 Git 变更提示
	    (diff-hl-mode +1)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; 设置打开 NeoTree
(global-set-key (kbd "C-x o") 'neotree-dir)

;; 设置 Git 管理快捷键
(global-set-key (kbd "C-x g") 'magit-status)

;; 加载 Web-Mode
(require 'pkg-web-mode)

;; 加载 TypeScript-Mode
(require 'pkg-typescript-mode)

(provide 'pkg-web)
