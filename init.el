(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; 设置可以读取的最大容量为 3MB
(setq read-process-output-max (* 3 1024 1024))

;; 缓解在快速移动时大量代码的语法高亮
(setq redisplay-skip-fontification-on-input t)

;; 启动时临时增加阈值
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; 启动后恢复为推荐值
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 200000000)  ; 恢复为 200 MB
            (setq gc-cons-percentage 0.6)))     ; 恢复为 60%

;; 显示垃圾回收信息，这个可以作为调试用
(setq garbage-collection-messages t)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-minimum-reported-time t)

;; 禁用 cl 库警告
(setq byte-compile-warnings '(cl-functions))

;; 查看已安装的包数量
;; (length package-alist)

(add-to-list 'load-path "~/.emacs.d/config")

;; 加载基础配置
(require 'pkg-basic)

;; 加载基础全局配置
(require 'pkg-global)

;; 加载 Dired 模式配置
(require 'pkg-dired)

;; 加载 org-mode 配置
(require 'pkg-org)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-more-chars-alist '((counsel-grep . 2) (t . 2)))
 '(org-agenda-files '("~/task/inbox.org" "~/task/me.inbox.org"))
 '(package-selected-packages
   '(zenburn-theme yaml-mode yafolding xclip with-venv web-mode use-package undo-tree typescript-mode tree-sitter-langs symbol-overlay spacemacs-theme spaceline solarized-theme separedit rg python-black pyim-basedict pyim py-isort projectile prettier-js popup origami org-bullets neotree multiple-cursors meow magit lsp-ui lsp-tailwindcss lsp-python-ms lsp-pyright json-mode htmlize highlight-blocks git-timemachine git-gutter flycheck expand-region emmet-mode elpy elfeed dockerfile-mode diff-hl dap-mode counsel blacken benchmark-init beacon amx ace-jump-mode)))

;; 加载开发配置
(require 'pkg-dev)

;; 加载主题配置
(require 'pkg-theme)

;; 加载其它语言配置
(require 'pkg-lang)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-more-chars-alist '((counsel-grep . 2) (t . 2)))
 '(org-agenda-files '("~/task/inbox.org" "~/task/me.inbox.org"))
 '(package-selected-packages
   '(zenburn-theme yaml-mode yafolding xclip with-venv web-mode use-package undo-tree typescript-mode tree-sitter-langs symbol-overlay spacemacs-theme spaceline solarized-theme separedit rg python-black pyim-basedict pyim py-isort projectile prettier-js popup origami org-bullets neotree multiple-cursors meow magit lsp-ui lsp-tailwindcss lsp-python-ms lsp-pyright json-mode htmlize highlight-blocks git-timemachine git-gutter flycheck expand-region emmet-mode elpy elfeed dockerfile-mode diff-hl dap-mode counsel blacken benchmark-init beacon ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
