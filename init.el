;; 优化启动时的垃圾回收阈值以加速启动
(defvar my/normal-gc-cons-threshold (* 16 1024 1024)
  "Normal garbage collection threshold after startup.")

(setq gc-cons-threshold most-positive-fixnum)

(defun my/restore-gc-threshold ()
  "Restore garbage collection threshold after startup."
  (setq gc-cons-threshold my/normal-gc-cons-threshold))

(add-hook 'after-init-hook #'my/restore-gc-threshold)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)


;; 设置可以读取的最大容量为 3MB
(setq read-process-output-max (* 3 1024 1024))

;; 缓解在快速移动时大量代码的语法高亮
(setq redisplay-skip-fontification-on-input t)

;; 显示垃圾回收信息，这个可以作为调试用
(setq garbage-collection-messages t)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'exec-path-from-shell))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-minimum-reported-time 0.1) ; 超过 0.1 秒才报告加载时间

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

;; 加载开发配置
(require 'pkg-dev)

;; 加载主题配置
(require 'pkg-theme)

;; 加载其它语言配置
(require 'pkg-lang)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9af2b1c0728d278281d87dc91ead7f5d9f2287b1ed66ec8941e97ab7a6ab73c0"
     "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"
     default))
 '(ivy-more-chars-alist '((counsel-grep . 2) (t . 2)))
 '(package-selected-packages
   '(amx beacon corfu counsel dap-mode dockerfile-mode elfeed emmet-mode
         exec-path-from-shell expand-region flycheck gcmh git-gutter
         git-timemachine gptel htmlize llm lsp-pyright lsp-tailwindcss
         lsp-ui magit multiple-cursors neotree orderless org-bullets
         origami prettier-js projectile pyim-basedict python-black rg
         separedit spacemacs-theme symbol-overlay treesit-auto
         undo-tree web-mode xclip yafolding yaml-mode yasnippet))
 '(pyim-dicts '((:name "mine" :file "~/.emacs.d/pyim/mine.pyim"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
