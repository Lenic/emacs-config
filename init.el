;; (let (
;;       ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
;;       (gc-cons-threshold most-positive-fixnum)
;;       ;; 清空避免加载远程文件的时候分析文件。
;;       (file-name-handler-alist nil))
;;   (require 'benchmark-init-modes)
;;   (require 'benchmark-init)
;;   (benchmark-init/activate)
;;
;;   ;; 下面才写你的其它配置
;;   )

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
;; 设置中国镜像源，提升第三方包的下载速度
;; (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")))
;; (setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(setenv "PATH" (concat "/usr/local/bin:/opt/homebrew/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/Users/lenic/.nvm/versions/node/v16.17.0/lib" "/usr/local/bin" "/opt/homebrew/bin")))

;; 设置可以读取的最大容量为 3MB
(setq read-process-output-max (* 3 1024 1024))

;; 缓解在快速移动时大量代码的语法高亮
(setq redisplay-skip-fontification-on-input t)

;; 垃圾回收设置阈值 100MB
(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
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
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "e09401ab2c457e2e4d8b800e1c546dbc8339dc33b2877836ba5d9b6294ae6e55" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(ivy-more-chars-alist '((counsel-grep . 2) (t . 2)))
 '(org-agenda-files '("~/task/inbox.org" "~/task/me.inbox.org"))
 '(package-selected-packages
   '(vscode-dark-plus-theme zenburn-theme yaml-mode yafolding xclip with-venv web-mode use-package undo-tree typescript-mode tree-sitter-langs symbol-overlay spacemacs-theme spaceline solarized-theme separedit rg python-black pyim-basedict pyim py-isort projectile prettier-js popup origami org-bullets neotree multiple-cursors meow magit lsp-ui lsp-tailwindcss lsp-python-ms lsp-pyright json-mode htmlize highlight-blocks git-timemachine git-gutter flycheck expand-region emmet-mode elpy elfeed dockerfile-mode diff-hl dap-mode counsel blacken benchmark-init beacon ace-jump-mode)))

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



