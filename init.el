;; 设置中国镜像源，提升第三方包的下载速度
(setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; (setq use-package-verbose t)
(setq use-package-always-ensure t)

(setq byte-compile-warnings '(cl-functions))

(add-to-list 'load-path "~/.emacs.d/config")

;; 加载基础全局配置
(require 'pkg-global)

;; 加载 Dired 模式配置
(require 'pkg-dired)

;; 加载基础配置
(require 'pkg-basic)

;; 加载 org-mode 配置
(require 'pkg-org)

;; 加载开发配置
(require 'pkg-dev);

;; 加载主题配置
(require 'pkg-theme)

;; 加载其它语言配置
(require 'pkg-lang)

(custom-set-variables '(org-agenda-files (quote ("~/task/inbox.org" "~/task/me.inbox.org"))))
