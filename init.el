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

;; 设置中国镜像源，提升第三方包的下载速度
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")))
;; (setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; 设置可以读取的最大容量为 3MB
(setq read-process-output-max (* 3 1024 1024))

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
(setq use-package-always-ensure t)

;; 禁用 cl 库警告
(setq byte-compile-warnings '(cl-functions))

;; 查看已安装的包数量
;; (length package-alist)

(add-to-list 'load-path "~/.emacs.d/config")

;; 加载基础全局配置
(require 'pkg-global)

;; 加载 Dired 模式配置
(require 'pkg-dired)

;; 加载基础配置
(require 'pkg-basic)

;; 加载 org-mode 配置
;; (require 'pkg-org)
;; (custom-set-variables '(org-agenda-files (quote ("~/task/inbox.org" "~/task/me.inbox.org"))))

;; 加载开发配置
(require 'pkg-dev);

;; 加载主题配置
(require 'pkg-theme)

;; 加载其它语言配置
(require 'pkg-lang)
