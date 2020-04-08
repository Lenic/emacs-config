;; 设置中国镜像源，提升第三方包的下载速度
(setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(add-to-list 'load-path "~/.emacs.d/config")

;; 加载基础全局配置
(require 'pkg-basic)

;; 加载 org-mode 配置
(require 'pkg-org)

;; 加载 Web 开发配置
(require 'pkg-web)

'(org-agenda-files ("~/task/inbox.org" "~/task/me.inbox.org" "~/task/mj.org"))
'(package-selected-packages
  (quote
   (multiple-cursors git-gutter+ amx markdown-mode ace-jump-mode dockerfile-mode rjsx-mode autopair tide diff-hl htmlize solarized-theme web-mode magit neotree projectile spacemacs-theme prettier-js emmet-mode counsel company)))
