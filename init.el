;; 设置中国镜像源，提升第三方包的下载速度
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config")

;; 加载基础全局配置
(require 'pkg-basic)

;; 加载 org-mode 配置
(require 'pkg-org)

;; 加载 Web 开发配置
(require 'pkg-web)

(custom-set-variables
 '(org-agenda-files (quote ("~/task/me.inbox.org" "~/task/mj.org")))
 '(package-selected-packages
   '(solarized-theme web-mode magit neotree projectile spacemacs-theme rjsx-mode prettier-js emmet-mode counsel company)))
