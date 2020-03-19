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
   (quote
    (autopair all-the-icons tide diff-hl htmlize solarized-theme web-mode magit neotree projectile spacemacs-theme prettier-js emmet-mode counsel company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
