;; 设置中国镜像源，提升第三方包的下载速度
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "8d805143f2c71cfad5207155234089729bb742a1cb67b7f60357fdd952044315" default)))
 '(org-agenda-files (quote ("~/task/me.inbox.org" "~/task/mj.org")))
 '(package-selected-packages
   (quote
    (rjsx-mode company ## 0blayout counsel swiper ivy htmlize neotree solarized-theme web-mode spacemacs-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :weight bold))))
 '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :weight bold))))
 '(org-level-3 ((t (:foreground "#67b11d" :weight normal)))))

(add-to-list 'load-path "~/.emacs.d/config")

;; 加载基础全局配置
(require 'pkg-basic)

;; 加载 org-mode 配置
(require 'pkg-org)

;; 加载 Web 开发配置
(require 'pkg-web)
