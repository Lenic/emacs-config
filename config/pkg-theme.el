;; -*- lexical-binding: t -*-

;; :defer t 只安装、不加载，主题要靠 load-theme 启用；
;; 包的 autoloads 已经把主题目录加进 `custom-theme-load-path'，所以放在 :init 里就能找到
(use-package zenburn-theme
  :defer t
  :init
  ;; 终端 frame 取不到背景色，Emacs 只能按 TERM 猜明暗，
  ;; 钉成 dark，让 magit diff 等按 (background dark) 取色的面孔用对颜色
  (setq frame-background-mode 'dark)
  (load-theme 'zenburn t))

(with-eval-after-load 'zenburn-theme
  (custom-set-faces
   '(region ((t (:background "#5F5F5F" :extend t))))))

(provide 'pkg-theme)
