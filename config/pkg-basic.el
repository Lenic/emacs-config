;; 设置 frame 的缺省值
(setq default-frame-alist '((tool-bar-lines . 0) ;; 不显示工具栏
                            (font . "Sarasa Term SC Nerd 14") ;; 设置字体
                            (width . 140) ;; 设置窗口宽度
                            (height . 30) ;; 设置窗口高度
                            (left . 0) ;; 设置窗口左边沿在屏幕上的坐标
                            (top . 0))) ;; 设置窗口上边沿在屏幕上的坐标

(add-hook 'after-make-frame-functions (lambda (new-frame)
                                        ;; 激活这个新的 frame
                                        (select-frame new-frame)))

;;;###autoload
(defun my/maximal-font ()
  "Format the current file with ESLint."
  (interactive)
  (set-face-attribute 'default nil :font "Sarasa Mono SC Nerd 16" ))

;;;###autoload
(defun my/normal-font ()
  "Format the current file with ESLint."
  (interactive)
  (set-face-attribute 'default nil :font "Sarasa Mono SC Nerd 14" ))

;; 设置 Emacs 的缺省工作路径
(setq default-directory "~/")

;; 设置平滑滚动
(setq scroll-step            1
      scroll-conservatively  10000)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; 保持鼠标所在行数不变屏幕向下滚动一行
(global-set-key (kbd "M-n") 'scroll-up-line)

;; 保持鼠标所在行数不变屏幕向上滚动一行
(global-set-key (kbd "M-p") 'scroll-down-line)

;; 滚动半屏设置
(defun window-half-height ()
  (max 1 (/ (- (1- (window-height (selected-window))) 4) 2)))
(defun scroll-up-half ()
  (interactive)
  (scroll-up (window-half-height)))
(defun scroll-down-half ()
  (interactive)
  (scroll-down (window-half-height)))
(global-set-key (kbd "M-N") 'scroll-up-half)
(global-set-key (kbd "M-P") 'scroll-down-half)

;; 设置系统内置的 isearch 在删除待搜索字符时不变动光标位置
(use-package isearch
  :ensure nil
  :defer 10
  :bind (:map isearch-mode-map
              ;; consistent with ivy-occur
              ("C-c C-o"                   . isearch-occur)
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  ;; 设置每次前进或者后退搜索后将目标位置放置在屏幕垂直居中
  (defadvice isearch-repeat-forward (after isearch-repeat-forward-recenter activate) (recenter))
  (defadvice isearch-repeat-backward (after isearch-repeat-backward-recenter activate) (recenter))
  (ad-activate 'isearch-repeat-forward)
  (ad-activate 'isearch-repeat-backward)
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s "))

;; 在 modeline 上显示所有的按键和执行的命令
;; (use-package keycast
;;   :defer 5
;;   :init
;;   (keycast-mode-line-mode t))

;; 子弹窗需要依赖的包
(use-package posframe
  :defer 5)

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  :defer 3
  :when (or (memq window-system '(mac ns x))
        (unless cabins--os-win
          (daemonp)))
  :init (exec-path-from-shell-initialize))

(provide 'pkg-basic)
