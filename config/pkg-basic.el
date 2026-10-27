;; -*- lexical-binding: t -*-

(defun my/activate-new-frame (new-frame)
  "Activate the newly created frame NEW-FRAME."
  (select-frame new-frame))
(add-hook 'after-make-frame-functions #'my/activate-new-frame)

(defun my/maximal-font ()
  "Switch the default font to a larger size (16pt)."
  (interactive)
  (set-face-attribute 'default nil :font "Sarasa Term SC Nerd 16" ))

(defun my/normal-font ()
  "Switch the default font to the normal size (14pt)."
  (interactive)
  (set-face-attribute 'default nil :font "Sarasa Term SC Nerd 14" ))

(defun my/minimal-font ()
  "Switch the default font to a smaller size (12pt)."
  (interactive)
  (set-face-attribute 'default nil :font "Sarasa Term SC Nerd 12" ))

(defun my/clear-kill-ring ()
  "Clear the kill ring (copy-paste stack)."
  (interactive)
  (setq kill-ring nil)
  (setq kill-ring-yank-pointer nil))

;; 设置 Emacs 的缺省工作路径：default-directory 是永久 buffer-local 变量，
;; 用 setq 只会改到「加载本文件时恰好是当前 buffer」的那一个 buffer
(setq-default default-directory "~/")

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
(defun my/scroll-half-page (direction)
  "按半屏高度滚动当前窗口。DIRECTION 为 1 时向下翻，为 -1 时向上翻。"
  (scroll-up (* direction (max 1 (/ (- (window-height) 5) 2)))))
(global-set-key (kbd "M-N") (lambda () (interactive) (my/scroll-half-page 1)))
(global-set-key (kbd "M-P") (lambda () (interactive) (my/scroll-half-page -1)))

;; 水平滚动只作用于光标所在行：适合只有一行超长的文件（如压缩过的 HTML）。
;; 重绘时按窗口所显示 buffer 的局部值读取 `auto-hscroll-mode'，所以只影响当前 buffer
(define-minor-mode my/hscroll-current-line-mode
  "开启时只水平滚动光标所在行，关闭后恢复整个窗口一起滚动。"
  :lighter " HScroll"
  (if my/hscroll-current-line-mode
      (setq-local auto-hscroll-mode 'current-line)
    (kill-local-variable 'auto-hscroll-mode)))
;; 与 C-x x t（`toggle-truncate-lines'）放在一起，h 即 hscroll
(define-key ctl-x-x-map (kbd "h") #'my/hscroll-current-line-mode)

;; 设置系统内置的 isearch 在删除待搜索字符时不变动光标位置
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ;; consistent with ivy-occur
              ("C-c C-o"                   . isearch-occur)
              ([remap isearch-delete-char] . isearch-del-char))
  :config
  ;; 设置每次前进或者后退搜索后将目标位置放置在屏幕垂直居中
  (defun my/isearch-recenter (&rest _)
    "Recenter the window after isearch."
    (recenter))
  (advice-add 'isearch-repeat-forward :after #'my/isearch-recenter)
  (advice-add 'isearch-repeat-backward :after #'my/isearch-recenter)
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "%s/%s "))

;; 在 modeline 上显示所有的按键和执行的命令
;; (use-package keycast
;;   :defer 5
;;   :init
;;   (keycast-mode-line-mode t))

;; Settings for exec-path-from-shell
;; fix the PATH environment variable issue
(use-package exec-path-from-shell
  ;; :defer 3
  :when (or (memq window-system '(mac ns x))
        (unless cabins--os-win
          (daemonp)))
  :init (exec-path-from-shell-initialize))

(setopt mode-line-collapse-minor-modes
        '(projectile-mode
          whitespace-mode
          yas-minor-mode
          emmet-mode
          git-gutter-mode
          beacon-mode
          eldoc-mode
          prettier-js-mode
          eslintd-fix-on-save-mode
          my/lsp-workspaces-mode
          ))

;; macOS 上 Emacs 自己计算初始 frame 的位置，总是落在主显示器上，
;; 不管从 Spotlight 启动时鼠标在哪块屏幕。这里把 frame 挪到鼠标所在显示器
;; 的工作区并居中；后面的全屏会在 frame 当前所在的显示器上进行，所以要先挪再全屏。
;; 终端（TUI）frame 不满足 display-graphic-p，直接跳过
(defun my/move-frame-to-mouse-monitor (&optional frame)
  "把 FRAME（缺省为当前 frame）移动到鼠标所在显示器并居中。"
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (let* ((mouse (mouse-absolute-pixel-position))
             (monitor
              (seq-find
               (lambda (m)
                 (let ((g (alist-get 'geometry m)))
                   (and (<= (nth 0 g) (car mouse) (+ (nth 0 g) (nth 2 g) -1))
                        (<= (nth 1 g) (cdr mouse) (+ (nth 1 g) (nth 3 g) -1)))))
               (display-monitor-attributes-list frame)))
             (wa (alist-get 'workarea monitor)))
        (when wa
          (set-frame-position
           frame
           (+ (nth 0 wa) (max 0 (/ (- (nth 2 wa) (frame-pixel-width frame)) 2)))
           (+ (nth 1 wa) (max 0 (/ (- (nth 3 wa) (frame-pixel-height frame)) 2)))))))))

(add-hook 'after-make-frame-functions #'my/move-frame-to-mouse-monitor)
;; 非 daemon 启动时，第一个 frame 在加载配置之前就已经创建好，不会触发上面的 hook。
;; early-init 里让它隐藏创建，这里挪到位后再显示，避免主显示器被切回桌面
(unless (daemonp)
  (let ((frame (selected-frame)))
    (unwind-protect
        (my/move-frame-to-mouse-monitor frame)
      (when (display-graphic-p frame)
        (make-frame-visible frame)))))

;; 首个图形 frame 启动后全屏。用 set-frame-parameter 直接设为全屏，
;; 而不是 toggle-frame-fullscreen：toggle 是「切换」，frame 已经是全屏时
;; （比如 macOS 恢复了上次的窗口状态）反而会退出全屏
(defvar my/fullscreen-done nil
  "首个图形 frame 是否已经执行过全屏。")

(defun my/fullscreen-first-graphic-frame (&optional frame)
  "FRAME（缺省为当前 frame）是首个图形 frame 时，延迟 1 秒将其全屏。"
  (let ((frame (or frame (selected-frame))))
    (when (and (not my/fullscreen-done)
               (display-graphic-p frame))
      (setq my/fullscreen-done t)
      (run-at-time 1 nil #'set-frame-parameter frame 'fullscreen 'fullboth))))

(add-hook 'after-make-frame-functions #'my/fullscreen-first-graphic-frame)
;; 非 daemon 启动时，第一个 frame 在加载配置之前就已经创建好，不会触发上面的 hook
(unless (daemonp)
  (my/fullscreen-first-graphic-frame))

(provide 'pkg-basic)
