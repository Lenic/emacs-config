;; -*- lexical-binding: t -*-

;;;; 主题按系统时间自动切换
;;
;; 白天用浅色主题，夜间用暗黑主题，GUI 与终端行为一致。
;;
;; 这里不做轮询：每次只登记一个「下一个时间段边界」的一次性定时器，
;; 触发后重新计算下一个边界再登记。好处是
;;   1. 切换发生在整点那一刻，不存在轮询周期造成的延迟；
;;   2. 不会随运行时间累积漂移；
;;   3. 睡眠 / 合盖唤醒后，过期的定时器会立刻补跑并自行修正到正确主题。

(defvar my/day-theme 'spacemacs-light
  "白天使用的浅色主题。")

(defvar my/dark-theme 'spacemacs-dark
  "夜间使用的暗黑主题。")

(defvar my/day-start-hour 7
  "白天时间段开始的整点，包含该点。")

(defvar my/day-end-hour 18
  "白天时间段结束的整点，不包含该点。")

;; 指示当前主题的类别：t 表示日间主题，nil 表示夜间主题
(defvar my/is-day-theme nil
  "Non-nil if currently using the day theme.")

(defvar my/theme-timer nil
  "指向下一次主题切换的一次性定时器。")

(defun my/theme-daytime-p (&optional time)
  "判断 TIME（缺省为当前时间）是否落在白天时间段内。"
  (let ((hour (nth 2 (decode-time time))))
    (and (>= hour my/day-start-hour)
         (< hour my/day-end-hour))))

(defun my/theme-expected ()
  "返回当前时刻应该使用的主题。"
  (if (my/theme-daytime-p) my/day-theme my/dark-theme))

(defun my/theme--boundary-time (hour day-offset)
  "返回相对今天偏移 DAY-OFFSET 天的 HOUR 整点时刻。
超出范围的日期由 `encode-time' 自行归一化，夏令时交界处传 -1 让它推断。"
  (let ((now (decode-time)))
    (encode-time (list 0 0 hour
                       (+ (nth 3 now) day-offset)
                       (nth 4 now)
                       (nth 5 now)
                       nil -1 nil))))

(defun my/theme-next-boundary ()
  "返回下一次需要切换主题的精确时刻。"
  (let* ((now (current-time))
         (hour (if (my/theme-daytime-p now) my/day-end-hour my/day-start-hour))
         (today (my/theme--boundary-time hour 0)))
    (if (time-less-p now today)
        today
      (my/theme--boundary-time hour 1))))

(defun my/theme-sync-frame (&optional frame)
  "让标题栏等 macOS 原生部件跟随当前主题的明暗。
FRAME 为 nil 时同步所有 frame，并更新 `default-frame-alist' 使新建的
frame 一出生就是正确的外观。"
  (let ((appearance (if my/is-day-theme 'light 'dark)))
    (unless frame
      (setf (alist-get 'ns-appearance default-frame-alist) appearance))
    (dolist (f (if frame (list frame) (frame-list)))
      (when (display-graphic-p f)
        (set-frame-parameter f 'ns-appearance appearance)))))

(defun my/theme-sync-background-mode ()
  "把 `frame-background-mode' 与所有 frame 的 background-mode 参数校正到当前主题。

`load-theme' 只改颜色，不会重算 frame 的 background-mode 参数 —— 那个参数
只在 frame 创建时算一次（见 `frame-set-background-mode'）。而不少包的面孔是靠
 `(background light)' / `(background dark)' 这类显示规格挑颜色的，典型如 magit
的 diff 面孔（`magit-diff-added' 等）、diff-mode、ediff、whitespace-mode。
不校正的话，切到浅色主题后它们会继续用暗色那一套，diff 在浅底上几乎看不清。

这里直接钉住 `frame-background-mode' 而不是让 Emacs 从背景色亮度去猜，
原因是终端 frame 的背景色是 \"unspecified-bg\"，亮度无从判断，
`frame--current-background-mode' 只能按 TERM 类型给个固定缺省值，
在终端里必然有一半时间是错的。
"
  (setq frame-background-mode (if my/is-day-theme 'light 'dark))
  ;; 新建的 frame 会在创建时自己读取上面的变量，这里只需要修正已存在的
  (mapc #'frame-set-background-mode (frame-list)))

(defun my/theme-apply (&optional force)
  "把主题切换到当前时刻应有的那一个。
FORCE 非 nil 时无条件重新加载。真正发生切换时返回新主题，否则返回 nil。"
  (let ((expected (my/theme-expected)))
    (when (or force (not (eq (car custom-enabled-themes) expected)))
      ;; disable-theme 会修改 custom-enabled-themes，所以遍历副本
      (mapc #'disable-theme (copy-sequence custom-enabled-themes))
      (load-theme expected t)
      (setq my/is-day-theme (eq expected my/day-theme))
      (my/theme-sync-background-mode)
      (my/theme-sync-frame)
      expected)))

(defun my/theme-schedule ()
  "取消旧的定时器，并登记下一个时间段边界的一次性定时器。"
  (when (timerp my/theme-timer)
    (cancel-timer my/theme-timer))
  (setq my/theme-timer
        (run-at-time (my/theme-next-boundary) nil #'my/theme--tick)))

(defun my/theme--tick ()
  "定时器回调：切换主题，然后登记下一次切换。"
  (my/theme-apply)
  (my/theme-schedule))

(defun my/theme-refresh ()
  "立即按系统时间校正主题，并重新登记下一次切换。"
  (interactive)
  (my/theme-apply t)
  (my/theme-schedule)
  (message "主题已校正为 %s，下一次切换 %s"
           (car custom-enabled-themes)
           (format-time-string "%m-%d %H:%M" (my/theme-next-boundary))))

(defun my/theme--on-focus-change ()
  "回到 Emacs 时校正一次主题，作为定时器被饿死时的兜底。"
  (when (and (frame-focus-state) (my/theme-apply))
    (my/theme-schedule)))

(defun my/theme-handle-new-frame (frame)
  "新建 FRAME 时校正主题。"
  (my/theme-apply)
  (my/theme-sync-frame frame))

(use-package spacemacs-theme
  :defer t
  :init
  (add-hook 'after-make-frame-functions #'my/theme-handle-new-frame)
  (add-function :after after-focus-change-function #'my/theme--on-focus-change)
  ;; daemon 模式下没有 frame，等第一个 frame 建立时再加载主题
  (unless (daemonp)
    (my/theme-apply t))
  ;; 定时器与 frame 无关，进程活着就一直挂着，一天只醒两次
  (my/theme-schedule))

(provide 'pkg-theme)
