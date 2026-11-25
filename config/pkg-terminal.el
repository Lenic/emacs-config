;; -*- lexical-binding: t -*-

;; 终端 Emacs 与终端程序（Ghostty / iTerm2）的配合：
;;
;; 1. Command 键：Emacs 占用终端时，让终端切到专门的按键配置，把终端自己占用的
;;    Command 组合键让给 Emacs 当 Meta；退出、挂起或关闭终端时切回来，
;;    所以其他标签页和 shell 不受影响。
;;    - Ghostty：用 AppleScript 激活 ~/.config/ghostty/config 里的 emacs 按键表
;;    - iTerm2：用 OSC 1337 SetProfile 切到「Emacs」profile（键位在 iTerm2 设置里配置），
;;      退出时切回启动这个标签页时的 profile（$ITERM_PROFILE）
;; 2. 光标颜色、背景色跟随 Emacs 主题（终端里 `set-cursor-color' 只改 frame 参数，
;;    不影响终端光标；浅色主题下终端自带的光标几乎看不见）
;;
;; 按每个 frame 自己的环境变量判断终端程序，所以 daemon 下从不同终端连进来的
;; emacsclient 各自生效；GUI frame、SSH 和其他终端不受影响。

;;;; 判断终端程序

(defun my/term--kind (&optional terminal)
  "TERMINAL 运行在哪个终端程序里：`ghostty'、`iterm'，都不是时返回 nil。"
  (when (and (eq (terminal-live-p terminal) t)
             ;; daemon 的初始终端不是真终端，但也会继承启动 daemon 时的 TERM_PROGRAM
             (not (equal (terminal-name terminal) "initial_terminal")))
    (pcase (getenv "TERM_PROGRAM" (car (frames-on-display-list terminal)))
      ("ghostty" 'ghostty)
      ("iTerm.app" 'iterm))))

(defun my/term--send (terminal string)
  "把转义序列 STRING 写到 TERMINAL；终端已挂起或正在关闭时忽略错误。"
  (ignore-errors (send-string-to-terminal string terminal)))

(defun my/term--hex (color)
  "把颜色 COLOR 转成 (R G B)，每项 0~255；不是有效颜色时返回 nil。"
  (when-let* ((rgb (and (stringp color)
                        (tty-color-standard-values (downcase color)))))
    (mapcar (lambda (v) (ash v -8)) rgb)))

;;;; Ghostty：按 tty 定位终端，借助 Ghostty 的 AppleScript

(defun my/ghostty--tty (terminal)
  "返回 TERMINAL 的 tty 设备路径。"
  (let ((name (terminal-name terminal)))
    (if (equal name "/dev/tty")
        ;; emacs -nw 的初始终端名是 /dev/tty，需要查出真实设备
        (concat "/dev/"
                (string-trim (shell-command-to-string
                              (format "ps -o tty= -p %d" (emacs-pid)))))
      name)))

(defun my/ghostty--perform (tty action)
  "让 Ghostty 在 tty 为 TTY 的终端上执行 ACTION；异步执行，不阻塞 Emacs。"
  (when (executable-find "osascript")
    (call-process
     "osascript" nil 0 nil "-e"
     (format "if application \"Ghostty\" is running then
  tell application \"Ghostty\"
    try
      perform action %S on (first terminal whose tty is %S)
    end try
  end tell
end if" action tty))))

;;;; 激活与取消

(defun my/term-activate (&optional terminal)
  "在 TERMINAL 上切到 Emacs 专用的按键配置，并同步光标和背景颜色。"
  (let ((kind (my/term--kind terminal)))
    (when (and kind (not (terminal-parameter terminal 'my/term-kind)))
      (set-terminal-parameter terminal 'my/term-kind kind)
      (pcase kind
        ('ghostty
         (let ((tty (my/ghostty--tty terminal)))
           (set-terminal-parameter terminal 'my/ghostty-tty tty)
           (my/ghostty--perform tty "activate_key_table:emacs")))
        ('iterm
         (set-terminal-parameter
          terminal 'my/iterm-profile
          (or (getenv "ITERM_PROFILE" (car (frames-on-display-list terminal)))
              "Default"))
         (my/term--send terminal "\e]1337;SetProfile=Emacs\a")))
      (my/term--send-cursor-color terminal (my/term--default-cursor-color))
      (my/term--send-background terminal))))

(defun my/term-deactivate (&optional terminal)
  "取消 TERMINAL 上的 Emacs 专用按键配置，恢复终端自己的光标和背景颜色。"
  (let ((kind (terminal-parameter terminal 'my/term-kind)))
    (when kind
      (pcase kind
        ('ghostty
         ;; 先通知 Ghostty 取消按键表，它不需要写终端，挂起或关闭时也能成功
         (my/ghostty--perform (terminal-parameter terminal 'my/ghostty-tty)
                              "deactivate_key_table")
         ;; 恢复主题自带的光标颜色（OSC 112）和背景色（OSC 111）
         (my/term--send terminal "\e]112\a\e]111\a"))
        ('iterm
         ;; 切回原 profile，profile 里的光标和背景颜色也一起恢复
         (my/term--send terminal
                        (format "\e]1337;SetProfile=%s\a"
                                (terminal-parameter terminal 'my/iterm-profile)))))
      (dolist (param '(my/term-kind my/term-cursor my/term-bg
                       my/ghostty-tty my/iterm-profile))
        (set-terminal-parameter terminal param nil)))))

(defun my/term-deactivate-all ()
  "退出 Emacs 时取消所有终端上的 Emacs 专用按键配置。"
  (mapc #'my/term-deactivate (terminal-list)))

(defun my/term--before-suspend-tty (&optional terminal)
  "emacsclient 终端挂起前先取消；`suspend-tty-functions' 在挂起之后才运行，那时已经写不了终端。"
  (my/term-deactivate terminal))

;;;; 光标颜色

(defun my/term--default-cursor-color ()
  "默认光标颜色，与 pkg-input.el 中 pyim 关闭时的颜色一致。"
  "#6CA8F0")

(defun my/term--send-cursor-color (terminal color)
  "把 TERMINAL 的光标设成 COLOR；与上次相同时不重复发送。
pyim 的指示器会周期性调用 `set-cursor-color'，去重可避免反复输出转义序列。"
  (let ((kind (terminal-parameter terminal 'my/term-kind))
        (rgb (my/term--hex color)))
    (when (and kind rgb
               (not (equal color (terminal-parameter terminal 'my/term-cursor))))
      (set-terminal-parameter terminal 'my/term-cursor color)
      (my/term--send terminal
                     (pcase kind
                       ('ghostty (apply #'format "\e]12;rgb:%02x/%02x/%02x\a" rgb))
                       ('iterm (apply #'format "\e]1337;SetColors=curbg=%02x%02x%02x\a" rgb)))))))

(defun my/term--after-set-cursor-color (color)
  "让 `set-cursor-color'（如 pyim 的中英文光标）在终端里也生效。"
  (my/term--send-cursor-color (frame-terminal) color))

;;;; 背景颜色
;; 把终端背景设成 Emacs 主题 default 面孔的背景色，窗口 padding 留白、透明区域都和主题一致

(defun my/term--send-background (terminal)
  "把 TERMINAL 的背景设成当前主题 default 面孔的背景色；与上次相同时不重复发送。"
  (let* ((kind (terminal-parameter terminal 'my/term-kind))
         (frame (car (frames-on-display-list terminal)))
         (color (and frame (face-background 'default frame t)))
         (rgb (my/term--hex color)))
    (when (and kind rgb
               (not (equal color (terminal-parameter terminal 'my/term-bg))))
      (set-terminal-parameter terminal 'my/term-bg color)
      (my/term--send terminal
                     (pcase kind
                       ('ghostty (apply #'format "\e]11;rgb:%02x/%02x/%02x\a" rgb))
                       ('iterm (apply #'format "\e]1337;SetColors=bg=%02x%02x%02x\a" rgb)))))))

(defun my/term-sync-colors (&rest _)
  "主题切换后，把所有终端的光标和背景同步为当前主题的颜色。"
  (dolist (term (terminal-list))
    (my/term--send-cursor-color term (my/term--default-cursor-color))
    (my/term--send-background term)))

;;;; 挂钩子

(advice-add 'set-cursor-color :after #'my/term--after-set-cursor-color)
(advice-add 'suspend-tty :before #'my/term--before-suspend-tty)
(add-hook 'enable-theme-functions #'my/term-sync-colors)            ; 主题切换
(add-hook 'tty-setup-hook #'my/term-activate)                       ; 新终端（含 emacsclient -t）
(add-hook 'resume-tty-functions #'my/term-activate)                 ; emacsclient 终端恢复
(add-hook 'suspend-hook #'my/term-deactivate)                       ; emacs -nw 下 C-z
(add-hook 'suspend-resume-hook #'my/term-activate)
(add-hook 'delete-terminal-functions #'my/term-deactivate)          ; 关闭 emacsclient 终端
(add-hook 'kill-emacs-hook #'my/term-deactivate-all)

;; emacs -nw 的终端初始化在读取配置之后，会触发上面的 tty-setup-hook；
;; 这里兜底配置加载时终端已经初始化完毕的情况
(when (terminal-parameter nil 'terminal-initted)
  (my/term-activate))

;;;; 其他程序把 Emacs 当编辑器调用（eca / ec --as-editor）
;; $VISUAL/$EDITOR 设为 eca（ec 的软链接），Claude Code 的 Ctrl+G 等调用时，
;; ec 给 emacsclient 设置 EC_INTENT=editor。这个变量随 -t 发给 server，挂在该客户端的
;; 终端 frame 上，daemon 自己的环境里没有，所以要用 (getenv VAR frame) 读；
;; 每个客户端各自一份，同时调用也不会串。
;;
;; 这样打开的 buffer：
;; - C-c C-c 保存并返回，C-c C-k 放弃修改并返回，和 git commit（with-editor）的习惯一致。
;;   git commit / rebase 的 buffer 已经有 `with-editor-mode'，不再重复开启，保留 magit 的处理。
;; - 不记进 recentf 和文件名历史（参照 git-commit 对 COMMIT_EDITMSG 的处理），
;;   临时文件不会堆在最近文件列表里。
;;
;; 手动 `ec 文件' 打开的 buffer 是普通编辑，不受影响。

(defvar recentf-exclude)
(defvar recentf-list)

(defvar-local my/server-as-editor nil
  "非 nil 表示这个 buffer 是其他程序通过 eca / ec --as-editor 打开的。")

(defun my/server-as-editor-p ()
  "当前客户端是否带着编辑器意图（EC_INTENT=editor）。
`server-visit-hook' 运行时，-t 新建的 frame 已经选中，但客户端还没登记到
`server-buffer-clients'，所以从 frame 的环境里读。"
  (equal (getenv "EC_INTENT" (selected-frame)) "editor"))

(defun my/server-recentf-exclude-p (file)
  "给 `recentf-exclude' 用：编辑器意图打开的 FILE 不记，保存时也不会再记进去。"
  (let ((buf (get-file-buffer file)))
    (and buf (buffer-local-value 'my/server-as-editor buf))))

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude #'my/server-recentf-exclude-p))

(defun my/server-forget-file (file)
  "把打开时已经记下的 FILE 从文件名历史和 recentf 里去掉。
`server-visit-files' 和 recentf 的 `find-file-hook' 都在 `server-visit-hook' 之前记录。"
  (let* ((file (expand-file-name file))
         (other-p (lambda (f) (not (string= (expand-file-name f) file)))))
    (setq file-name-history (seq-filter other-p file-name-history))
    (when (boundp 'recentf-list)
      (setq recentf-list (seq-filter other-p recentf-list)))))

(defun my/server-finish ()
  "保存当前 buffer，结束 emacsclient 编辑；终端 frame 随之关闭，回到调用方。"
  (interactive)
  (save-buffer)
  (server-edit))

(defun my/server-abort ()
  "放弃未保存的修改，结束 emacsclient 编辑；文件内容保持打开时的样子。"
  (interactive)
  (when (buffer-modified-p)
    (revert-buffer t t t))
  (server-edit))

(define-minor-mode my/server-edit-mode
  "eca 打开的 buffer：C-c C-c 保存并返回，C-c C-k 放弃并返回。"
  :lighter " Client"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'my/server-finish)
            (define-key map (kbd "C-c C-k") #'my/server-abort)
            map)
  (when my/server-edit-mode
    (message "C-c C-c 保存并返回，C-c C-k 放弃并返回")))

(defun my/server-visit ()
  "带编辑器意图打开时：不记历史，开启 `my/server-edit-mode'（git commit 等除外）。"
  (when (my/server-as-editor-p)
    (setq my/server-as-editor t)
    (my/server-forget-file buffer-file-name)
    (unless (bound-and-true-p with-editor-mode)
      (my/server-edit-mode 1))))

(defun my/server-done ()
  "结束编辑后 buffer 可能还留在 Emacs 里（打开前就存在的文件），恢复成普通 buffer。"
  (setq my/server-as-editor nil)
  (my/server-edit-mode -1))

(add-hook 'server-visit-hook #'my/server-visit)
(add-hook 'server-done-hook #'my/server-done)

(provide 'pkg-terminal)
