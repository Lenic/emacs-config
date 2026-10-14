;; -*- lexical-binding: t -*-

(use-package dired
  :commands dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  ;; 在 macOS 上，ls 不支持 --dired 选项，而在 Linux 上则受支持
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  ;; 设置 Dired 模式显示方式：增加文件大小的可读性
  (setq dired-listing-switches "-alh"))


;; 只在调用 dirvish-side 时才加载 dirvish；没有开启 dirvish-override-dired-mode，
;; 普通的 dired 不受影响
(use-package dirvish
  :commands (dirvish-side)
  :bind (:map dirvish-mode-map
              ("TAB" . dirvish-subtree-toggle)   ; 展开/折叠目录，类似 neotree
              ("<backtab>" . dirvish-subtree-up)) ; 跳到所在子树的父目录
  :custom
  ;; 侧边栏固定在右侧（neotree 默认在左侧，两者可以同时打开）
  (dirvish-side-display-alist '((side . right) (slot . 0)))
  ;; 侧边栏宽度
  (dirvish-side-width 35)
  ;; 不自动展开当前文件的父目录
  (dirvish-side-auto-expand nil)
  ;; 显示目录的展开状态（+/-），使用 subtree-state 时 dirvish 会自动加载 dirvish-subtree
  (dirvish-side-attributes '(subtree-state))
  ;; 用 +/- 表示目录的折叠/展开状态
  (dirvish-subtree-state-style 'plus)
  ;; 从侧边栏打开文件时，不在 neotree 等特殊窗口里打开
  (dirvish-side-open-file-action #'my/dirvish-side-select-main-window)
  ;; dirvish-side 依赖 dirvish-override-dired-mode 的那组 advice，没有它侧边栏只是
  ;; 一个普通的 dired（所有详情都会显示，TAB 也不能展开子树）。但直接开启那个 mode
  ;; 会让所有 dired 都变成 dirvish，所以这里装同一组 advice，只在侧边栏 buffer 里生效
  :config
  (defun my/dirvish-side-buffer-p ()
    (when-let* ((dv (dirvish-curr))) (eq (dv-type dv) 'side)))

  (pcase-dolist (`(,sym ,fn ,how)
                 '((dired--find-file dirvish--find-entry :override)
                   (dired-find-alternate-file dirvish-find-alt-a :override)
                   (dired-do-find-marked-files dirvish-find-marked-files-a :override)
                   (dired-noselect dirvish-dired-noselect-a :around)
                   (dired-insert-subdir dirvish-insert-subdir-a :after)
                   (wdired-change-to-wdired-mode dirvish-wdired-enter-a :after)
                   (wdired-change-to-dired-mode dirvish--setup-dired :after)))
    (advice-add sym :around
                (pcase how
                  (:override (lambda (orig &rest args)
                               (apply (if (my/dirvish-side-buffer-p) fn orig) args)))
                  (:around (lambda (orig &rest args)
                             (if (my/dirvish-side-buffer-p)
                                 (apply fn orig args)
                               (apply orig args))))
                  (:after (lambda (orig &rest args)
                            (prog1 (apply orig args)
                              (when (my/dirvish-side-buffer-p) (apply fn args))))))
                `((name . ,(intern (format "my/dirvish-side-%s" fn))))))

  ;; 侧边栏 buffer 也会登记到 dired-buffers 里，普通 dired 打开同一目录时会复用它，
  ;; 这里让侧边栏以外查找 dired buffer 时跳过侧边栏的 buffer
  (advice-add 'dired-find-buffer-nocreate :around
              (lambda (orig &rest args)
                (if (my/dirvish-side-buffer-p)
                    (apply orig args)
                  (let ((dired-buffers
                         (seq-remove (lambda (e)
                                       (and (buffer-live-p (cdr e))
                                            (with-current-buffer (cdr e)
                                              (my/dirvish-side-buffer-p))))
                                     dired-buffers)))
                    (apply orig args))))
              '((name . my/dirvish-side-skip-side-buffers)))

  ;; 侧边栏用 -A 列出文件，不显示 . 和 ..（dirvish 在创建会话时读取 dired-listing-switches）
  (advice-add 'dirvish-side--new :around
              (lambda (orig &rest args)
                (let ((dired-listing-switches "-Alh"))
                  (apply orig args)))
              '((name . my/dirvish-side-listing-switches)))

  ;; 和 neotree 一样默认隐藏以 . 开头的文件，用 dired-x 的 C-x M-o（dired-omit-mode）切换。
  ;; 权限、大小、日期等详情由 dirvish-hide-details 隐藏
  (defun my/dirvish-side-setup-omit (&rest _)
    (setq-local dired-omit-files "\\`\\."
                ;; dired-omit-mode 默认还会隐藏 .elc 等扩展名，neotree 不隐藏，这里也不隐藏
                dired-omit-extensions nil
                dired-omit-verbose nil)
    (dired-omit-mode 1))
  (advice-add 'dirvish-side-root-conf :after #'my/dirvish-side-setup-omit)

  ;; 展开子目录时 dirvish 在临时 buffer 里计算隐藏规则，读到的是全局默认值，
  ;; 这里把侧边栏 buffer 的规则临时设成默认值，让子目录和顶层保持一致
  (advice-add 'dirvish-subtree--readin :around
              (lambda (orig &rest args)
                (let ((files dired-omit-files) (exts dired-omit-extensions))
                  (cl-letf (((default-value 'dired-omit-files) files)
                            ((default-value 'dired-omit-extensions) exts))
                    (apply orig args))))
              '((name . my/dirvish-side-subtree-omit)))

  ;; macOS 自带的 ls 不支持 --group-directories-first，侧边栏改用 Emacs 内置的
  ;; ls-lisp 生成列表，目录排在文件前面。dirvish 的 buffer 只存在于侧边栏，
  ;; 所以下面这几个函数都只在侧边栏里调用，普通 dired 仍然使用系统的 ls
  (require 'ls-lisp)
  (defun my/dirvish-side-with-ls-lisp (orig &rest args)
    (let ((ls-lisp-use-insert-directory-program nil)
          (ls-lisp-dirs-first t))
      (apply orig args)))
  (dolist (fn '(dirvish-dired-noselect-a ; 创建侧边栏的目录 buffer
                dirvish-subtree--readin  ; TAB 展开子目录
                dirvish-revert))         ; 刷新侧边栏（g）
    (advice-add fn :around #'my/dirvish-side-with-ls-lisp))

  ;; 和 neotree 一样，打开文件时选择最近使用过的普通窗口：跳过侧边栏窗口、
  ;; 专用（dedicated）窗口和 neotree 窗口。找不到时保持 dirvish 的默认行为
  (defun my/dirvish-side-select-main-window ()
    (when-let* ((wins (seq-remove
                       (lambda (w)
                         (or (window-parameter w 'window-side)
                             (window-dedicated-p w)
                             (with-current-buffer (window-buffer w)
                               (derived-mode-p 'neotree-mode))))
                       (window-list nil 'nomini))))
      (select-window (car (sort wins (lambda (a b)
                                       (> (window-use-time a)
                                          (window-use-time b)))))))))

;; 把侧边栏切换到指定目录：侧边栏已打开时直接切换（dirvish-side 加 C-u 只在侧边栏
;; 没显示时才会提示目录），没打开时用该目录打开。写在 use-package 外面，
;; 这样 dirvish 还没加载时也能用 M-x 调用
(defun my/dirvish-side-switch-dir (dir)
  "在 dirvish 侧边栏中打开目录 DIR，侧边栏没打开时先打开它。"
  (interactive
   ;; 没有全局开启 ivy-mode，这里临时用 ivy 补全目录，和 counsel-find-file 的体验一致
   (let ((completing-read-function #'ivy-completing-read))
     (require 'ivy)
     (list (read-directory-name "Directory: "))))
  (require 'dirvish-side)
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (if-let* ((win (dirvish-side--session-visible-p)))
        ;; 已经是这个目录时不再打开：对同一目录 find-alternate-file 会把侧边栏 buffer 关掉
        (unless (equal dir (expand-file-name
                            (buffer-local-value 'default-directory (window-buffer win))))
          (with-selected-window win
            (dirvish--find-entry 'find-alternate-file dir)))
      (dirvish-side dir))))

;; 类似 neotree-find：在侧边栏中定位当前 buffer 的文件。文件在侧边栏的目录树中时
;; 逐级展开到该文件；不在时（或侧边栏没打开时）把侧边栏切换到文件所在的目录
(defun my/find-in-dirvish-side ()
  "在 dirvish 侧边栏中定位当前 buffer 的文件，并选中侧边栏窗口。"
  (interactive)
  (require 'dirvish-side)
  (require 'dirvish-subtree)
  (let* ((file (and buffer-file-name (expand-file-name buffer-file-name)))
         (dir (expand-file-name (if file (file-name-directory file) default-directory)))
         (target (directory-file-name (or file dir)))
         (win (dirvish-side--session-visible-p))
         (root (and win (expand-file-name
                         (buffer-local-value 'default-directory (window-buffer win))))))
    (unless (and root (string-prefix-p root (file-name-as-directory target)))
      (my/dirvish-side-switch-dir dir)
      (setq win (dirvish-side--session-visible-p)
            root dir))
    (select-window win)
    ;; 从顶部开始展开，避免光标停在其他子目录里时找不到目标
    (goto-char (point-min))
    (unless (equal (file-name-as-directory target) root)
      (dirvish-subtree-expand-to target))))

(provide 'pkg-dired)
