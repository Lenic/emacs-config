;; -*- lexical-binding: t -*-

(use-package dired
  :commands dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  ;; 设置 Dired 模式显示方式：增加文件大小的可读性
  (dired-listing-switches "-alh")
  ;; macOS 自带的 ls 不支持 --group-directories-first，改用 Emacs 内置的 ls-lisp
  ;; 生成列表（不调用系统的 ls），按名称排序且目录排在文件前面。dired 和 dirvish 都生效
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-dirs-first t)
  :config
  (require 'ls-lisp))


;; 只在调用 dirvish-side 时才加载 dirvish。dirvish 只用于侧边栏；没有开启
;; dirvish-override-dired-mode，M-x dired 以及从侧边栏按 o 打开的目录仍然是普通的 dired
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
  ;; 显示目录的展开状态（+/-），使用 subtree-state 时 dirvish 会自动加载 dirvish-subtree
  (dirvish-side-attributes '(subtree-state))
  ;; 用 +/- 表示目录的折叠/展开状态
  (dirvish-subtree-state-style 'plus)
  ;; 从侧边栏打开文件时，不在 neotree 等特殊窗口里打开
  (dirvish-side-open-file-action #'my/dirvish-side-select-main-window)
  :config
  ;; dirvish 依赖 dirvish-override-dired-mode 的那组 advice，没有它 dirvish buffer 只是
  ;; 一个普通的 dired（所有详情都会显示，TAB 也不能展开子树）。但直接开启那个 mode
  ;; 会让所有 dired 都变成 dirvish，所以这里装同一组 advice，只在 dirvish buffer 里生效
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
                               (apply (if (dirvish-curr) fn orig) args)))
                  (:around (lambda (orig &rest args)
                             (if (dirvish-curr)
                                 (apply fn orig args)
                               (apply orig args))))
                  (:after (lambda (orig &rest args)
                            (prog1 (apply orig args)
                              (when (dirvish-curr) (apply fn args))))))
                `((name . ,(intern (format "my/dirvish-%s" fn))))))

  ;; dirvish buffer 也会登记到 dired-buffers 里，普通 dired 打开同一目录时会复用它，
  ;; 这里让普通 dired 查找 buffer 时跳过 dirvish 的 buffer（dirvish 自己新建 buffer 时
  ;; 会清空 dired-buffers，不经过这里）
  (advice-add 'dired-find-buffer-nocreate :around
              (lambda (orig &rest args)
                (let ((dired-buffers
                       (seq-remove (lambda (e)
                                     (and (buffer-live-p (cdr e))
                                          (with-current-buffer (cdr e) (dirvish-curr))))
                                   dired-buffers)))
                  (apply orig args)))
              '((name . my/dirvish-skip-dirvish-buffers)))

  ;; dirvish 用 -A 列出文件，不显示 . 和 ..（dirvish 在创建会话时读取 dired-listing-switches）
  (advice-add 'dirvish--new :around
              (lambda (orig &rest args)
                (let ((dired-listing-switches "-Alh"))
                  (apply orig args)))
              '((name . my/dirvish-listing-switches)))

  ;; 和 neotree 一样默认隐藏以 . 开头的文件，用 dired-x 的 C-x M-o（dired-omit-mode）切换。
  ;; 权限、大小、日期等详情由 dirvish-hide-details 隐藏
  (defun my/dirvish-setup-omit (&rest _)
    (setq-local dired-omit-files "\\`\\."
                ;; dired-omit-mode 默认还会隐藏 .elc 等扩展名，neotree 不隐藏，这里也不隐藏
                dired-omit-extensions nil
                dired-omit-verbose nil)
    (dired-omit-mode 1))
  (advice-add 'dirvish--setup-dired :after #'my/dirvish-setup-omit)

  ;; 展开子目录时 dirvish 在临时 buffer 里计算隐藏规则，读到的是全局默认值，
  ;; 这里把当前 buffer 的规则临时设成默认值，让子目录和顶层保持一致
  (advice-add 'dirvish-subtree--readin :around
              (lambda (orig &rest args)
                (let ((files dired-omit-files) (exts dired-omit-extensions))
                  (cl-letf (((default-value 'dired-omit-files) files)
                            ((default-value 'dired-omit-extensions) exts))
                    (apply orig args))))
              '((name . my/dirvish-subtree-omit)))

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
                                          (window-use-time b))))))))

  ;; 在侧边栏里用 o（或鼠标）在其他窗口打开目录时，默认会在新窗口里继续使用侧边栏会话。
  ;; 这里改成选中主窗口，在主窗口里用普通的 dired 打开该目录。主窗口不是 dirvish
  ;; buffer，上面那组 advice 不生效，所以打开的是原生 dired。dirvish 只用于侧边栏，
  ;; 这个 hook 只会在侧边栏里触发
  (defun my/dirvish-side-open-dir-in-main-window (entry find-fn)
    (when (and (eq find-fn 'find-file-other-window)
               (file-directory-p entry)
               (my/dirvish-side-select-main-window))
      (dired entry)
      t))
  (add-hook 'dirvish-find-entry-hook #'my/dirvish-side-open-dir-in-main-window)

  ;; dirvish 会把 dired 的表头行（"  /path/to/dir:" 以及 total 行）隐藏掉。这里改成
  ;; 不隐藏，而是用 overlay 显示成一行缩写后的路径，正好出现在 Project 下面。
  ;; 路径太长时只保留末尾部分，前面用 … 代替。
  ;; 目录在 C-u my/dirvish-side-switch-dir 打开的 worktree 里时，
  ;; 路径前面加上 worktree 当前的分支名或 commit 短 hash，和项目本身的目录区分开
  (defun my/dirvish-show-dir-header (&rest _)
    (dolist (ov (overlays-in (point-min) (1+ (point-min))))
      (when (and (overlay-get ov 'dired-header)
                 (eq (overlay-get ov 'invisible) t))
        (let* ((path (abbreviate-file-name default-directory))
               (commit (my/dirvish-side--worktree-commit default-directory))
               (tag (if commit (propertize (format "[%s] " commit) 'face 'warning) ""))
               (max (- (or (bound-and-true-p dirvish-side-width) 35) 2 (string-width tag))))
          (when (> (string-width path) max)
            (setq path (concat "…" (truncate-string-to-width
                                    path (string-width path)
                                    (- (string-width path) (1- max))))))
          (overlay-put ov 'invisible nil)
          (overlay-put ov 'display
                       (concat " " tag (propertize path 'face 'dired-directory) "\n"))))))
  (advice-add 'dirvish--hide-dired-header :after #'my/dirvish-show-dir-header)

  ;; ace-window 在窗口左上角放标记时会 move-to-window-line 0 再 recenter 0。point
  ;; 不能停在上面那行路径的 display 文本里，会落到第一个文件行，recenter 后侧边栏
  ;; 就往下滚了一行，路径被滚出窗口。这里放完标记后恢复 dirvish 窗口原来的起始位置
  (defun my/aw-keep-dirvish-window-start (orig path leaf)
    (let* ((win (cdr leaf))
           (ws (and (buffer-local-value 'dirvish--props (window-buffer win))
                    (window-start win))))
      (prog1 (funcall orig path leaf)
        (when ws (set-window-start win ws t)))))
  (with-eval-after-load 'ace-window
    (advice-add 'aw--lead-overlay :around #'my/aw-keep-dirvish-window-start))

  ;; avy-goto-line 收集候选行时，dired 表头行（现在显示为路径）会被当成一行候选，
  ;; 这里去掉它；同时如果表头被隐藏（上一行换行符 invisible 导致第一个文件行被跳过），
  ;; 把第一个文件行补回候选列表
  (defun my/avy-dirvish-add-first-line (orig &optional arg beg end bottom-up)
    ;; 先统一成从上到下的顺序，最后再按 bottom-up 反转
    (let* ((cands (funcall orig arg beg end nil))
           (wins (delete-dups (mapcar #'cdr cands))))
      (dolist (win wins)
        (with-current-buffer (window-buffer win)
          (when-let* ((pos (alist-get :content-begin
                                      (bound-and-true-p dirvish--props))))
            (unless (cdr dired-subdir-alist) ; 插入了子目录时表头本来就显示
              (setq cands (seq-remove (lambda (c)
                                        (and (eq (cdr c) win) (< (car c) pos)))
                                      cands)))
            (when (and (<= (or beg (window-start win)) pos)
                       (< pos (or end (window-end win t)))
                       (not (invisible-p pos))
                       (not (member (cons pos win) cands)))
              (push (cons pos win) cands)))))
      ;; 按窗口顺序、位置重新排序，bottom-up 时整体反转，和 avy 原本的顺序一致
      (setq cands (sort cands
                        (lambda (a b)
                          (let ((ia (seq-position wins (cdr a)))
                                (ib (seq-position wins (cdr b))))
                            (if (= ia ib) (< (car a) (car b)) (< ia ib))))))
      (if bottom-up (nreverse cands) cands)))
  (with-eval-after-load 'avy
    (advice-add 'avy--line-cands :around #'my/avy-dirvish-add-first-line)))

;; 在 DIR 下执行 git，返回 (退出码 . 输出)，输出里包含 stderr，失败时可以直接当错误信息显示
(defun my/dirvish-side--git (dir &rest args)
  (with-temp-buffer
    (let* ((default-directory dir)
           (code (apply #'process-file "git" nil t nil args)))
      (cons code (string-trim (buffer-string))))))

;; 返回 DIR 所在仓库的 git 目录。FLAG 为 "--git-common-dir" 时是公共目录（主工作区的 .git），
;; 同一项目的所有 worktree 都相同；为 "--git-dir" 时是 DIR 所在工作区自己的 git 目录
(defun my/dirvish-side--git-path (dir flag)
  (pcase-let ((`(,code . ,out) (my/dirvish-side--git dir "rev-parse" "--path-format=absolute" flag)))
    (and (zerop code) (file-truename (file-name-as-directory out)))))

;; worktree 在项目目录里面且没有被忽略时，把它加到 .git/info/exclude 里：只对本地生效，
;; 不用改项目的 .gitignore。否则 worktree 会出现在项目的未跟踪文件里。
;; 在 .config 下面时忽略整个 .config，其他位置只忽略 worktree 目录本身
(defun my/dirvish-side--ignore-worktree (main worktree)
  (let ((rel (file-relative-name (file-truename worktree) (file-truename main))))
    (unless (or (string-prefix-p "../" rel)
                (file-name-absolute-p rel)
                (zerop (car (my/dirvish-side--git main "check-ignore" "-q" rel))))
      (let ((exclude (expand-file-name
                      (cdr (my/dirvish-side--git main "rev-parse" "--git-path" "info/exclude"))
                      main)))
        (make-directory (file-name-directory exclude) t)
        (with-temp-buffer
          (when (file-exists-p exclude) (insert-file-contents exclude))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (if (string-prefix-p ".config/" rel) "/.config/" (concat "/" rel)) "\n")
          (write-region nil nil exclude nil 'silent))))))

;; 在 WORKTREE 创建项目的 worktree，代码停在项目当前的 commit 上。先 prune 一次，
;; 手动删过 worktree 目录时 git 里还留着登记信息，不清掉会创建失败
(defun my/dirvish-side--create-worktree (main worktree)
  (my/dirvish-side--git main "worktree" "prune")
  (pcase-let ((`(,code . ,out) (my/dirvish-side--git
                                main "worktree" "add" "--detach"
                                (directory-file-name (expand-file-name worktree)) "HEAD")))
    (unless (zerop code)
      (user-error "创建 worktree 失败：%s" out))))

;; 已有的 WORKTREE 必须是当前项目的 worktree。只是普通目录时 git 会向上找到项目本身，
;; checkout 就切换到项目目录上去了；是别的仓库时切换的也不是这个项目的代码；
;; 是项目主目录时会把正在开发的代码切换掉
(defun my/dirvish-side--check-worktree (main worktree)
  (pcase-let ((`(,code . ,wtop) (my/dirvish-side--git worktree "rev-parse" "--show-toplevel")))
    (unless (and (zerop code)
                 (equal (file-truename (file-name-as-directory wtop))
                        (file-truename worktree)))
      (user-error "%s 不受 git 管理" (abbreviate-file-name worktree))))
  (let ((common (my/dirvish-side--git-path worktree "--git-common-dir")))
    (unless (equal common (my/dirvish-side--git-path main "--git-common-dir"))
      (user-error "%s 不是当前项目的 worktree" (abbreviate-file-name worktree)))
    (when (equal common (my/dirvish-side--git-path worktree "--git-dir"))
      (user-error "%s 是项目的主目录，不能作为目标目录" (abbreviate-file-name worktree)))))

;; 项目有 node_modules 时，在 worktree 里建一个软链接指向它，worktree 里也能正常解析依赖
(defun my/dirvish-side--link-node-modules (main worktree)
  (let ((src (expand-file-name "node_modules" main))
        (dst (expand-file-name "node_modules" worktree)))
    (when (and (file-directory-p src)
               (not (file-exists-p dst))
               (not (file-symlink-p dst)))
      (make-symbolic-link src dst))))

;; 在 worktree 的 git 目录（主工作区的 .git/worktrees/<名字>/）里放一个标记文件，记录这是
;; C-u my/dirvish-side-switch-dir 使用的 worktree，重启 Emacs 后也能识别。删除 worktree 时
;; git 会连同这个目录一起删掉
(defconst my/dirvish-side--marker "dirvish-side")

(defun my/dirvish-side--mark-worktree (worktree)
  (when-let* ((gitdir (my/dirvish-side--git-path worktree "--git-dir")))
    (write-region "" nil (expand-file-name my/dirvish-side--marker gitdir) nil 'silent)))

;; DIR 在打过标记的 worktree 里时返回 worktree 根目录。worktree 根目录下的 .git 是一个文件，
;; 内容是 "gitdir: <git 目录>"，这里直接读文件，不调用 git，projectile 每次判断时开销很小
(defun my/dirvish-side--marked-worktree (dir)
  (when-let* ((root (locate-dominating-file dir ".git"))
              (dotgit (expand-file-name ".git" root))
              ((file-regular-p dotgit))
              (gitdir (with-temp-buffer
                        (insert-file-contents dotgit)
                        (and (re-search-forward "^gitdir: \\(.+\\)$" nil t) (match-string 1))))
              ((file-exists-p (expand-file-name my/dirvish-side--marker
                                                (expand-file-name gitdir root)))))
    (file-name-as-directory (expand-file-name root))))

;; projectile 不把这些 worktree 记到历史项目列表里
(with-eval-after-load 'projectile
  (setq projectile-ignored-project-function #'my/dirvish-side--marked-worktree))

;; 返回已经在 WORKTREE 以外的工作区检出本地分支 BRANCH 的那个工作区目录，没有时返回 nil
(defun my/dirvish-side--branch-worktree (worktree branch)
  (let ((self (file-truename worktree))
        (ref (concat "branch refs/heads/" branch))
        path found)
    (dolist (line (split-string (cdr (my/dirvish-side--git worktree "worktree" "list" "--porcelain"))
                                "\n"))
      (cond ((string-prefix-p "worktree " line)
             (setq path (file-name-as-directory (substring line (length "worktree ")))))
            ((and (equal line ref) (not found)
                  (not (equal (file-truename path) self)))
             (setq found path))))
    found))

;; DIR 在用 C-u 切换过的 worktree 里时，返回该 worktree 当前的分支名，
;; 不在分支上（detached）时返回 commit 的短 hash
(defun my/dirvish-side--worktree-commit (dir)
  (when-let* ((root (my/dirvish-side--marked-worktree dir)))
    (pcase-let ((`(,code . ,out) (my/dirvish-side--git root "symbolic-ref" "--short" "-q" "HEAD")))
      (if (zerop code)
          out
        (pcase-let ((`(,code . ,out) (my/dirvish-side--git root "rev-parse" "--short" "HEAD")))
          (and (zerop code) out))))))

;; 把 DIR 所在项目的 worktree 切换到 COMMIT，返回 worktree 里和 DIR 对应的目录（该 commit
;; 里没有这个目录时返回 worktree 根目录）。worktree 的位置是 PATH，默认 .config/worktree：
;; 相对路径（包括 ../）从项目根目录展开，也可以用 ~/ 或绝对路径。项目根目录取主工作区，
;; 这样 DIR 本身就在 worktree 里时也能再次切换
(defun my/dirvish-side--checkout-worktree (dir commit &optional path)
  (pcase-let ((`(,code . ,top) (my/dirvish-side--git dir "rev-parse" "--show-toplevel")))
    (unless (zerop code)
      (user-error "目录不受 git 管理：%s" (abbreviate-file-name dir)))
    (let* ((main (with-temp-buffer
                   (insert (cdr (my/dirvish-side--git dir "worktree" "list" "--porcelain")))
                   (goto-char (point-min))
                   (if (re-search-forward "^worktree \\(.+\\)$" nil t) (match-string 1) top)))
           (worktree (file-name-as-directory
                      (expand-file-name (or path ".config/worktree") main))))
      (when (file-exists-p worktree)
        (my/dirvish-side--check-worktree main worktree))
      (my/dirvish-side--ignore-worktree main worktree)
      (unless (file-exists-p worktree)
        (my/dirvish-side--create-worktree main worktree))
      (my/dirvish-side--link-node-modules main worktree)
      ;; 先解析成完整的 hash，找不到时给出明确的提示（直接 checkout 时 git 会把它当成路径报错）。
      ;; 输入的是本地分支时切换到分支本身；该分支已经在别的工作区检出时 git 不允许
      ;; 再检出一次，这时和输入 commit id 一样，用 --detach 停在分支最新的 commit 上
      (pcase-let* ((`(,code . ,hash) (my/dirvish-side--git
                                      worktree "rev-parse" "--verify" "--quiet"
                                      (concat commit "^{commit}")))
                   (branch-p (and (zerop code)
                                  (zerop (car (my/dirvish-side--git
                                               worktree "show-ref" "--verify" "--quiet"
                                               (concat "refs/heads/" commit))))))
                   (used-by (and branch-p (my/dirvish-side--branch-worktree worktree commit)))
                   (`(,code . ,out)
                    (cond ((not (zerop code)) (cons code (format "找不到 commit 或分支 %s" commit)))
                          ((and branch-p (not used-by))
                           (my/dirvish-side--git worktree "checkout" commit "--"))
                          (t (my/dirvish-side--git worktree "checkout" "--detach" hash)))))
        (unless (zerop code)
          (user-error "切换到 %s 失败：%s" commit out))
        (when used-by
          (message "分支 %s 已在 %s 检出，worktree 停在它最新的 commit 上"
                   commit (abbreviate-file-name used-by))))
      ;; 打上标记；之前已经被 projectile 记到历史列表里的，顺便删掉
      (my/dirvish-side--mark-worktree worktree)
      (when (bound-and-true-p projectile-known-projects)
        (let ((root (abbreviate-file-name worktree))
              (inhibit-message t))
          (when (member root projectile-known-projects)
            (projectile-remove-known-project root))))
      (let ((target (expand-file-name
                     (file-relative-name (file-truename dir)
                                         (file-truename (file-name-as-directory top)))
                     worktree)))
        (file-name-as-directory (if (file-directory-p target) target worktree))))))

;; 把侧边栏切换到指定目录：侧边栏已打开时直接切换（dirvish-side 加 C-u 只在侧边栏
;; 没显示时才会提示目录），没打开时用该目录打开。写在 use-package 外面，
;; 这样 dirvish 还没加载时也能用 M-x 调用。
;; 加 C-u 时先输入 commit id 和 worktree 的位置，把项目的 worktree（不存在时自动创建）
;; 切换到该 commit 后，在侧边栏里打开 worktree 中对应的目录
(defun my/dirvish-side-switch-dir (dir &optional commit worktree)
  "在 dirvish 侧边栏中打开目录 DIR，侧边栏没打开时先打开它。
COMMIT 非空（交互时加 C-u）时，把 DIR 所在项目的 worktree 切换到 COMMIT
（worktree 不存在时先创建），并打开 worktree 中和 DIR 对应的目录。
WORKTREE 是 worktree 的位置，默认 .config/worktree：相对路径（包括 ../）
从项目根目录展开，也可以用 ~/ 或绝对路径。"
  (interactive
   ;; 输入时允许嵌套 minibuffer：没有紧跟在 C-y 后面的 M-y 会再打开一个 minibuffer
   ;; 选择 kill-ring 里的内容，默认不允许嵌套，会报错
   (let* ((enable-recursive-minibuffers t)
          (commit (when current-prefix-arg
                    (let ((c (string-trim (read-string "Commit/Branch: "))))
                      (when (string-empty-p c) (user-error "没有输入 commit id 或分支名"))
                      c)))
          ;; 预先填好默认位置，直接回车确认；清空后回车也用默认位置
          (worktree (when commit
                      (let ((w (string-trim (read-string "Worktree: " ".config/worktree"))))
                        (unless (string-empty-p w) w))))
          ;; 没有全局开启 ivy-mode，这里临时用 ivy 补全目录，和 counsel-find-file 的体验一致
          (completing-read-function #'ivy-completing-read))
     (require 'ivy)
     (list (read-directory-name "Directory: ") commit worktree)))
  (require 'dirvish-side)
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (when commit
      (setq dir (my/dirvish-side--checkout-worktree dir commit worktree)))
    (if-let* ((win (dirvish-side--session-visible-p)))
        ;; 已经是这个目录时不再打开：对同一目录 find-alternate-file 会把侧边栏 buffer 关掉。
        ;; 切换了 commit 时文件可能变了，刷新一下
        (if (equal dir (expand-file-name
                        (buffer-local-value 'default-directory (window-buffer win))))
            (when commit
              (with-selected-window win (revert-buffer)))
          (with-selected-window win
            (dirvish--find-entry 'find-alternate-file dir)))
      (dirvish-side dir))))

;; 类似 neotree-find：在侧边栏中定位当前 buffer 的文件并选中侧边栏。
;; 复用 follow 模式的定位逻辑，但只在调用时执行一次
(defun my/find-in-dirvish-side ()
  "在 dirvish 侧边栏中定位当前 buffer 的文件，并选中侧边栏窗口。"
  (interactive)
  (require 'dirvish-side)
  (if (dirvish-side--session-visible-p)
      (dirvish-side--auto-jump)
    (dirvish-side))                     ; 打开时会自动展开到当前文件
  (select-window (dirvish-side--session-visible-p)))

(provide 'pkg-dired)
