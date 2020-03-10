;; 设置 TODO 任务的几个状态
(setq org-todo-keywords
      '((sequence "TODO(t)" "START(s!)" "PAUSE(p@/!)" "|" "DONE(d!)")
	(sequence "BUG(b)" "|" "FIXED(f!)")
	(sequence "|" "CANCELED(c@/!)")))

;; org 自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; 设置使用 indent 模式
(add-hook 'org-mode-hook 'org-indent-mode)

;; 设置快速捕获
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/task/inbox.org")

;; 移除系统默认的 Capture 模版
(setq org-capture-templates nil)

;; 设置分组
(add-to-list 'org-capture-templates '("w" "工作专区"))
(add-to-list 'org-capture-templates '("m" "私人事务"))

;; 工作专区
;; 添加一个任务收集
(add-to-list 'org-capture-templates
             '("ww" "新建任务" entry (file+headline "~/task/mj.org" "临时任务") "* TODO %?\n%U\n%a\n\n"))
;; 添加一个灵感记录
(add-to-list 'org-capture-templates
             '("wi" "灵光闪现" entry (file+datetree "~/task/workspace.inspirations.org") "* %U\n%a\n%?"))
;; 添加一个备忘
(add-to-list 'org-capture-templates
             '("wm" "备忘录" entry (file+datetree "~/task/workspace.memo.org") "* %U\n%a\n%?"))

;; 私人事务
;; 添加一个任务收集
(add-to-list 'org-capture-templates
             '("mt" "新建任务" entry (file+headline "~/task/me.inbox.org" "Tasks") "* TODO %?\n%U\n%a"))
;; 添加一个灵感记录
(add-to-list 'org-capture-templates
             '("mi" "灵光乍现" entry (file+datetree "~/task/me.inspirations.org") "* %U\n%a\n%?"))
;; 添加一个备忘
(add-to-list 'org-capture-templates
             '("mm" "备忘录" entry (file+datetree "~/task/me.memo.org") "* %U\n%a\n%?"))
;; 添加个人日记本
(add-to-list 'org-capture-templates
             '("mj" "日记本" entry (file+datetree "~/task/me.journal.org") "* %U\n%?"))

;; 设置 Agenda 模块快捷键
(global-set-key "\C-ca" 'org-agenda)

(provide 'pkg-org)
