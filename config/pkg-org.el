;; 设置全局快捷键打开默认的任务管理
(global-set-key (kbd "<f2>") (lambda () (interactive) (find-file "~/task/inbox.org")))

;; 设置快速捕获
(global-set-key (kbd "C-c c") 'org-capture)

;; 设置 Agenda 模块快捷键
(global-set-key (kbd "C-c a") 'org-agenda)

(use-package htmlize
  :commands org-mode)
(use-package org-bullets
  :commands org-mode
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              ;; org 自动换行
              (setq truncate-lines nil)
              ;; 设置 Bullets
              (org-bullets-mode t)
              ;; 设置使用 indent 模式
              (org-indent-mode t)
              ;; 设置 TODO 任务的几个状态
              (setq org-todo-keywords
                    '((sequence "TODO(t)" "START(s!)" "PAUSE(p@/!)" "|" "DONE(d!)")
                      (sequence "BUG(b)" "|" "FIXED(f!)")
                      (sequence "|" "CANCELED(c@/!)"))))))

;; 设置缺省的日志记录位置
(setq org-default-notes-file "~/task/inbox.org")

;; 移除系统默认的 Capture 模版
(setq org-capture-templates nil)

;; 设置分组
;; (add-to-list 'org-capture-templates '("w" "工作专区"))
;; (add-to-list 'org-capture-templates '("m" "私人事务"))

;; 工作专区
;; 添加一个任务收集
(add-to-list 'org-capture-templates
             '("i" "工作相关" entry (file+headline "~/task/inbox.org" "Tasks") "* TODO %?\n%U\n%a\n\n"))

;; 私人事务
;; 添加一个任务收集
(add-to-list 'org-capture-templates
             '("m" "个人生活" entry (file+headline "~/task/me.inbox.org" "Tasks") "* TODO %?\n%U\n%a"))

(provide 'pkg-org)
