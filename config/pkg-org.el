;; -*- lexical-binding: t -*-

(defun my/open-inbox-org ()
  "Open the default inbox.org file."
  (interactive)
  (find-file "~/task/inbox.org"))

;; 设置全局快捷键打开默认的任务管理
(global-set-key (kbd "<f2>") #'my/open-inbox-org)

;; 设置快速捕获
(global-set-key (kbd "C-c c") #'org-capture)

;; 设置 Agenda 模块快捷键
(global-set-key (kbd "C-c a") #'org-agenda)

(use-package htmlize
  :defer t)

(defun my/org-mode-setup ()
  "Setup buffer-local configurations for Org mode."
  ;; org 自动换行
  (setq truncate-lines nil)
  ;; 设置使用 indent 模式
  (org-indent-mode t))

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-default-notes-file "~/task/inbox.org")
  (org-todo-keywords
   '((sequence "TODO(t)" "START(s!)" "PAUSE(p@/!)" "|" "DONE(d!)")
     (sequence "BUG(b)" "|" "FIXED(f!)")
     (sequence "|" "CANCELED(c@/!)")))
  :config
  ;; 移除系统默认并设置新的 Capture 模版
  (setq org-capture-templates
        '(("i" "工作相关" entry (file+headline "~/task/inbox.org" "Tasks") "* TODO %?\n%U\n%a\n\n")
          ("m" "个人生活" entry (file+headline "~/task/me.inbox.org" "Tasks") "* TODO %?\n%U\n%a")))
  :hook (org-mode . my/org-mode-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(provide 'pkg-org)
