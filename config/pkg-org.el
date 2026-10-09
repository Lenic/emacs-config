;; -*- lexical-binding: t -*-

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
  (org-todo-keywords
   '((sequence "TODO(t)" "START(s!)" "PAUSE(p@/!)" "|" "DONE(d!)")
     (sequence "BUG(b)" "|" "FIXED(f!)")
     (sequence "|" "CANCELED(c@/!)")))
  :config
  ;; 移除系统默认并设置新的 Capture 模版
  (setq org-capture-templates
        '(("m" "个人生活" entry (file+headline "~/task/me.inbox.org" "Tasks") "* TODO %?\n%U\n%a")))
  :hook (org-mode . my/org-mode-setup))

(provide 'pkg-org)
