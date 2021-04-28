;; 设置全局快捷键打开默认的任务管理
(global-set-key (kbd "<f2>") (lambda () (interactive) (find-file "~/task/inbox.org")))

;; 设置快速捕获
(global-set-key (kbd "C-c c") 'org-capture)

;; 设置 Agenda 模块快捷键
(global-set-key (kbd "C-c a") 'org-agenda)

(use-package htmlize)
(use-package org-bullets
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
             '("i" "工作相关" entry (file+headline "~/task/inbox.org" "临时任务") "* TODO %?\n%U\n%a\n\n"))

;; 私人事务
;; 添加一个任务收集
(add-to-list 'org-capture-templates
             '("m" "个人生活" entry (file+headline "~/task/me.inbox.org" "Tasks") "* TODO %?\n%U\n%a"))

;; ;; 设置 Roam 模式
;; (use-package company-org-roam)
;; (use-package org-roam
;;   :after company-org-roam
;;   :hook
;;   (after-init . org-roam-mode)
;;   :custom
;;   (org-roam-directory "~/roam/")
;;   :bind (:map org-roam-mode-map
;;               (("C-c n l" . org-roam)
;;                ("C-c n f" . org-roam-find-file)
;;                ("C-c n g" . org-roam-graph))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert))
;;               (("C-c n I" . org-roam-insert-immediate))))
;; ;; 设置 Org-Roam 的 Server 模式
;; (use-package org-roam-server
;;   :ensure t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 10086
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

(provide 'pkg-org)
