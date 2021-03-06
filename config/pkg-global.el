(use-package counsel
  :config
  ;; 设置 counsel-fzf 命令使用 rg 作为核心输出端
  (setq counsel-fzf-cmd "rg -l -L --glob '!.git' --hidden . | fzf -f \"%s\"")
  ;; 设置 counsel-rg 命令同样搜索快捷方式内部的内容
  (push "--follow" (cdr (nthcdr 0 counsel-rg-base-command)))
  ;; 设置输入两个字符后就开始执行匹配
  (custom-set-variables
   '(ivy-more-chars-alist '((counsel-grep . 2) (t . 2))))
  :bind
  ;; swiper 配置
  ("C-s" . swiper-isearch)
  ;; 替换命令执行
  ("M-x" . counsel-M-x)
  ;; 替换 Buffer 界面
  ("C-x C-b" . counsel-ibuffer)
  ;; 替换打开文件
  ("C-x C-f" . counsel-find-file)
  ;; 设置 RG 全文搜索
  ("C-c k" . counsel-rg)
  ;; 设置项目下的文件名查找
  ("C-c p" . counsel-fzf)
  ;; 设置查找特定目录下的文件名查找
  ("C-c f" . counsel-file-jump))

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/workspace/" "~/example/" "~/test/")
        projectile-require-project-root nil
        projectile-completion-system 'ivy
        projectile-switch-project-action 'neotree-projectile-action)
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "npm ci"
                                    :test "npm test"
                                    :run "npm run serve"
                                    :test-suffix ".spec"))

;; 在 swiper 中仍然可以输入中文，只不过换成了 M-i 这个快捷键
(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "M-i") 'pyim-convert-string-at-point))

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package amx)
(use-package wgrep)

(use-package multiple-cursors)

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  ;; 设置显示可视化撤销树形结构
  (global-undo-tree-mode))

;; 设置 Ace-Jump
(use-package ace-jump-mode
  :bind
  ("C-c j" . ace-jump-word-mode)
  ("C-c l" .  ace-jump-line-mode)
  :config
  (setq ace-jump-mode-scope 'window))

;; 显示行尾空白字符
(use-package whitespace
  :ensure nil
  :config
  (setq whitespace-style '(face trailing)
        whitespace-global-modes '(not markdown-mode))
  :hook (((web-mode typescript-mode emacs-lisp-mode) . whitespace-mode)
         (before-save . (lambda () (progn
                                     (untabify (point-min) (point-max))
                                     (whitespace-cleanup))))))

;; 处理特别长的行，避免带来一些性能问题
(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

;; 可以正常处理驼峰单词了：使用 M-f/b 时在每个驼峰单词之间停顿
(use-package subword
  :hook (after-init . global-subword-mode))

;; 开启全局窗口变动记录
(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode)
  :config (setq winner-dont-bind-my-keys nil))

;; 拷贝当前 Buffer 到剪切板
(defun copy-buffer-path ()
  (interactive)
  (if (equal buffer-file-name nil)
      (message "没有文件名")
    (setq path (vc-find-root buffer-file-name ".git"))
    (if (equal path nil)
        (kill-new (message buffer-file-name))
      (setq absolutePath (expand-file-name path))
      (setq targetPath (substring buffer-file-name (length absolutePath)))
      (kill-new (message targetPath)))))
(global-set-key (kbd "C-c C-p") 'copy-buffer-path)

(provide 'pkg-global)
