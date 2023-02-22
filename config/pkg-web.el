;; 设置保存后自动格式化代码
(use-package prettier-js
  :commands prettier-js-mode)

;; 快速编写 HTML 代码
(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-expand-jsx-className? t)
  :config
  (unbind-key "<C-return>" emmet-mode-keymap))

;; 附加 Web 开发的各种插件
(defun my/web-dev-attached ()
  ;; 设置使用 Tree Sitter 语法高亮
  ;; (tree-sitter-hl-mode t)
  ;; 在文件左侧显示 Git 状态
  (git-gutter-mode 1)
  ;; 设置 Prettier 格式化代码
  (when (or (file-exists-p (format "%s/.prettierrc" (projectile-project-root))) (file-exists-p (format "%s/.prettierignore" (projectile-project-root))))
    (prettier-js-mode 1))
  ;; 启动 Flycheck 语法检查
  (flycheck-mode 1)
  ;; 打开自动完成模式
  (yas-minor-mode 1)
  ;; 设置关闭自动换行
  (setq truncate-lines t)
  ;; 开启显示行号
  (display-line-numbers-mode +1)
  ;; 启动行号左侧对齐，并且不随着宽度变化而变化
  (setq display-line-numbers-width-start t)
  ;; 启动代码折叠功能
  (yafolding-mode 1)
  ;; 设置列参考线：120
  (setq display-fill-column-indicator-column 120)
  (display-fill-column-indicator-mode t))

;; 设置 CSS 及其它 CSS 预处理语言
(add-hook 'css-mode-hook
          (lambda ()
            ;; 通用前端开发设置
            (my/web-dev-attached)
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)))

(use-package json-mode
  :commands json-mode
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 其它开发设置
              (my/web-dev-attached)
              ;; 设置自动缩进的宽度
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

;; JavaScript/TypeScript 语法检查设置
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;; CSS/LESS 语法检查设置
(defun my/use-stylelint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (stylelint
          (and root
               (expand-file-name "node_modules/.bin/stylelint"
                                 root))))
    (when (and stylelint (file-executable-p stylelint))
      (setq-local flycheck-css-stylelint-executable stylelint)
      (setq-local flycheck-less-stylelint-executable stylelint))))

;; 语法检查包
(use-package flycheck
  :commands flycheck-mode
  :config
  ;; 设置 flycheck 只在文件打开和保存的时候检查语法
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook 'my/use-stylelint-from-node-modules)
  :hook ((css-mode web-mode js-mode typescript-mode) . flycheck-mode))

(defun my/web-html-setup()
  "Setup for html files."
  )

(defun my/web-vue-setup()
  "Setup for vue related."
  )

(defun my/web-js-setup()
  "Setup for js related."
  ;; 启动 Emmet 快速补充 HTML 代码
  (emmet-mode t)
  ;; 加载通用 Web 开发配置
  (my/web-dev-attached))

(use-package web-mode
  :commands web-mode
  :mode ("\\.vue\\'" "\\.html\\'")
  :init
  (setq web-mode-content-types-alist '(("vue" . "\\.vue\\'"))
        web-mode-css-indent-offset 2                  ;; CSS 默认缩进 2 空格：包含 HTML 的 CSS 部分以及纯 CSS/LESS/SASS 文件等
        web-mode-code-indent-offset 2                 ;; JavaScript 默认缩进 2 空格：包含 HTML 的 SCRIPT 部分以及纯 JS/JSX/TS/TSX 文件等
        web-mode-markup-indent-offset 2               ;; HTML 默认缩进 2 空格：包含 HTML 文件以及 Vue 文件的 TEMPLATE 部分
        web-mode-enable-css-colorization t            ;; 开启 CSS 部分色值的展示：展示的时候会有光标显示位置异常
        web-mode-enable-current-column-highlight nil)
  :config
  (add-hook 'web-mode-hook (lambda()
                             ;; 启动 Emmet 快速补充 HTML 代码
                             (emmet-mode t)
                             (my/web-dev-attached)
                             (flycheck-add-mode 'javascript-eslint 'web-mode)
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup))))))

;; JavaScript 和 JavaScript React 插件配置
(use-package js
  :commands js-mode
  :ensure nil
  :init
  (setq js-indent-level 2)
  :config
  (add-hook 'js-mode-hook 'my/web-js-setup))

;; TypeScript 和 TypeScript React 插件配置
(use-package typescript-mode
  :commands typescript-mode
  :mode "\\.ts[x]?\\'"
  :init
  ;; 设置缩进两个空格
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook 'my/web-js-setup))

;; 直接编辑 HTML 文件时的设置
(add-hook 'mhtml-mode-hook 'my/web-dev-attached)

;; 使用 ESLint 格式化当前 Buffer
(defun my/format-with-eslint ()
  "Call prettier on current file."
  (interactive)
  (setq my/current-file-format-command (format "cd %s ; npx eslint --fix %s"
                                               (projectile-project-root)
                                               (buffer-file-name)))
  (call-process-shell-command my/current-file-format-command)
  (message "format completed."))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

;;;###autoload
(defun eslint-fix ()
  "Format the current file with ESLint."
  (interactive)
  (unless buffer-file-name
    (error "ESLint requires a file-visiting buffer"))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save file %s? " buffer-file-name))
        (save-buffer)
      (error "ESLint may only be run on an unmodified buffer")))

  (setq my/current-file-format-command (format "cd %s ; npx eslint --fix %s"
                                               (projectile-project-root)
                                               (buffer-file-name)))
  (call-process-shell-command my/current-file-format-command))

;;;###autoload
(define-minor-mode eslint-fix-auto-mode
  "Run `eslint-fix' after save."
  :group 'eslint-fix
  (if eslint-fix-auto-mode
      (add-hook 'after-save-hook #'eslint-fix nil t)
    (remove-hook 'after-save-hook #'eslint-fix t)))

(provide 'pkg-web)
