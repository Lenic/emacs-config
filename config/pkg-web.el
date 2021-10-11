;; 设置保存后自动格式化代码
(use-package prettier-js
  :defer 3
  :hook ((css-mode web-mode typescript-mode js-mode json-mode) . prettier-js-mode))

;; 快速编写 HTML 代码
(use-package emmet-mode
  :defer 3
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode))

;; 附加 Web 开发的各种插件
(defun web-dev-attached ()
  ;; 设置关闭自动换行
  (setq truncate-lines t)
  ;; 开启显示行号
  (display-line-numbers-mode +1)
  ;; 开启代码折叠子模式
  (origami-mode t)
  (hs-minor-mode t)
  ;; 设置列参考线：120
  (setq display-fill-column-indicator-column 120)
  (display-fill-column-indicator-mode t)
  ;; 开启代码折叠快捷键
  (define-key hs-minor-mode-map (kbd "C-c C-f") 'hs-toggle-hiding))

;; 设置 CSS 及其它 CSS 预处理语言
(add-hook 'css-mode-hook
          (lambda ()
            ;; 开启 LSP 模式自动完成
            (lsp)
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)
            ;; 设置 Company 后端
            (add-to-list (make-local-variable 'company-backends)
                         '(company-css company-files company-capf company-dabbrev))
            ;; 其它开发设置
            (web-dev-attached)))

(use-package json-mode
  :defer 3
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 开启 LSP 模式自动完成
              (lsp)
              ;; 设置自动缩进的宽度
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)
              ;; 其它开发设置
              (web-dev-attached))))

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
  :defer 3
  :config
  (add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook 'my/use-stylelint-from-node-modules)
  :hook ((css-mode json-mode web-mode typescript-mode) . flycheck-mode))

(defun my/web-html-setup()
  "Setup for web-mode html files."
  (message "web-mode use html related setup")
  ;; 开启 LSP 模式自动完成
  (lsp)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends)
               '(company-files company-css company-capf company-dabbrev)))

(defun my/web-vue-setup()
  "Setup for vue related."
  (message "web-mode use vue related setup")
  ;; 开启 LSP 模式自动完成
  (lsp)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends)
               '(company-files company-css)))

(defun my/web-js-setup()
  "Setup for js related."
  (message "web-mode use js related setup")
  ;; Tide 安装
  ;; (tide-setup)
  ;; 设置 Company 后端
  ;; (add-to-list (make-local-variable 'company-backends) '(company-capf))
  ;; 当 tsserver 服务没有启动时自动重新启动
  ;; (unless (tide-current-server) (tide-restart-server)))
  ;; 开启 LSP 模式自动完成
  (lsp)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends) '(company-files company-css company-capf company-dabbrev-code :separate)))

(use-package web-mode
  :defer 1
  :mode ("\\.tsx\\'" "\\.js[x]?\\'" "\\.vue\\'" "\\.html\\'")
  :init
  (setq web-mode-content-types-alist
        '(("vue" . "\\.vue\\'")
          ("jsx" . "\\.tsx\\'")
          ("jsx"  . "\\.js[x]?\\'")))
  (setq web-mode-css-indent-offset 2                  ;; CSS 默认缩进 2 空格：包含 HTML 的 CSS 部分以及纯 CSS/LESS/SASS 文件等
        web-mode-code-indent-offset 2                 ;; JavaScript 默认缩进 2 空格：包含 HTML 的 SCRIPT 部分以及纯 JS/JSX/TS/TSX 文件等
        web-mode-markup-indent-offset 2               ;; HTML 默认缩进 2 空格：包含 HTML 文件以及 Vue 文件的 TEMPLATE 部分
        web-mode-enable-css-colorization t            ;; 开启 CSS 部分色值的展示：展示的时候会有光标显示位置异常
        web-mode-enable-current-column-highlight nil)
  :config
  (add-hook 'web-mode-hook (lambda()
                             (web-dev-attached)
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup))
                                   ((member web-mode-content-type '("jsx"))
                                    (my/web-js-setup))))))

(use-package lsp-tailwindcss
  :defer 3
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package typescript-mode
  :defer 3
  :mode "\\.ts\\'"
  :init
  ;; 设置缩进两个空格
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook '(lambda()
                                     (web-dev-attached)
                                     ;; Tide 安装
                                     ;; (tide-setup)
                                     ;; 当 tsserver 服务没有启动时自动重新启动
                                     ;; (unless (tide-current-server) (tide-restart-server)))))
                                     ;; 开启 LSP 模式自动完成
                                     (lsp)
                                     ;; 设置 Company 后端
                                     (add-to-list (make-local-variable 'company-backends)
                                                  '(company-files company-css company-capf company-dabbrev-code :separate)))))

;; 直接编辑 HTML 文件时的设置
(add-hook 'mhtml-mode-hook 'web-dev-attached)

(provide 'pkg-web)
