;; 设置保存后自动格式化代码
(use-package prettier-js
  :defer 3
  :hook ((css-mode web-mode typescript-mode js-mode json-mode) . prettier-js-mode))

;; 快速编写 HTML 代码
(use-package emmet-mode
  :defer 3
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode))

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

;; 附加 Web 开发的各种插件
(defun web-dev-attached ()
  ;; 设置开启 flycheck 语法检查
  (my/use-eslint-from-node-modules)
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
            (eglot-ensure)
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)
            ;; 设置 Company 后端
            (add-to-list (make-local-variable 'company-backends)
                         '(company-css company-files company-capf company-dabbrev))
            ;; 其它开发设置
            (web-dev-attached)))

(use-package json-mode
  :defer 3
  :after eglot
  :mode "\\.json\\'"
  :config
  (add-to-list 'eglot-server-programs '(json-mode . ("vscode-json-languageserver" "--stdio")))
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 开启 LSP 模式自动完成
              (eglot-ensure)
              ;; 设置自动缩进的宽度
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)
              ;; 其它开发设置
              (web-dev-attached))))

;; 语法检查包
(use-package flycheck
  :defer 3
  :hook ((css-mode json-mode web-mode typescript-mode) . flycheck-mode))

(defun my/web-html-setup()
  "Setup for web-mode html files."
  (message "web-mode use html related setup")
  ;; 开启 LSP 模式自动完成
  (eglot-ensure)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends)
               '(company-files company-css company-capf company-dabbrev)))

(defun my/web-vue-setup()
  "Setup for vue related."
  (message "web-mode use vue related setup")
  ;; 开启 LSP 模式自动完成
  (eglot-ensure)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends) '(company-files company-css)))

(defun my/web-js-setup()
  "Setup for js related."
  (message "web-mode use js related setup")
  ;; 开启 LSP 模式自动完成
  (eglot-ensure)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends) '(company-files company-css company-capf company-dabbrev)))

(use-package web-mode
  :after eglot
  :init
  (setq web-mode-css-indent-offset 2                  ;; CSS 默认缩进 2 空格：包含 HTML 的 CSS 部分以及纯 CSS/LESS/SASS 文件等
        web-mode-code-indent-offset 2                 ;; JavaScript 默认缩进 2 空格：包含 HTML 的 SCRIPT 部分以及纯 JS/JSX/TS/TSX 文件等
        web-mode-markup-indent-offset 2               ;; HTML 默认缩进 2 空格：包含 HTML 文件以及 Vue 文件的 TEMPLATE 部分
        web-mode-enable-css-colorization t            ;; 开启 CSS 部分色值的展示：展示的时候会有光标显示位置异常
        web-mode-enable-current-column-highlight nil)
  ;; 构建一个专门为 Vue 文件开启的 web-mode 子模式
  (define-derived-mode my-vue-mode web-mode "Vue"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . my-vue-mode))
  (add-hook 'my-vue-mode-hook 'my/web-vue-setup)
  (add-to-list 'eglot-server-programs '(my-vue-mode "vls"))
  (add-to-list 'eglot-server-programs '(my-vue-mode . ("css-languageserver" "--stdio")))
  ;; 构建一个专门为 html 文件开启的 web-mode 子模式
  (define-derived-mode my-html-mode web-mode "Html"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (add-to-list 'auto-mode-alist '("\\.html\\'" . my-html-mode))
  (add-hook 'my-html-mode-hook 'my/web-html-setup)
  (add-to-list 'eglot-server-programs '(my-html-mode . ("html-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(my-html-mode . ("css-languageserver" "--stdio")))
  ;; 构建一个专门为 js/jsx 文件开启的 web-mode 子模式
  (define-derived-mode my-jsx-mode web-mode "JSX"
    "A major mode derived from web-mode, for editing .vue files with LSP support.")
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . my-jsx-mode))
  (add-hook 'my-jsx-mode-hook 'my/web-js-setup)
  (add-to-list 'eglot-server-programs '(my-jsx-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(my-jsx-mode . ("css-languageserver" "--stdio"))))

(use-package lsp-tailwindcss
  :defer 3
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-server-version "0.6.13"))

(use-package typescript-mode
  :defer 3
  :after eglot
  :mode "\\.ts[x]?\\'"
  :init
  ;; 设置缩进两个空格
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook '(lambda()
                                     (web-dev-attached)
                                     ;; 开启 LSP 模式自动完成
                                     (eglot-ensure)
                                     ;; 设置 Company 后端
                                     (add-to-list (make-local-variable 'company-backends)
                                                  '(company-files company-css)))))

;; 直接编辑 HTML 文件时的设置
(add-hook 'mhtml-mode-hook 'web-dev-attached)

(provide 'pkg-web)
