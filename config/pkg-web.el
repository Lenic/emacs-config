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
  ;; 打开自动补全括号功能
  (electric-pair-mode 1)
  ;; 在文件左侧显示 Git 状态
  (git-gutter-mode 1)
  ;; 设置 Prettier 格式化代码
  (when (or (file-exists-p (format "%s/.prettierrc" (projectile-project-root)))
            (file-exists-p (format "%s/.prettierrc.js" (projectile-project-root)))
            (file-exists-p (format "%s/.prettierignore" (projectile-project-root))))
    (prettier-js-mode 1))
  ;; 设置本地的 Tab 宽度
  (setq-local tab-width 2)
  ;; 打开自动完成模式
  (yas-minor-mode 1)
  ;; 开启自动 ESLint 修复
  (eslintd-fix-mode)
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
            ;; 开启 LSP 模式自动完成
            (eglot-ensure)
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)))

(use-package json-ts-mode
  :commands json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :config
  ;; 加载 LSP 配置
  (require 'lsp-mode)
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 其它开发设置
              (my/web-dev-attached)
              ;; 开启 LSP 模式自动完成
              (eglot-ensure))))

(defun my/web-html-setup()
  "Setup for html files."
  ;; 开启 LSP 模式自动完成
  (eglot-ensure))

(defun my/web-vue-setup()
  "Setup for vue related."
  ;; 开启 LSP 模式自动完成
  (eglot-ensure))

(defun my/web-js-setup()
  "Setup for js related."
  ;; 启动 Emmet 快速补充 HTML 代码
  (emmet-mode t)
  ;; 加载通用 Web 开发配置
  (my/web-dev-attached)
  ;; 开启 LSP 模式自动完成
  (eglot-ensure))

(use-package web-mode
  :defer 5
  :after eglot
  :init
  ;; web-mode setup
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  ;; Volar
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio")))
  ;; 编辑配置
  (setq web-mode-content-types-alist '(("vue" . "\\.vue\\'"))
        web-mode-css-indent-offset 2                  ;; CSS 默认缩进 2 空格：包含 HTML 的 CSS 部分以及纯 CSS/LESS/SASS 文件等
        web-mode-code-indent-offset 2                 ;; JavaScript 默认缩进 2 空格：包含 HTML 的 SCRIPT 部分以及纯 JS/JSX/TS/TSX 文件等
        web-mode-markup-indent-offset 2               ;; HTML 默认缩进 2 空格：包含 HTML 文件以及 Vue 文件的 TEMPLATE 部分
        web-mode-enable-css-colorization t            ;; 开启 CSS 部分色值的展示：展示的时候会有光标显示位置异常
        web-mode-enable-auto-indentation nil          ;; 禁止粘贴时格式化代码
        web-mode-enable-current-column-highlight nil)
  :config
  (add-hook 'web-mode-hook (lambda()
                             ;; 启动 Emmet 快速补充 HTML 代码
                             (emmet-mode t)
                             (my/web-dev-attached)
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup))))))

;; JavaScript 和 JavaScript React 插件配置
(add-to-list 'auto-mode-alist '("\\.\\(js\\|jsx\\)\\'" . js-ts-mode))
(add-hook 'js-ts-mode-hook 'my/web-js-setup)
;; TypeScript 和 TypeScript React 插件配置
(add-hook 'tsx-ts-mode-hook 'my/web-js-setup)
;; JSON 插件配置
(add-hook 'json-ts-mode-hook 'my/web-dev-attached)

;; (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))

;; 开启 ESLint 的自动修复模式：需要预先在全局安装 eslint_d 包
(use-package eslintd-fix
  :commands eslintd-fix-mode)

;; 直接编辑 HTML 文件时的设置
(add-hook 'mhtml-mode-hook 'my/web-dev-attached)

(provide 'pkg-web)
