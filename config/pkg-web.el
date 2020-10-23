(define-derived-mode web-vue-mode web-mode "WVue"
  "A major mode derived from web-mode, for editing .vue files with LSP support.")
(define-derived-mode web-react-mode web-mode "WReact"
  "A major mode derived from web-mode, for editing .jsx files with LSP support.")

;; 设置保存后自动格式化代码
(use-package prettier-js
  :hook ((css-mode web-mode typescript-mode js-mode json-mode) . prettier-js-mode))

;; LSP 模式的 JS 自动完成配置
(use-package tide
  :hook
  ((web-react-mode) .
   (lambda ()
     ;; Tide 安装
     (tide-setup)
     (tide-hl-identifier-mode t)
     ;; 当 tsserver 服务没有启动时自动重新启动
     (unless (tide-current-server) (tide-restart-server)))))

;; 快速编写 HTML 代码
(use-package emmet-mode
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode))

;; 附加 Web 开发的各种插件
(defun web-dev-attached ()
  ;; 设置关闭自动换行
  (setq truncate-lines t)
  ;; 开启显示行号
  (display-line-numbers-mode +1)
  ;; 开启代码折叠子模式
  (hs-minor-mode +1)
  ;; use eslint with web-mode for jsx files
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;; 开启代码折叠快捷键
  (global-set-key (kbd "s-/") 'hs-toggle-hiding))

;; 设置 CSS 及其它 CSS 预处理语言
(add-hook 'css-mode-hook
          (lambda ()
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)
            ;; 其它开发设置
            (web-dev-attached)))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 设置自动缩进的宽度
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)
              ;; 其它开发设置
              (web-dev-attached))))

(use-package eglot)
(use-package web-mode
  :after eglot
  :init
  ;; 设置默认的缩进
  (setq web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-vue-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-react-mode))
  ;; 设置语法高亮模式
  (add-hook 'web-react-mode-hook '(lambda ()
                                    (web-dev-attached)
                                    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))))
  (defclass eglot-vls (eglot-lsp-server) ()
    :documentation "Vue Language Server.")
  (add-hook 'web-vue-mode-hook #'eglot-ensure)
  (add-to-list 'eglot-server-programs
               '(web-vue-mode . (eglot-vls . ("vls" "--stdio"))))
  (cl-defmethod eglot-initialization-options ((server eglot-vls))
    "Passes through required vetur initialization options to VLS."
    '(:vetur
      (:completion
       (:autoImport t :useScaffoldSnippets t :tagCasing "kebab")
       :grammar
       (:customBlocks
        (:docs "md" :i18n "json"))
       :validation
       (:template t :style t :script t)
       :format
       (:options
        (:tabSize 2 :useTabs :json-false)
        :defaultFormatter
        (:html "prettyhtml" :css "prettier" :postcss "prettier" :scss "prettier" :less "prettier" :stylus "stylus-supremacy" :js "prettier" :ts "prettier")
        :defaultFormatterOptions
        (:js-beautify-html
         (:wrap_attributes "force-expand-multiline")
         :prettyhtml
         (:printWidth 100 :singleQuote :json-false :wrapAttributes :json-false :sortAttributes :json-false))
        :styleInitialIndent :json-false :scriptInitialIndent :json-false)
       :trace
       (:server "verbose")
       :dev
       (:vlsPath ""))
      ))
  )

(use-package typescript-mode
  :mode "\\.ts[x]?\\'"
  :init
  ;; 设置缩进两个空格
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook 'web-dev-attached))

(provide 'pkg-web)
