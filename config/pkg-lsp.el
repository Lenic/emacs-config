;; Corfu 配置
(use-package corfu
  :defer 3  ; 延迟加载
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator))   ; 空格键插入分隔符
  :custom
  (corfu-preselect-first t)                 ; 预选第一个候选项
  :init
  (global-corfu-mode))                      ; 全局启用 Corfu 模式

;; Emacs 内置补全相关设置
(use-package emacs
  :after corfu
  :custom
  (completion-cycle-threshold 1)            ; 如果只有一个时，按 TAB 时直接补全
  (tab-always-indent 'complete)             ; 使用 Tab 键进行补全
  (completion-styles '(basic substring partial-completion flex orderless))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

;; Orderless 配置
(use-package orderless
  :after corfu
  :custom
  (completion-styles '(orderless basic))                                    ; 设置补全样式
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))) ; 文件补全使用部分补全
  (orderless-component-separator #'orderless-escapable-split-on-space)      ; 使用空格分隔组件
  (orderless-matching-styles '(orderless-literal orderless-regexp)))        ; 设置匹配样式

;; Kind-icon 支持（可选）
;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default)                              ; 使用 corfu faces
;;   (kind-icon-use-icons nil)                                            ; 使用文本而不是图标
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)) ; 添加图标格式化器

;; Cape 支持（可选）
(use-package cape
  :after corfu
  :init
  ;; 添加补全后端
  (add-to-list 'completion-at-point-functions #'cape-file)     ; 文件补全
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)  ; 动态缩写补全
  (add-to-list 'completion-at-point-functions #'cape-keyword)) ; 关键词补全

;; LSP 模式配置
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none) ;; 我们使用 Corfu 进行补全
  :config
  (setq lsp-enable-snippet nil                          ; 禁用代码片段
        lsp-lens-enable nil                             ; 禁用代码镜头功能
        lsp-eldoc-render-all t                          ; 显示所有 eldoc 信息
        lsp-restart 'ignore                             ; 忽略 LSP 服务器重启提示
        lsp-eldoc-enable-hover t                        ; 启用鼠标悬停文档
        lsp-disabled-clients '(eslint)                  ; 禁用 eslint 客户端
        lsp-signature-auto-activate t                   ; 自动显示函数签名
        lsp-headerline-breadcrumb-icons-enable nil      ; 禁用面包屑导航图标
        lsp-signature-render-documentation t            ; 渲染函数签名文档
        lsp-completion-show-detail t                    ; 显示补全的详细信息
        lsp-completion-show-kind t                      ; 显示补全项的类型
        lsp-diagnostic-package :flycheck                ; 使用 flycheck 进行诊断
        lsp-enable-file-watchers t                      ; 启用文件监视
        lsp-enable-symbol-highlighting nil              ; 禁用符号高亮
        lsp-enable-dap-auto-configure nil               ; 禁用 DAP 自动配置
        lsp-flycheck-live-reporting nil                 ; 禁用 flycheck 实时报告
        lsp-headerline-breadcrumb-enable nil            ; 禁用面包屑导航
        lsp-completion-enable-additional-text-edit nil  ; 禁用额外的文本编辑
        lsp-idle-delay 0.500                            ; 增加空闲延迟，减少 CPU 使用
        lsp-log-io nil                                  ; 禁用日志记录，提高性能
        lsp-auto-guess-root t                           ; 自动猜测项目根目录
        lsp-file-watch-threshold 2000                   ; 限制监视的文件数量
        lsp-signature-function 'lsp-signature-posframe  ; 使用 posframe 显示签名
        lsp-signature-function 'lsp-lv-message)         ; 使用 minibuffer 显示函数签名

  ;; 优化大文件处理，忽略特定文件和目录
  (setq lsp-file-watch-ignored-files
        '("[/\\\\]\\.git$"          ; 忽略 .git 目录
          "[/\\\\]\\.hg$"           ; 忽略 .hg 目录
          "[/\\\\]\\.svn$"          ; 忽略 .svn 目录
          "[/\\\\]CVS$"             ; 忽略 CVS 目录
          "[/\\\\]target$"          ; 忽略 target 目录
          "[/\\\\]node_modules$"    ; 忽略 node_modules 目录
          "[/\\\\]\\.cache$"        ; 忽略 .cache 目录
          "[/\\\\]\\.elc$"))        ; 忽略 .elc 文件

  ;; 配置补全样式，使用 orderless 进行模糊匹配
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-overrides '((file (styles . (partial-completion)))))

  ;; 设置 LSP 补全使用 orderless
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  :hook ((lsp-mode . eldoc-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion)))  ; 设置 LSP 补全使用 orderless

;; 可选：使用 lsp-ui 增强 LSP 功能
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 3
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-delay 1
        lsp-ui-sideline-enable t))

(provide 'pkg-lsp)
