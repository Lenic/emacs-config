;; Corfu 配置
(use-package corfu
  :defer 3  ; 延迟加载
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator))   ; 空格键插入分隔符
  :custom
  (corfu-preselect-first t)                 ; 预选第一个候选项
  (corfu-scroll-margin 5)                   ; 使用滚动边距
  :config
  (unless (display-graphic-p)
    (require 'corfu-terminal)
    (corfu-terminal-mode t))
  :init
  (global-corfu-mode))                      ; 全局启用 Corfu 模式

;; Emacs 内置补全相关设置
(use-package emacs
  :after corfu
  :custom
  (completion-cycle-threshold 1)            ; 如果只有一个时，按 TAB 时直接补全
  (tab-always-indent 'complete))            ; 使用 Tab 键进行补全

;; Orderless 配置
(use-package orderless
  :after corfu
  :custom
  (completion-styles '(orderless basic))                                    ; 设置补全样式
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))) ; 文件补全使用部分补全
  (orderless-component-separator #'orderless-escapable-split-on-space)      ; 使用空格分隔组件
  (orderless-matching-styles '(orderless-literal orderless-regexp)))        ; 设置匹配样式

;; LSP 模式配置
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none) ;; 我们使用 Corfu 进行补全
  :config
  (setq lsp-enable-snippet nil                          ; 禁用代码片段
        lsp-enable-folding nil                          ; 禁用基于 LSP 的代码折叠功能
        lsp-semantic-tokens-enable nil                  ; 禁用语义令牌功能
        lsp-typescript-format-enable nil                ; 禁用 TypeScript 代码格式化功能
        lsp-lens-enable nil                             ; 禁用代码镜头功能
        lsp-enable-on-type-formatting nil               ; 关闭类型格式化
        lsp-eldoc-render-all t                          ; 显示所有 eldoc 信息
        lsp-restart 'ignore                             ; 忽略 LSP 服务器重启提示
        lsp-clients-typescript-max-ts-server-memory 8192; 设置 TypeScript 可用的最大内存为 8G
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
        lsp-file-watch-threshold 2000)                  ; 限制监视的文件数量
  ;; 设置 LSP 补全使用 orderless
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook ((lsp-mode . eldoc-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion)))

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
