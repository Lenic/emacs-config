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

(defun my/lsp-obj-get (obj key)
  "从 lsp-mode 返回的对象 OBJ 中取出 KEY 对应的值。
KEY 是不带冒号的字符串（比如 \"command\"），自动兼容 hash-table 和 plist 两种表示。"
  (if (hash-table-p obj)
      (gethash key obj)
    (plist-get obj (intern (concat ":" key)))))

(defvar my/lsp-ts-fixall-kind-map
  '(("unusedIdentifier" . "source.removeUnusedImports")
    ("import"            . "source.addMissingImports"))
  "tsActionId 到可靠 source.* code action kind 的映射表，方便以后继续扩展。")

(defun my/lsp-ts-fixall-handler (command-obj)
  "拦截 `_typescript.applyFixAllCodeAction`；根据 tsActionId 查
`my/lsp-ts-fixall-kind-map`，命中则改用对应的 source.* kind 处理；
查不到映射的情况仍照常转发给服务器。"
  (let* ((args (my/lsp-obj-get command-obj "arguments"))
         (ts-action-id (and args (> (length args) 0)
                            (my/lsp-obj-get (elt args 0) "tsActionId")))
         (kind (cdr (assoc ts-action-id my/lsp-ts-fixall-kind-map))))
    (if kind
        (lsp-execute-code-action-by-kind kind)
      (lsp--send-execute-command (my/lsp-obj-get command-obj "command")
                                 (my/lsp-obj-get command-obj "arguments")))))

(defun my/lsp-register-action-handler (server-id command handler)
  "为 SERVER-ID 对应的 lsp client 注册一个本地 action-handler。
COMMAND 是要拦截的命令名字符串（对应 code action 里 command.command 字段）。
HANDLER 是接收 command 对象（含 \"command\"/\"arguments\"）的处理函数。"
  (with-eval-after-load 'lsp-mode
    (if-let ((client (gethash server-id lsp-clients)))
        (puthash command handler (lsp--client-action-handlers client))
      (lsp-warn "未找到 server-id 为 %s 的 lsp client，注册失败" server-id))))

(with-eval-after-load 'lsp-javascript
  (my/lsp-register-action-handler 'ts-ls
                                  "_typescript.applyFixAllCodeAction"
                                  #'my/lsp-ts-fixall-handler))

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
        ;; lsp-clients-typescript-max-ts-server-memory 8192; 设置 TypeScript 可用的最大内存为 8G
        lsp-eldoc-enable-hover t                        ; 启用鼠标悬停文档
        lsp-disabled-clients '(eslint)                  ; 禁用 eslint 客户端
        lsp-signature-auto-activate t                   ; 自动显示函数签名
        lsp-headerline-breadcrumb-icons-enable nil      ; 禁用面包屑导航图标
        lsp-signature-render-documentation t            ; 渲染函数签名文档
        lsp-completion-show-detail t                    ; 显示补全的详细信息
        lsp-completion-show-kind t                      ; 显示补全项的类型
        lsp-diagnostics-provider :flycheck              ; 使用 flycheck 进行诊断
        lsp-enable-file-watchers t                      ; 启用文件监视
        lsp-enable-symbol-highlighting nil              ; 禁用符号高亮
        lsp-enable-dap-auto-configure nil               ; 禁用 DAP 自动配置
        lsp-flycheck-live-reporting nil                 ; 禁用 flycheck 实时报告
        lsp-headerline-breadcrumb-enable nil            ; 禁用面包屑导航
        lsp-completion-enable-additional-text-edit nil  ; 禁用额外的文本编辑
        lsp-idle-delay 0.500                            ; 增加空闲延迟，减少 CPU 使用
        lsp-log-io nil                                  ; 禁用日志记录，提高性能
        lsp-auto-guess-root nil                         ; 自动猜测项目根目录
        lsp-file-watch-threshold 2000)                  ; 限制监视的文件数量
  ;; 设置 lsp-mode-booster 加速
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
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
