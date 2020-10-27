;; Java 配置
(use-package lsp-java
  :after lsp-mode
  :config (add-hook 'java-mode-hook '(lambda()
                                       (lsp)
                                       (display-line-numbers-mode +1))))
;; Java 调试配置
(use-package dap-mode
  :after lsp-java
  :config (setq dap-auto-configure-features (remove 'controls dap-auto-configure-features)))
(use-package dap-java :ensure nil)

 ;; 语法检查包
;; (use-package flycheck
;;   :hook ((css-mode web-mode typescript-mode js-mode json-mode java-mode) . flycheck-mode)
;;   :config
;;   (defun my/use-eslint-from-node-modules ()
;;     (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
;;     (let* ((root (locate-dominating-file
;;                   (or (buffer-file-name) default-directory)
;;                   "node_modules"))
;;            (eslint
;;             (and root
;;                  (expand-file-name "node_modules/.bin/eslint"
;;                                    root))))
;;       (when (and eslint (file-executable-p eslint))
;;         (setq-local flycheck-javascript-eslint-executable eslint))))
;;   (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

;; (use-package meghanada
;;   :after flycheck
;;   :config
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               ;; meghanada-mode on
;;               (meghanada-mode t)
;;               (flycheck-mode +1)
;;               (setq c-basic-offset 2)
;;               ;; use code format
;;               (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;;               ;; 配置 Java 环境变量
;;               (cond
;;                ((eq system-type 'windows-nt)
;;                 (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
;;                 (setq meghanada-maven-path "mvn.cmd"))
;;                (t
;;                 (setq meghanada-java-path "java")
;;                 (setq meghanada-maven-path "mvn"))))))

(provide 'pkg-java)
