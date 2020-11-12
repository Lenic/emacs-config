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
