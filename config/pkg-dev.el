;; 设置自动完成
(use-package company
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  :hook ((css-mode web-mode vue-mode typescript-mode js-mode rjsx-mode json-mode emacs-lisp-mode java-mode) . company-mode))

;; 设置自动完成时显示图标
;; (use-package company-box
;;   :after company
;;   :hook (company-mode . company-box-mode))

;; 指定符号高亮
(use-package symbol-overlay
  :bind
  (("C-c i" . symbol-overlay-put)
   ("C-c q" . symbol-overlay-remove-all)))

;; 自动补全括号
(use-package autopair
  :hook ((css-mode web-mode vue-mode typescript-mode js-mode rjsx-mode json-mode java-mode) . autopair-mode))

;; 语法检查包
(use-package flycheck
  :hook ((css-mode web-mode typescript-mode js-mode json-mode java-mode) . flycheck-mode)
  :config
  (defun my/use-eslint-from-node-modules ()
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint
            (and root
                 (expand-file-name "node_modules/.bin/eslint"
                                   root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

;; 加载 Web 开发配置
(require 'pkg-web)

;; 加载 Java 开发配置
(require 'pkg-java)

(provide 'pkg-dev)
