;; -*- lexical-binding: t -*-

;;;; Node 项目工具查找

(defun my/node-bin (name)
  "从当前文件所在目录逐级向上查找 node_modules/.bin/NAME，返回其绝对路径。
找不到时返回 nil。逐级向上找，monorepo 里依赖被提升到根目录时也能找到。"
  (let* ((relative (concat "node_modules/.bin/" name))
         (root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                (lambda (dir)
                  (file-executable-p (expand-file-name relative dir))))))
    (and root (expand-file-name relative root))))

;;;; 格式化工具的项目配置检测

(defvar my/prettier-config-files
  '(".prettierrc" ".prettierrc.json" ".prettierrc.json5"
    ".prettierrc.yaml" ".prettierrc.yml" ".prettierrc.toml"
    ".prettierrc.js" ".prettierrc.cjs" ".prettierrc.mjs"
    ".prettierrc.ts" ".prettierrc.cts" ".prettierrc.mts"
    "prettier.config.js" "prettier.config.cjs" "prettier.config.mjs"
    "prettier.config.ts" "prettier.config.cts" "prettier.config.mts")
  "Prettier 支持的配置文件名。")

(defvar my/eslint-config-files
  '("eslint.config.js" "eslint.config.cjs" "eslint.config.mjs"
    "eslint.config.ts" "eslint.config.cts" "eslint.config.mts"
    ".eslintrc" ".eslintrc.js" ".eslintrc.cjs"
    ".eslintrc.json" ".eslintrc.yaml" ".eslintrc.yml")
  "ESLint 支持的配置文件名，包括新版 flat config 和旧版 eslintrc。")

(defun my/package-json-has-key-p (dir key)
  "DIR 下的 package.json 中包含顶层字段 KEY（符号）时返回非 nil。"
  (let ((file (expand-file-name "package.json" dir)))
    (and (file-readable-p file)
         (ignore-errors
           (with-temp-buffer
             (insert-file-contents file)
             (assq key (json-parse-buffer :object-type 'alist)))))))

(defun my/project-config-p (config-files package-json-key)
  "从当前文件向上能找到工具配置时返回非 nil。
配置可以是 CONFIG-FILES 中的任一文件，也可以是 package.json 里的
PACKAGE-JSON-KEY 字段。"
  (locate-dominating-file
   (or (buffer-file-name) default-directory)
   (lambda (dir)
     (or (seq-some (lambda (file) (file-exists-p (expand-file-name file dir)))
                   config-files)
         (my/package-json-has-key-p dir package-json-key)))))

;;;; Prettier / ESLint 保存前格式化的开关
;;
;; `prettier-js-mode' 和 `eslintd-fix-on-save-mode' 只作为开关使用：
;; 两个 mode 开启时会各自往 `before-save-hook' 挂自己的格式化函数，这里把它去掉，
;; 改为挂 `my/run-save-formatters'，由它按固定顺序统一执行。
;; 项目里有配置时自动开启，也可以随时 M-x 手动开启或关闭。

(defun my/prettier-js-mode-setup ()
  "让 `prettier-js-mode' 只作为开关，格式化交给 `my/run-save-formatters'。"
  (remove-hook 'before-save-hook #'prettier-js-prettify t)
  (when prettier-js-mode
    ;; 优先使用项目自己安装的 prettier，找不到时使用全局的
    (when-let* ((prettier (my/node-bin "prettier")))
      (setq-local prettier-js-command prettier))
    (my/enable-save-formatters)))
(add-hook 'prettier-js-mode-hook #'my/prettier-js-mode-setup)

(defun my/eslintd-fix-on-save-mode-setup ()
  "让 `eslintd-fix-on-save-mode' 只作为开关，格式化交给 `my/run-save-formatters'。"
  (remove-hook 'before-save-hook #'eslintd-fix-buffer t)
  (when eslintd-fix-on-save-mode
    (my/enable-save-formatters)))
(add-hook 'eslintd-fix-on-save-mode-hook #'my/eslintd-fix-on-save-mode-setup)

(use-package prettier-js
  :commands (prettier-js-mode prettier-js-prettify))

;; 快速编写 HTML 代码
(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-indent-after-insert nil)
  :config
  ;; emmet-jsx-major-modes 里的模式扩展 class 时会展开成 className，
  ;; 默认列表不包含 tree-sitter 模式，这里补充 js-ts-mode/tsx-ts-mode，
  ;; 让 web-mode（Vue 等）继续保持默认的 class 展开
  (add-to-list 'emmet-jsx-major-modes 'js-ts-mode)
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode)
  (unbind-key "<C-return>" emmet-mode-keymap))

;; 附加 Web 开发的各种插件
(defun my/web-dev-attached ()
  "Web 开发通用的 buffer 局部设置。"
  ;; 仅对当前 buffer 开启自动补全括号功能
  (electric-pair-local-mode 1)
  ;; 在文件左侧显示 Git 状态
  (git-gutter-mode 1)
  ;; 保存前格式化：项目里有配置时自动开启，执行顺序由 `my/save-formatters' 决定
  ;; 1. ESLint 修复只对 JS/TS/Vue 自动开启——本函数同时服务于 json/css/mhtml，
  ;;    对这些文件跑 eslint --fix 没有意义
  (when (and (derived-mode-p 'js-ts-mode 'tsx-ts-mode 'web-mode)
             (my/project-config-p my/eslint-config-files 'eslintConfig))
    (eslintd-fix-on-save-mode 1))
  ;; 2. Prettier 排版
  (when (my/project-config-p my/prettier-config-files 'prettier)
    (prettier-js-mode 1))
  ;; 启动 Flycheck 语法检查
  (flycheck-mode 1)
  ;; 设置本地的 Tab 宽度
  (setq-local tab-width 2)
  ;; 打开自动完成模式
  (yas-minor-mode 1)
  ;; 设置关闭自动换行
  (setq-local truncate-lines t)
  ;; 开启显示行号（左侧对齐由 early-init.el 的
  ;; `display-line-numbers-width-start' 全局设定，此处无需重复）
  (display-line-numbers-mode +1)
  ;; 启动代码折叠功能
  (yafolding-mode 1)
  ;; 设置列参考线：120
  (setq-local display-fill-column-indicator-column 120)
  (display-fill-column-indicator-mode t))

;; 设置 CSS 及其它 CSS 预处理语言
(defun my/web-enable-lsp-snippet ()
  "在当前 buffer 局部开启 LSP 的 snippet 支持。
vscode 的 CSS/JSON 语言服务器在客户端不支持 snippet 时会整个关闭补全，
而全局的 `lsp-enable-snippet' 是 nil。服务器初始化时读取的是启动它的 buffer
里的值，所以必须在 `lsp-deferred' 之前调用，且只影响 CSS/JSON 服务器。"
  (setq-local lsp-enable-snippet t))

(defun my/web-css-setup ()
  "CSS/LESS/SCSS 的 buffer 局部设置。"
  ;; 通用前端开发设置
  (my/web-dev-attached)
  ;; 开启 LSP 模式自动完成
  (my/web-enable-lsp-snippet)
  (lsp-deferred)
  ;; 设置自动缩进的宽度
  (setq-local css-indent-offset 2))
;; css-mode-hook 同时覆盖 css-mode 及其派生的 less-css-mode、scss-mode；
;; css-ts-mode 派生自 css-base-mode，需要单独挂载
(add-hook 'css-mode-hook #'my/web-css-setup)
(add-hook 'css-ts-mode-hook #'my/web-css-setup)

;; 按项目里安装的工具选择 flycheck checker
(defvar my/eslint-lsp-chained nil
  "是否已经把 lsp checker 链接到 javascript-eslint 之后。")

(defun my/web-select-flycheck-checker (&rest _)
  "LSP 接管 buffer 后，按项目里安装的工具选择 flycheck checker。
LESS/CSS 项目装了 stylelint 时使用 less-stylelint；
JS/TS/Vue 项目装了 eslint 时先跑 javascript-eslint，再接着跑 lsp。"
  (when flycheck-mode
    (cond
     ((derived-mode-p 'less-css-mode)
      (when (my/node-bin "stylelint")
        (flycheck-select-checker 'less-stylelint)))
     ;; css-ts-mode 也用 less-stylelint，原因见 flycheck 的 :config
     ((derived-mode-p 'css-ts-mode)
      (when (my/node-bin "stylelint")
        (flycheck-select-checker 'less-stylelint)))
     ((derived-mode-p 'js-ts-mode 'tsx-ts-mode 'web-mode)
      (when (and (my/node-bin "eslint")
                 (flycheck-valid-checker-p 'lsp)
                 (flycheck-valid-checker-p 'javascript-eslint))
        ;; next-checker 是 checker 的全局属性，不是 buffer 局部的，设置一次即可。
        ;; lsp checker 要等 lsp 第一次接管 buffer 时才定义，所以不能放到 flycheck 的 :config 里
        (unless my/eslint-lsp-chained
          (flycheck-add-next-checker 'javascript-eslint 'lsp)
          (setq my/eslint-lsp-chained t))
        (flycheck-select-checker 'javascript-eslint))))))
;; 必须在 `lsp-diagnostics-flycheck-enable' 之后执行：它会把 checker 强制设成 lsp，
;; 而它和 lsp-managed-mode-hook 的先后顺序不固定，挂在 hook 上时选好的 checker 可能又被改回去
(advice-add 'lsp-diagnostics-flycheck-enable :after #'my/web-select-flycheck-checker)

(defun my/web-json-setup ()
  "JSON 的 buffer 局部设置。"
  ;; 其它开发设置
  (my/web-dev-attached)
  ;; 开启 LSP 模式自动完成
  (my/web-enable-lsp-snippet)
  (lsp-deferred))

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :hook (json-ts-mode . my/web-json-setup))

;; CSS/LESS 语法检查设置：优先使用项目自己安装的 stylelint
(defun my/use-stylelint-from-node-modules ()
  (when-let* ((stylelint (my/node-bin "stylelint")))
    (setq-local flycheck-css-stylelint-executable stylelint)
    (setq-local flycheck-less-stylelint-executable stylelint)))

;; 语法检查包
(use-package flycheck
  :commands flycheck-mode
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d")
  ;; 设置 flycheck 只在文件打开和保存的时候检查语法
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-idle-change-delay 1)
  (setq flycheck-checker-error-threshold 10000)
  ;; Vue 文件同样使用 ESLint 检查
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; css-ts-mode 改用 less-stylelint 检查：css-stylelint 会把文件名（--stdin-filename）
  ;; 传给 stylelint，项目配置里按文件名匹配的 overrides 会生效，有的项目把 *.css
  ;; 交给 postcss-html 解析，结果纯 CSS 一条都检查不到；less-stylelint 不传文件名，
  ;; 按普通 CSS 解析，和 less-css-mode 的检查效果一致
  (flycheck-add-mode 'less-stylelint 'css-ts-mode)
  (add-hook 'flycheck-mode-hook 'my/use-stylelint-from-node-modules)
  :hook ((css-mode web-mode js-ts-mode tsx-ts-mode) . flycheck-mode))

(defun my/web-js-setup ()
  "JS/TS/Vue/HTML 的 buffer 局部设置。"
  ;; 启动 Emmet 快速补充 HTML 代码
  (emmet-mode t)
  ;; 加载通用 Web 开发配置
  (my/web-dev-attached)
  ;; 开启 LSP 模式自动完成
  (lsp-deferred))

(use-package web-mode
  :mode ("\\.vue\\'" "\\.html\\'")
  :config
  (setq web-mode-content-types-alist '(("vue" . "\\.vue\\'"))
        web-mode-css-indent-offset 2                  ;; CSS 默认缩进 2 空格：包含 HTML 的 CSS 部分以及纯 CSS/LESS/SASS 文件等
        web-mode-code-indent-offset 2                 ;; JavaScript 默认缩进 2 空格：包含 HTML 的 SCRIPT 部分以及纯 JS/JSX/TS/TSX 文件等
        web-mode-markup-indent-offset 2               ;; HTML 默认缩进 2 空格：包含 HTML 文件以及 Vue 文件的 TEMPLATE 部分
        web-mode-enable-css-colorization t            ;; 开启 CSS 部分色值的展示：展示的时候会有光标显示位置异常
        web-mode-enable-auto-indentation nil          ;; 禁止粘贴时格式化代码
        web-mode-enable-current-column-highlight nil)
  :hook (web-mode . my/web-js-setup))

;; TailwindCSS 插件配置
(use-package lsp-tailwindcss
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-add-on-mode t
        lsp-tailwindcss-server-version "0.14.24")
  :config
  ;; 是否启用由插件自己判断：有 tailwind.config.*，或依赖里的 tailwindcss 是 v4 及以上。
  ;; v3 monorepo 的配置文件埋得较深时，可设置 `lsp-tailwindcss-experimental-config-file'
  ;; 第一次加载完成后尝试拉起 TailwindCSS 服务
  (lsp-deferred))

;; JavaScript 和 JavaScript React 插件配置
(add-to-list 'auto-mode-alist '("\\.\\(js\\|jsx\\)\\'" . js-ts-mode))
(add-hook 'js-ts-mode-hook 'my/web-js-setup)
;; TypeScript 和 TypeScript React 插件配置
(add-to-list 'auto-mode-alist '("\\.\\(ts\\|tsx\\)\\'" . tsx-ts-mode))
(add-hook 'tsx-ts-mode-hook 'my/web-js-setup)

;; ESLint 自动修复：需要预先在全局安装 eslint_d 包。
;; 定义出 `eslintd-fix-on-save-mode' 开关（mode-line 显示 ESLint）和
;; `eslintd-fix-buffer'，后者由 `my/run-save-formatters' 在保存前调用
(use-package reformatter
  :config
  (reformatter-define eslintd-fix
    :lighter " ESLint"
    :program (executable-find "eslint_d")
    :args (list "--fix-to-stdout" "--stdin" "--stdin-filename" (buffer-file-name))
    :input-file (reformatter-temp-file-in-current-directory "js")
    :exit-code-success-p (lambda (code) (or (eq code 1) (eq code 0))))
  ;; stylelint 自动修复：定义出 `stylelint-fix-buffer' 和 `stylelint-fix-region'。
  ;; 和 flycheck 检查时一样不传文件名（原因见 flycheck 的 :config），优先用项目本地的 stylelint。
  ;; 还有无法自动修复的问题时退出码是 2，但标准输出里仍然是修复后的代码，所以也算成功
  (reformatter-define stylelint-fix
    :program (or (my/node-bin "stylelint") "stylelint")
    :args '("--fix" "--stdin")
    :exit-code-success-p (lambda (code) (memq code '(0 2)))
    :mode nil))

(defun my/stylelint-fix ()
  "用 stylelint 自动修复当前 buffer，然后重新检查，剩下的就是需要手动处理的问题。"
  (interactive)
  (stylelint-fix-buffer t)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

;; 直接编辑 HTML 文件时的设置
(add-hook 'mhtml-mode-hook 'my/web-dev-attached)

(provide 'pkg-web)
