;; -*- lexical-binding: t -*-

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; 设置可以读取的最大容量为 3MB
(setq read-process-output-max (* 3 1024 1024))

;; 缓解在快速移动时大量代码的语法高亮
(setq redisplay-skip-fontification-on-input t)

;; 显示垃圾回收信息：只在排查 GC 卡顿时临时打开，
;; 常开会不停刷 echo area，把真正的提示冲掉
(setq garbage-collection-messages nil)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; 启动收尾时再开启的全局 mode：这几个库不是预加载的，
;; 挂到 after-init-hook 上，避免在读取配置期间加载
(add-hook 'after-init-hook #'global-auto-revert-mode) ; 自动加载已修改文件
(add-hook 'after-init-hook #'global-hl-line-mode)     ; 高亮当前行
(add-hook 'after-init-hook #'delete-selection-mode)   ; 选中时编辑直接删除选中值

(require 'use-package)
(setq use-package-always-ensure t
      use-package-minimum-reported-time 0.1) ; 超过 0.1 秒才报告加载时间

;; 禁用 cl 库的过时函数警告，其余编译警告保持开启。
;; 注意不能写成 '(cl-functions)——那是「只保留」这一类警告、关掉其它全部
(setq byte-compile-warnings '(not cl-functions))

;; Custom 自动写入的内容（包列表、受信任主题等）放到单独的 custom.el，
;; init.el 只保留手写配置。代码里的变量统一用 :custom / setopt 设置，
;; 不要再调用 custom-set-variables，否则会被 Custom 重复写进 custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(add-to-list 'load-path (concat user-emacs-directory "config"))

;; 加载基础配置
(require 'pkg-basic)

;; 加载基础全局配置
(require 'pkg-global)

;; 加载 Dired 模式配置
(require 'pkg-dired)

;; 加载 org-mode 配置
;; (require 'pkg-org)

;; 加载开发配置
(require 'pkg-dev)

;; 加载主题配置
(require 'pkg-theme)

;; 加载输入法配置
(require 'pkg-input)

;; 加载其它语言配置
(require 'pkg-lang)
