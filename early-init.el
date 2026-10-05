;; -*- lexical-binding: t -*-

;; macOS 从 15 直接跳到 26，旧版 libgccjit 仍按 "Darwin 主版本 - 9" 推算部署目标，
;; 会给 driver 传入非法的 -mmacosx-version-min=18.0，导致 native-comp 全线报
;; "error invoking gcc driver"。显式指定部署目标绕开。必须放在最前面，早于任何
;; 可能触发 JIT 编译的代码。
(when (eq system-type 'darwin)
  (setenv "MACOSX_DEPLOYMENT_TARGET" "26.0"))

;; 优化 Emacs 的启动速度：启动期间不做 GC，启动结束后恢复正常阈值。
;; 注意：GC 阈值只在这里设置这一份。init.el 里不要再挂第二个
;; after-init-hook，否则两者会互相覆盖，最终生效值取决于挂载顺序。
(defvar my/normal-gc-cons-threshold (* 32 1024 1024)
  "Normal garbage collection threshold after startup.")

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(defun my/restore-gc-threshold ()
  "Restore garbage collection threshold and percentage after startup."
  (setq gc-cons-threshold my/normal-gc-cons-threshold
        gc-cons-percentage 0.1))

(add-hook 'after-init-hook #'my/restore-gc-threshold)

;; 允许 JIT 编译，降低编译优先级避免启动时 CPU 飙升
(setq native-comp-jit-compilation t)

;; 异步原生编译从干净环境启动，看不到已加载的软依赖（如 neotree 对 all-the-icons、
;; projectile 的可选集成），会刷出一堆 "function is not known to be defined" 警告。
;; 编译结果本身是对的，所以只记录到 *Warnings*，不弹窗抢走布局。
;; 想重新看到弹窗改回 t，想彻底静音设为 nil。
(setq native-comp-async-report-warnings-errors 'silent)

;; 设置 LSP_MODE 使用 plist 进行反序列化
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

;; 操作系统相关变量
(defvar cabins--os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defvar cabins--os-mac (eq system-type 'darwin))

;; 隐藏菜单栏
(menu-bar-mode 0)

;; 开启 TCP 连接到 Server
(setq server-use-tcp t)

;; 直接打开软链接地址的文件，而不是打开原始文件的地址
(setq vc-follow-symlinks nil)

;; 设置自动加载已修改文件
(global-auto-revert-mode t)

;; 设置弹窗窗口出现纵向分隔的极限值：这个值能在 Mac 正常分辨率下仍然以上下的方式分隔弹出窗口
(setq split-width-threshold 1800)

;; 高亮当前行
(global-hl-line-mode t)

;; 编码设置
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Always load newest byte code
(setq load-prefer-newer t)

;; 设置 Mac 上的缺省按键映射
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; 设置缩进使用空格而非 Tab，同时设置 Tab 宽度是 4 个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; 设置自动备份
(setq auto-save-default t)

;; 设置 yes 和 no 的输入使用简写
(defalias 'yes-or-no-p 'y-or-n-p)

;; 备份设置
(setq backup-by-copying t ; 自动备份
      backup-directory-alist '(("." . "~/.em_backup")) ; 自动备份在目录"~/.em_backup"下
      delete-old-versions t ; 自动删除旧的备份文件
      kept-new-versions 3 ; 保留最近的3个备份文件
      kept-old-versions 1 ; 保留最早的1个备份文件
      version-control t) ; 多次备份

;; 显示光标所在列数
(column-number-mode 1)

;; 设置行号根据右侧对齐
(setq display-line-numbers-width-start t)

;; 隐藏工具栏
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Newline at end of file
(setq require-final-newline t)

;;关闭启动画面
(setq inhibit-startup-message t)

;; 隐藏滚动条
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; 设置选中时编辑直接删除选中值
(delete-selection-mode t)

;; 高亮匹配括号
(show-paren-mode t)

;; 设置光标样式
(setq-default cursor-type 'box)
