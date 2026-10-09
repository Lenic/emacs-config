;; -*- lexical-binding: t -*-

(use-package dired
  :commands dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  ;; 在 macOS 上，ls 不支持 --dired 选项，而在 Linux 上则受支持
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  ;; 设置 Dired 模式显示方式：增加文件大小的可读性
  (setq dired-listing-switches "-alh"))

(provide 'pkg-dired)
