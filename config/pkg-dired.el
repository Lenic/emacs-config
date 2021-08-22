(use-package dired
  :defer 10
  :ensure nil
  :config
  ;; 在 macOS 上，ls 不支持 --dired 选项，而在 Linux 上则受支持
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  ;; 设置 Dired 模式显示方式：增加文件大小的可读性
  (setq dired-listing-switches "-alh")
  ;; 设置使用单个 Buffer 打开文件夹
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 设置回车直接在当前 Buffer 打开文件夹
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  ;; 设置 ^ 直接回到上一级文件夹
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

(provide 'pkg-dired)
