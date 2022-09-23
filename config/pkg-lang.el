(use-package markdown-mode
  :commands markdown-mode)

(use-package dockerfile-mode
  :commands dockerfile-mode)

(use-package yaml-mode
  :commands yaml-mode)

(use-package elfeed
  :commands elfeed
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '("https://sspai.com/feed"
          "https://feed.iplaysoft.com"
          "https://www.appinn.com/feed"
          "https://www.ifanr.com/feed")))

(provide 'pkg-lang)
