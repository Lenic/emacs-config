(use-package markdown-mode
  :defer 3)

(use-package dockerfile-mode
  :defer 3)

(use-package yaml-mode
  :defer 3)

(use-package elfeed
  :defer 3
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '("https://sspai.com/feed"
          "https://feed.iplaysoft.com"
          "https://www.appinn.com/feed"
          "https://www.ifanr.com/feed")))

(provide 'pkg-lang)
