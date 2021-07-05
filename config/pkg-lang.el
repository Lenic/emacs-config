(use-package markdown-mode)

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package elfeed
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '("https://sspai.com/feed"
          "https://www.ifanr.com/feed")))

(provide 'pkg-lang)
