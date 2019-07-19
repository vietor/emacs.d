;;; init-pasttimes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; RSS

(global-set-key (kbd "M-g s n") 'newsticker-show-news)
(setq-default newsticker-retrieval-interval 0
              newsticker-automatically-mark-items-as-old nil
              newsticker-url-list-defaults nil
              newsticker-url-list '(("xkcd" "https://www.xkcd.com/rss.xml" nil nil nil)
                                    ("slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil nil nil)))

(provide 'init-pasttimes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-pasttimes.el ends here
