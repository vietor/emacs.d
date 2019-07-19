;;; init-pasttimes.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; RSS

(setq-default newsticker-retrieval-interval 0
              newsticker-automatically-mark-items-as-old nil
              newsticker-url-list-defaults nil
              newsticker-url-list '(("xkcd" "https://www.xkcd.com/rss.xml" nil nil nil)
                                    ("SMBC" "http://www.smbc-comics.com/rss.php" nil nil nil)
                                    ("slashdot" "http://rss.slashdot.org/Slashdot/slashdot" nil nil nil)))

(global-set-key (kbd "M-g s n")   'newsticker-show-news)

(provide 'init-pasttimes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-pasttimes.el ends here
