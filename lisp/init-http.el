;;; init-http.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))
  :init
  (defun open-restclient()
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'init-http)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-http.el ends here
