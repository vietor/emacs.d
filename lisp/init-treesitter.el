;;; init-treesitter.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (and (require 'treesit nil t)
           (fboundp 'treesit-available-p)
           (treesit-available-p))
  (add-to-list 'treesit-extra-load-path
               (concat user-emacs-space-directory "tree-sitter"))

  (let ((try-treesit-remap-lang '("js" "python" "yaml"))
        (lang-to-grammar-name '(("c++" . "cpp")
                                ("js" . "javascript"))))
    (dolist (lang try-treesit-remap-lang)
      (when (treesit-language-available-p
             (intern (or (cdr (assoc-string lang lang-to-grammar-name)) lang)))
        (let ((mode-name (intern (concat lang "-mode")))
              (ts-mode-name (intern (concat lang "-ts-mode"))))
          (when (fboundp ts-mode-name)
            (add-to-list 'major-mode-remap-alist
                         (cons mode-name ts-mode-name))))))))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
