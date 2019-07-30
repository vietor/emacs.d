;;; init-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when window-system
  (use-package 'doom-themes)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

(use-package 'move-dup)
(global-set-key [M-up] 'md-move-lines-up)
(global-set-key [M-down] 'md-move-lines-down)
(global-set-key [M-S-up] 'md-move-lines-up)
(global-set-key [M-S-down] 'md-move-lines-down)

(use-package 'avy)
(global-set-key (kbd "C-;") 'avy-goto-char-timer)

(use-package 'multiple-cursors)
(defun mc/save-lists () "Ignore save history.")
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(use-package 'smex t)
(before-aproject-change
 (smex-save-to-file))
(after-aproject-change
 (setq smex-save-file (aproject-store-file "smex"))
 (smex-load-save-file))
(global-set-key [remap execute-extended-command] 'smex)

(use-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(after-load 'which-key
  (diminish 'which-key-mode))

;; buffer and file

(defun browse-current-file ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p filename))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" filename)))))
(global-set-key (kbd "M-g f b") 'browse-current-file)

(defun delete-current-buffer-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "M-g f d") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (when (file-exists-p filename)
            (rename-file filename new-name 1))
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "M-g f r") 'rename-current-buffer-file)

;; comment

(defun comment-like-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))
(global-set-key (kbd "M-;")     'comment-like-eclipse)

;; move line

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key [remap move-beginning-of-line] 'smart-beginning-of-line)

(defun smart-end-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (when (re-search-forward "[ \t]*$" (point-at-eol) t)
      (goto-char (match-beginning 0)))
    (when (= oldpos (point))
      (end-of-line))))
(global-set-key [remap move-end-of-line] 'smart-end-of-line)

;; buffer indent & beautify

(defvar buffer-indent-disabled-modes
  '(sql-mode text-mode shell-mode eshell-mode term-mode))
(defun buffer-intent ()
  (interactive)
  (unless (apply 'derived-mode-p buffer-indent-disabled-modes)
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))
(global-set-key (kbd "<f12>")  'buffer-intent)

(defvar buffer-beautify-alist nil)
(defun buffer-beautify ()
  (interactive)
  (let ((beautify (cdr (assoc major-mode buffer-beautify-alist))))
    (if (not beautify)
        (buffer-intent)
      (let ((c-point (point))
            (w-start (window-start)))
        (funcall beautify)
        (goto-char c-point)
        (goto-char (line-beginning-position))
        (set-window-start (selected-window) w-start)))))
(global-set-key (kbd "M-<f12>") 'buffer-beautify)

(provide 'init-enhance)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; init-enhance.el ends here
