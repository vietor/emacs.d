;;; init-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when window-system
  (require-package 'doom-themes)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)

(require-package 'move-dup)
(global-set-key [M-up] 'md-move-lines-up)
(global-set-key [M-down] 'md-move-lines-down)
(global-set-key [M-S-up] 'md-move-lines-up)
(global-set-key [M-S-down] 'md-move-lines-down)

(require-package 'multiple-cursors)
(defun mc/save-lists () "Ignore save history.")
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(when (maybe-require-package 'smex)
  (after-aproject-change
   (setq smex-save-file (aproject-store-file "smex")))
  (global-set-key [remap execute-extended-command] 'smex))

(when (maybe-require-package 'ivy)
  (require-package 'swiper)
  (add-hook 'after-init-hook 'ivy-mode)
  (after-load 'ivy
    (diminish 'ivy-mode)
    (setq-default ivy-use-virtual-buffers t
                  ivy-virtual-abbreviate 'fullpath
                  ivy-count-format ""
                  ivy-magic-tilde nil
                  ivy-dynamic-exhibit-delay-ms 150
                  ivy-use-selectable-prompt t
                  ivy-initial-inputs-alist '((Man-completion-table . "^")
                                             (woman . "^")))
    (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point)
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
    (dolist (k '("C-j" "C-RET"))
      (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done)))

  (require-package 'counsel)
  (add-hook 'after-init-hook 'counsel-mode)
  (after-load 'counsel
    (diminish 'counsel-mode)
    (setq-default counsel-mode-override-describe-bindings t)))

;; buffer and file

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

;; buffer indent

(defvar buffer-indent-ignore-modes
  '(sql-mode text-mode))
(defvar-local buffer-indent-function nil)
(defvar-local deep-buffer-indent-function nil)

(defun indent-current-buffer ()
  (interactive)
  (if (functionp buffer-indent-function)
      (funcall buffer-indent-function)
    (unless (apply 'derived-mode-p buffer-indent-ignore-modes)
      (save-excursion
        (delete-trailing-whitespace)
        (indent-region (point-min) (point-max))))))
(global-set-key (kbd "<f12>")   'indent-current-buffer)

(defun deep-indent-current-buffer ()
  (interactive)
  (if (functionp deep-buffer-indent-function)
      (funcall deep-buffer-indent-function)
    (indent-current-buffer)))
(global-set-key (kbd "M-<f12>") 'deep-indent-current-buffer)

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
    (beginning-of-line)
    (and (= oldpos (point))
         (back-to-indentation))))
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

(provide 'init-enhance)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
;;; init-enhance.el ends here
