;;; init-enhance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :ensure t
  :when window-system
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t))

(use-package highlight-escape-sequences
  :ensure t
  :hook (after-init . hes-mode))

(use-package move-dup
  :ensure t
  :diminish
  :hook (after-init . global-move-dup-mode))

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char-timer))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" .  mc/skip-to-previous-like-this)
         ("M-<down-mouse-1>" . nil)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (defun mc/save-lists () "Ignore save history."))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :hook (after-init . whole-line-or-region-global-mode))

(use-package browse-kill-ring
  :ensure t
  :bind (("M-Y" . browse-kill-ring)
         :map browse-kill-ring-mode-map
         ("C-g" . browse-kill-ring-quit)
         ("M-n" . browse-kill-ring-forward)
         ("M-p" . browse-kill-ring-previous))
  :init
  (setq browse-kill-ring-separator "\f"))

(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (setq-default which-key-idle-delay 1.5))

;; embed package

(use-package ya-formatter
  :demand t
  :bind
  ("<f12>" . ya-formatter-indent)
  ("M-<f12>" . 'ya-formatter-beautify)
  ("C-M-<f12>" . 'ya-formatter-clean-eol))

(use-package insert-string
  :demand t
  :bind
  ("M-g t i" . insert-random-string)
  ("M-g t t" . insert-ordered-string))

;; buffer and file

(defun browse-current-file ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p filename))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" (if (eq system-type 'windows-nt) "/") filename)))))
(bind-key "M-g f b" 'browse-current-file)

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
(bind-key "M-g f d" 'delete-current-buffer-file)

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
(bind-key "M-g f r" 'rename-current-buffer-file)

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
(bind-key "M-;" 'comment-like-eclipse)

;; move line

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(bind-key [remap move-beginning-of-line] 'smart-beginning-of-line)

(defun smart-end-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (when (re-search-forward "[ \t]*$" (point-at-eol) t)
      (goto-char (match-beginning 0)))
    (when (= oldpos (point))
      (end-of-line))))
(bind-key [remap move-end-of-line] 'smart-end-of-line)

(provide 'init-enhance)
;;; init-enhance.el ends here
