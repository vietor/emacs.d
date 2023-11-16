;;; init-windows.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package winner
  :hook (after-init . winner-mode)
  :init
  (setq winner-dont-bind-my-keys 5))

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :init
  (setq-default switch-window-shortcut-style 'qwerty)
  (setq-default switch-window-timeout nil))

;; switch buffers

(defun switch-to-scratch-buffer ()
  (interactive)
  (let ((exists (get-buffer "*scratch*"))
        (buffer (get-buffer-create "*scratch*")))
    (unless exists
      (set-buffer-major-mode buffer))
    (switch-to-buffer buffer)))
(bind-key "<f8>" 'switch-to-scratch-buffer)

(defun switch-to-empty-scratch-buffer ()
  (interactive)
  (switch-to-scratch-buffer)
  (buffer-disable-undo)
  (erase-buffer)
  (buffer-enable-undo))
(bind-key "C-<f8>" 'switch-to-empty-scratch-buffer)

(defun switch-to-shell-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*shell*")))
    (unless (buffer-live-p buffer)
      (shell)
      (setq buffer (get-buffer "*shell*")))
    (switch-to-buffer buffer)))
(bind-key "M-<f8>" 'switch-to-shell-buffer)

(defun candidate-buffer-p(buffer)
  (let ((name (buffer-name buffer)))
    (and (not (get-buffer-window buffer))
         (or (not (or (string-prefix-p " " name)
                      (string-prefix-p "*" name)))
             (seq-contains '("*scratch*" "*shell*") name)))))

(defun switch-to-candidate-buffer ()
  (interactive)
  (let ((buffer nil))
    (setq buffer (seq-find 'candidate-buffer-p (cdr (buffer-list))))
    (when buffer
      (switch-to-buffer buffer))))
(bind-key "<f7>" 'switch-to-candidate-buffer)

;; kill buffers

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (ignore-errors
    (delete-window)))
(bind-key "C-x C-k" 'kill-current-buffer)

(defun kill-buffers-exclude-current ()
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(bind-key "C-x 4 1" 'kill-buffers-exclude-current)

(defun kill-buffers-switch-scratch ()
  (interactive)
  (delete-other-windows)
  (switch-to-empty-scratch-buffer)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(bind-key "C-x 4 0" 'kill-buffers-switch-scratch)

(defun toggle-delete-other-windows ()
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))
(bind-key "C-x 1" 'toggle-delete-other-windows)

;; split windows

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))
(bind-key "C-x 2" (split-window-func-with-other-buffer 'split-window-vertically))
(bind-key "C-x 3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun split-window-horizontally-instead ()
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))
(bind-key "C-x 4 3" 'split-window-horizontally-instead)

(defun split-window-vertically-instead ()
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))
(bind-key "C-x 4 2" 'split-window-vertically-instead)

(provide 'init-windows)
;;; init-windows.el ends here
