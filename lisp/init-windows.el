;;; -*- lexical-binding: t -*-

(setq winner-dont-bind-my-keys 5)
(winner-mode 1)

(require-package 'switch-window)
(setq-default
 switch-window-shortcut-style 'alphabet
 switch-window-timeout nil)
(global-set-key (kbd "C-x o")   'switch-window)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(defun kill-buffers-exclude-current ()
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(global-set-key (kbd "C-x 4 1") 'kill-buffers-exclude-current)

(defun kill-buffers-switch-scratch ()
  (interactive)
  (delete-other-windows)
  (switch-to-scratch-buffer)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(global-set-key (kbd "C-x 4 0") 'kill-buffers-switch-scratch)

(defun toggle-delete-other-windows ()
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))
(global-set-key (kbd "C-x 1") 'toggle-delete-other-windows)

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))
(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun split-window-horizontally-instead ()
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))
(global-set-key (kbd "C-x 4 3") 'split-window-horizontally-instead)

(defun split-window-vertically-instead ()
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))
(global-set-key (kbd "C-x 4 2") 'split-window-vertically-instead)

(defun smart-last-usage-buffer ()
  (let ((target-buffer nil) (blist (cdr (buffer-list (selected-frame)))))
    (catch 'break
      (while blist
        (let (name mode (buffer (car blist)))
          (setq blist (cdr blist))
          (setq name (buffer-name buffer))
          (setq mode (buffer-major-mode buffer))
          (unless (or (string-prefix-p " " name)
                      (and (string-prefix-p "*" name)
                           (string-suffix-p "*" name)
                           (not (member name '("*scratch*" "*shell*"))))
                      (member mode '(dired-mode gtags-select-mode)))
            (setq target-buffer buffer)
            (throw 'break nil)))))
    target-buffer))

(defun split-window-most-recent-buffer()
  (interactive)
  (if (eq last-command 'split-window-most-recent-buffer)
      (progn
        (jump-to-register :split-window-most-recent-buffer)
        (setq this-command 'split-window-most-recent-buffer-unset))
    (window-configuration-to-register :split-window-most-recent-buffer)
    (switch-to-buffer-other-window (smart-last-usage-buffer))))
(global-set-key (kbd "C-<f6>")  'split-window-most-recent-buffer)

(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))
(global-set-key (kbd "<f8>")    'switch-to-scratch-buffer)

(defun switch-to-empty-scratch-buffer ()
  (interactive)
  (switch-to-scratch-buffer)
  (buffer-disable-undo)
  (erase-buffer)
  (buffer-enable-undo))
(global-set-key (kbd "C-<f8>")  'switch-to-empty-scratch-buffer)

(defun switch-to-last-usage-buffer()
  (interactive)
  (switch-to-buffer (smart-last-usage-buffer)))
(global-set-key (kbd "<f6>")    'switch-to-last-usage-buffer)

(defun switch-to-shell-buffer ()
  (interactive)
  (let ((buffer (get-buffer "*shell*")))
    (unless (buffer-live-p buffer)
      (shell)
      (setq buffer (get-buffer "*shell*")))
    (switch-to-buffer buffer)))
(global-set-key (kbd "M-<f8>")  'switch-to-shell-buffer)

(provide 'init-windows)
