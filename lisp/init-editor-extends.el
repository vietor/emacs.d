(defun kill-back-to-indentation ()
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(defun delete-current-buffer-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
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

(defvar retain-trailing-whitespace nil)
(make-variable-buffer-local 'retain-trailing-whitespace)

(defadvice delete-trailing-whitespace (around retain activate)
  (unless retain-trailing-whitespace
    ad-do-it))

(defvar buffer-indent-function nil)
(defvar deep-buffer-indent-function nil)
(make-variable-buffer-local 'buffer-indent-function)
(make-variable-buffer-local 'deep-buffer-indent-function)

(defun indent-current-buffer ()
  (interactive)
  (if (functionp buffer-indent-function)
      (funcall buffer-indent-function)
    (save-excursion
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max)))))

(defun deep-indent-current-buffer ()
  (interactive)
  (if (functionp deep-buffer-indent-function)
      (funcall deep-buffer-indent-function)
    (indent-current-buffer)))

(global-set-key (kbd "<f12>")   'indent-current-buffer)
(global-set-key (kbd "M-<f12>") 'deep-indent-current-buffer)	

(defun comment-eclipse ()
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
	
(global-set-key (kbd "M-;")     'comment-eclipse)

(provide 'init-editor-extends)