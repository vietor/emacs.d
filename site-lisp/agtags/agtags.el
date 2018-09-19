;;; agtags.el --- emacs frontend to GNU Global source code tagging system  -*- lexical-binding: t; -*-

;;; Commentary:
;; A package to integrate GNU Global source code tagging system
;; (http://www.gnu.org/software/global) with Emacs.

;;; Code:
(require 'grep)

(defvar agtags-mode)
(defvar agtags-key-prefix "C-x w")

(defun agtags/get-root ()
  "Get and validate env  `GTAGSROOT`."
  (let ((path (getenv "GTAGSROOT")))
    (when (zerop (length path))
      (error "No env `GTAGSROOT` provided"))
    path))

(defun agtags/auto-update()
  "Auto update tags file, when buffer was save"
  (when (and agtags-mode
             buffer-file-name
             (string-prefix-p (agtags/get-root) buffer-file-name))
    (call-process "global" nil nil nil "-u" (concat "--single-update=" buffer-file-name))))

(defun agtags/build-tags ()
  "Create tag files (e.g. GTAGS) in directory `GTAGSROOT`."
  (interactive)
  (let ((rootpath (agtags/get-root)))
    (dolist (file (list "GRTAGS" "GPATH" "GTAGS"))
      (ignore-errors
        (delete-file (expand-file-name file rootpath))))
    (with-temp-buffer
      (cd rootpath)
      (when (zerop (call-process (executable-find "gtags") nil t nil "-i"))
        (message "Tags create or update by GTAGS")))))

(defun agtags/global-start (arguments)
  "Execute the global command, use ARGUMENTS."
  (let ((command-string
         (mapconcat #'shell-quote-argument (append (list "global" "--result=grep") arguments) " ")))
    (compilation-start command-string 'agtags-global-mode)))

(defun agtags/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun agtags/read-input (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (agtags/dwim-at-point))
         (final-prompt
          (if suggested
              (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt)))
         ;; Ask the user for input, but add `suggested' to the history
         ;; so they can use M-n if they want to modify it.
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil nil suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
        user-input
      suggested)))

(defun agtags/search (string)
  "Input pattern (STRING), search with grep(1) and move to the locations."
  (interactive (list (agtags/read-input "Search string")))
  (agtags/global-start (list "-g" string)))

(defvar agtags-global-mode-font-lock-keywords
  '(("^Global \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ("^Global found \\([0-9]+\\)" (1 compilation-info-face))))

(defvar agtags-global-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map "\r" 'compile-goto-error)
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
    map))

(define-derived-mode agtags-global-mode grep-mode "Global"
  "A mode for showing outputs from gnu global.")

(define-minor-mode agtags-mode nil
  :lighter " G"
  (if agtags-mode
      (add-hook 'before-save-hook 'agtags/auto-update nil 'local)
    (remove-hook 'before-save-hook 'agtags/auto-update 'local)))

(defun agtags-bind-key()
  (dolist (pair '(("b" . agtags/build-tags)
                  ("p" . agtags/search)))
    (global-set-key (kbd (concat agtags-key-prefix " " (car pair))) (cdr pair))))

(provide 'agtags)
;;; agtags.el ends here
