;;; agtags.el --- emacs frontend to GNU Global source code tagging system  -*- lexical-binding: t; -*-

;;; Commentary:
;; A package to integrate GNU Global source code tagging system
;; (http://www.gnu.org/software/global) with Emacs.

;;; Code:
(require 'grep)

(defvar agtags-mode)

(defgroup agtags nil
  "GNU Global source code tagging system."
  :group 'tools)

(defcustom agtags-key-prefix "C-x t"
  "It is used for the prefix key of agtags's command."
  :safe 'stringp
  :type 'string
  :group 'agtags)

(defcustom agtags-auto-dwim nil
  "Non-nil if `dwim' in agtags-*-mode when Global finished."
  :safe 'booleanp
  :type 'boolean
  :group 'agtags)

(defcustom agtags-global-ignore-case nil
  "Non-nil if Global should ignore case in the search pattern."
  :safe 'booleanp
  :type 'boolean
  :group 'agtags)

(defcustom agtags-global-treat-text nil
  "Non-nil if Global should include matches from text files.
This affects `agtags-find-file' and `agtags-find-grep'."
  :safe 'booleanp
  :type 'boolean
  :group 'agtags)

(defun agtags/get-root ()
  "Get and validate env  `GTAGSROOT`."
  (let ((path (getenv "GTAGSROOT")))
    (when (zerop (length path))
      (error "No env `GTAGSROOT` provided"))
    path))

(defun agtags/auto-update()
  "Auto update tags file, when buffer was save."
  (when (and agtags-mode
             buffer-file-name
             (string-prefix-p (agtags/get-root) buffer-file-name))
    (call-process "global" nil nil nil "-u" (concat "--single-update=" buffer-file-name))))

(defun agtags/update-tags ()
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

(defun agtags/global-start (arguments &optional result)
  "Execute the global command, use ARGUMENTS; output format use RESULT."
  (let* ((xr (or result "grep"))
         (xs (append (list "global"
                           "-v"
                           (format "--result=%s" xr)
                           (and agtags-global-ignore-case "--ignore-case")
                           (and agtags-global-treat-text "--other"))
                     arguments)))
    (compilation-start (mapconcat #'identity (delq nil xs) " ")
                       (if (string= xr "path") 'agtags-path-mode 'agtags-grep-mode))))

(defun agtags/completing (flag string predicate code)
  "Completion function with FLAG for `completing-read'. Require: STRING PREDICATE CODE."
  (let ((option (cond ((eq flag 'files)   (if agtags-global-treat-text "-cPo" "-cP"))
                      ((eq flag 'grtags)  "-cr")
                      ((eq flag 'gsyms)   "-cs")
                      (t                  "-c")))
        (complete-list (make-vector 63 0)))
    (if agtags-global-ignore-case
        (setq option (concat option "i")))
    (with-temp-buffer
      (call-process "global" nil t nil option string)
      (goto-char (point-min))
      (while (not (eobp))
        (looking-at ".*")
        (intern (buffer-substring (match-beginning 0) (match-end 0)) complete-list)
        (forward-line)))
    (cond ((eq code nil)
           (try-completion string complete-list predicate))
          ((eq code t)
           (all-completions string complete-list predicate))
          ((eq code 'lambda)
           (if (intern-soft string complete-list) t nil)))))

(defun agtags/current-token ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun agtags/read-input (prompt)
  "Read a value from the minibuffer with PROMPT."
  (let* ((final-prompt (format "%s: " prompt)))
    (read-from-minibuffer final-prompt)))

(defun agtags/read-input-dwim (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (agtags/current-token))
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

(defun agtags/open-file ()
  "Input pattern (PATTERN), search with grep(1) and move to the locations."
  (interactive)
  (let ((default-directory (agtags/get-root))
        (user-input (completing-read
                     "Open File: "
                     (lambda (string predicate code)
                       (agtags/completing 'files string predicate code))
                     nil t)))
    (when (> (length user-input) 0)
      (find-file user-input))))

(defun agtags/find-file (pattern)
  "Input pattern (PATTERN), search with grep(1) and move to the locations."
  (interactive (list (agtags/read-input "Find files")))
  (agtags/global-start (list "--path" pattern) "path"))

(defun agtags/find-with-grep (pattern)
  "Input pattern (PATTERN), search with grep(1) and move to the locations."
  (interactive (list (agtags/read-input-dwim "Search string")))
  (agtags/global-start (list "--grep" pattern)))

(defconst agtags/global-mode-font-lock-keywords
  '(("^Global \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ("^Global found \\([0-9]+\\)" (1 compilation-info-face))))

(defconst agtags/global-mode-map
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

(defconst agtags/path-regexp-alist
  `((,"^\\(?:[^\"'\n]*/\\)?[^ )\t\n]+$" 0)))

(defconst agtags/grep-regexp-alist
  `((,"^\\(.+?\\):\\([0-9]+\\):\\(?:$\\|[^0-9\n]\\|[0-9][^0-9\n]\\|[0-9][0-9].\\)"
     1 2
     (,(lambda ()
         (let* ((start (1+ (match-end 2)))
                (mbeg (text-property-any start (line-end-position) 'global-color t)))
           (and mbeg (- mbeg start)))))
     nil 1)))

(defun agtags/global-mode-finished (buffer status)
  "Function to call when a gun global process finishes.
BUFFER is the global's mode buffer, STATUS was the finish status."
  (when (and agtags-auto-dwim (string-match-p "^finished" status))
    (let ((num-found 0))
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "^\\([0-9]+\\)\\ \\(file\\|files\\|object\\|objects\\)\\ located" nil t)
          (setq num-found (string-to-number (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))
      ;; dwim
      (cond ((< num-found 1)
             (kill-buffer-and-window))
            ((= num-found 1)
             (next-error-no-select)
             (kill-buffer-and-window))
            (t (pop-to-buffer  buffer))))))

(defvar agtags-grep-mode-map agtags/global-mode-map)
(defvar agtags-grep-mode-font-lock-keywords agtags/global-mode-font-lock-keywords)

;;;###autoload
(define-derived-mode agtags-grep-mode grep-mode "Global Grep"
  "A mode for showing outputs from gnu global."
  (setq-local grep-scroll-output nil)
  (setq-local grep-highlight-matches nil)
  (setq-local compilation-error-regexp-alist agtags/grep-regexp-alist)
  (setq-local compilation-finish-functions #'agtags/global-mode-finished))

(defvar agtags-path-mode-map agtags/global-mode-map)
(defvar agtags-path-mode-font-lock-keywords agtags/global-mode-font-lock-keywords)

;;;###autoload
(define-compilation-mode agtags-path-mode "Global Files"
  "A mode for showing files from gnu global."
  (setq-local compilation-error-regexp-alist agtags/path-regexp-alist)
  (setq-local compilation-finish-functions #'agtags/global-mode-finished))

;;;###autoload
(define-minor-mode agtags-mode nil
  :lighter " G"
  (if agtags-mode
      (add-hook 'before-save-hook 'agtags/auto-update nil 'local)
    (remove-hook 'before-save-hook 'agtags/auto-update 'local)))

;;;###autoload
(defun agtags-bind-keys()
  "Set global key bindings for agtags."
  (dolist (pair '(("b" . agtags/update-tags)
                  ("f" . agtags/open-file)
                  ("F" . agtags/find-file)
                  ("p" . agtags/find-with-grep)))
    (global-set-key (kbd (concat agtags-key-prefix " " (car pair))) (cdr pair))))

(provide 'agtags)
;;; agtags.el ends here
