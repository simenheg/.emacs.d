;;; defuns.el --- Miscellaneous utility functions

(defun auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already exists an
.elc file corresponding to the current buffer file, then recompile the file."
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (concat (buffer-file-name) "c")))
    (byte-compile-file buffer-file-name)))

(defun beginning-of-indentation-or-line ()
  "Sets point back to indentation level. If already there, sets point to
beginning of line."
  (interactive)
  (let ((prev (point)))
    (back-to-indentation)
    (when (= prev (point))
      (beginning-of-line))))

;; Find the header or source file corresponding to this file.
(defalias 'c-find-corresponding-file 'ff-find-other-file)

(defun c-include-header (header)
  "Append a file to the list of included header files."
  (interactive "MHeader name: ")
  (save-excursion
    (goto-char (point-min))
    (insert "#include <" header ".h>\n")))

(defun dired-beginning-of-buffer ()
  "Jump to the first file listed in Dired."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 2))

(defun dired-end-of-buffer ()
  "Jump to the last file listed in Dired."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defadvice dired-next-line (after dired-next-line (arg) activate)
  "Don't move past the last file in Dired."
  (when (= (point-max) (point))
    (dired-previous-line arg)))

(defadvice dired-previous-line (around dired-previous-line (arg) activate)
  "Don't move before the first in Dired."
  (when (< 3 (line-number-at-pos))
    ad-do-it))

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

(defun inc-next-number (&optional arg)
  "Increment first number found after point (ala Vim's C-a)."
  (interactive "p")
  (or arg (setq arg 1))
  (when (search-forward-regexp "\\([0-9]+\\)" nil t)
    (let ((num (string-to-number (match-string 0))))
      (replace-match (number-to-string (+ num arg))))
    (goto-char (match-beginning 0))))

(defun insert-header (title)
  "Insert a neat, commented header."
  (interactive "MHeader: ")
  (comment-dwim nil)
  (let ((offset (+ (length title) 4)))
    (while (< (+ (current-column) offset) fill-column)
      (insert "-")))
  (insert " [ " title " ]"))

(defun kill-region-or-backward-kill-sexp (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-sexp'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-sexp arg)))

(defun mail-add-contact (full-name alias email-address)
  (interactive "sFull name: \nsAlias: \nsEmail address: ")
  (append-to-file
   (format "alias %s \"%s <%s>\"\n" alias full-name email-address)
   nil
   mail-personal-alias-file))

(defun message-insert-citation-line ()
  "Insert a simple citation line."
  (when message-reply-headers
    (insert (mail-header-from message-reply-headers) " writes:")
    (newline)))

(defadvice split-window-right (after balance-windows activate)
  "Workaround for unbalanced splits."
  (balance-windows))

(defun tidy-buffer ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beginning (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beginning end nil)
    (whitespace-cleanup)
    (untabify beginning end)))

(provide 'defuns)

;;; defuns.el ends here
