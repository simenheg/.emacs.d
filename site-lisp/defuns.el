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

(defun c-find-corresponding-file ()
  "Switch to a buffer visiting the corresponding C/header file."
  (interactive)
  (let* ((file-split (split-string (buffer-name) "\\."))
         (file-stem (car file-split))
         (file-suffix (cadr file-split))
         (file-name (concat file-stem
                            (if (string-equal file-suffix "h")
                                ".c" ".h"))))
    (when (file-readable-p file-name)
      (find-file file-name))))

(defun c-include-header (header)
  "Append a file to the list of included header files."
  (interactive "MHeader name: ")
  (save-excursion
    (goto-char (point-min))
    (insert "#include <" header ".h>\n")))

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

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

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
    (whitespace-cleanup)
    (indent-region beginning end nil)
    (untabify beginning end)))

(provide 'defuns)

;;; defuns.el ends here
