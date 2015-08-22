;;; defuns.el --- Miscellaneous utility functions

(defun auto-byte-recompile ()
  "If the current buffer is in emacs-lisp-mode and there already
exists an .elc file corresponding to the current buffer file,
then recompile the file."
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (concat (buffer-file-name) "c")))
    (byte-compile-file buffer-file-name)))

(defun beginning-of-indentation-or-line ()
  "Sets point back to indentation level. If already there, sets
point to beginning of line."
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

;; Thanks to Mickey Petersen at masteringemacs.org
(defun clean-mode-line (alist)
  (interactive)
  (dolist (cleaner alist)
    (let* ((mode (car cleaner))
           (mode-str (cdr cleaner))
           (old-mode-str (cdr (assq mode minor-mode-alist))))
      (when old-mode-str
        (setcar old-mode-str mode-str))
      ;; Major mode
      (when (eq mode major-mode)
        (setq mode-name mode-str)))))

(defun dec-next-number (&optional arg)
  "Decrease first number found after point (ala Vim's C-x)."
  (interactive "p")
  (inc-next-number (* (or arg 1) -1)))

(declare-function dired-next-line "dired" (arg))

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

(defadvice dired-previous-line (around dired-previous-line (arg)
                                       activate)
  "Don't move before the first in Dired."
  (when (< 3 (line-number-at-pos))
    ad-do-it))

(defun duplicate (arg)
  "Duplicate the current line, or region if active."
  (interactive "p")
  (let ((beginning
         (if (region-active-p)
             (region-beginning)
           (line-beginning-position)))
        (end
         (if (region-active-p)
             (region-end)
           (line-end-position)))
        (point (point)))
    (goto-char end)
    (dotimes (_ arg)
      (end-of-line)
      (newline)
      (insert (buffer-substring beginning end)))
    (backward-char (- end point))))

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

(defun fmt (symbols)
  "Insert '(format t \"x1: ~a, x2: ~a, ..., xn: ~a~%)' for every
  x separated by a space in `symbols'."
  (interactive "MPrint symbols: ")
  (insert
   (format
    "(format t \"%s: ~a~%%\" %s)"
    (replace-regexp-in-string " " ": ~a, " symbols)
    symbols)))

(defmacro define-eshell-command (name buffer-name command)
  "Define function NAME that runs the given COMMAND in an Eshell
buffer named BUFFER-NAME, creating it if necessary.

A numeric prefix arg switches to the session with that number,
creating it if necessary. A non-numeric prefix arg means to
create a new session."
  `(defun ,name (&optional arg)
     (interactive "P")
     (declare-function eshell-return-to-prompt "em-hist")
     (declare-function eshell-reset "esh-mode")
     (declare-function eshell-send-input "esh-mode")
     (let ((eshell-buffer-name ,buffer-name))
       (eshell arg)
       (unless (comint-check-proc (current-buffer))
         (eshell-return-to-prompt)
         (when (char-after (point))
           (eshell-reset))
         (insert ,command)
         (eshell-send-input)))))

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
  (let ((offset (+ (length title) 5)))
    (while (< (+ (current-column) offset) fill-column)
      (insert "-")))
  (insert " [ " title " ]"))

(defun kill-region-or-backward-kill-sexp (&optional arg region)
  "`kill-region' if the region is active, otherwise
`backward-kill-sexp'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end) t)
    (backward-kill-sexp arg)))

(defun mail-add-contact (full-name alias email-address)
  (interactive "sFull name: \nsAlias: \nsEmail address: ")
  (append-to-file
   (format "alias %s \"%s <%s>\"\n" alias full-name email-address)
   nil
   mail-personal-alias-file))

(defun message-insert-citation-line ()
  "Insert a simple citation line."
  (defvar message-reply-headers)
  (declare-function mail-header-from "nnheader")
  (when message-reply-headers
    (insert (mail-header-from message-reply-headers) " writes:")
    (newline)))

(defun move-line-down ()
  "Transpose line at point with the line below.
Point stays at the same position in the original line."
  (interactive)
  (let ((col (current-column)))
    (forward-line)
    (transpose-lines 1)
    (forward-line -1)
    (move-to-column col)))

(defun move-line-up ()
  "Transpose line at point with the line above.
Point stays at the same position in the original line."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (move-to-column col)))

(defun revert-buffer-noconfirm ()
  "Like `revert-buffer', but don't ask for confirmation."
  (interactive)
  (revert-buffer nil t))

(defadvice split-window-right (after balance-windows activate)
  "Workaround for unbalanced splits."
  (balance-windows))

(defun tidy-buffer ()
  "Ident, untabify and unwhitespacify current buffer, or region
if active."
  (interactive)
  (let ((beginning
         (if (region-active-p) (region-beginning) (point-min)))
        (end
         (if (region-active-p) (region-end) (point-max))))
    (indent-region beginning end)
    (whitespace-cleanup)
    (untabify beginning (if (< end (point-max)) end (point-max)))))

(defun today ()
  "Insert today's date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Courtesy of https://github.com/larstvei/
(defun try (url)
  (interactive (list (read-from-minibuffer "url: ")))
  (with-current-buffer (url-retrieve-synchronously url)
    (eval-region (search-forward-regexp "^$") (point-max))))

(provide 'defuns)

;;; defuns.el ends here
