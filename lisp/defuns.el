;;; defuns.el --- Miscellaneous utility functions

(require 'seq)
(eval-when-compile (require 'subr-x))

(defun alphabet ()
  "Display the alphabet."
  (interactive)
  (message
   (concat
    "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z\n"
    "a b c d e f g h i j k l m n o p q r s t u v w x y z")))

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

(defun copy-buffer-file-name ()
  "Put the current buffer file name in the kill ring."
  (interactive)
  (let ((name (buffer-file-name)))
    (when name
      (kill-new name))
    (message (or name "Buffer is not visiting a file"))))

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
  "Duplicate the current line, or region if active.
When called with a prefix argument the current line or region is
commented out before it's copied."
  (interactive "P")
  (setq arg (or arg 1))
  (let ((beg (if (region-active-p)
                 (region-beginning)
               (line-beginning-position)))
        (end (if (region-active-p)
                 (region-end)
               (line-end-position)))
        (point (point)))
    (goto-char end)
    (let ((to-duplicate (buffer-substring beg end)))
      (when (listp arg)
        (comment-region beg end)
        (setq arg 1))
      (dotimes (_ arg)
        (end-of-line)
        (newline)
        (insert to-duplicate)))
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
  "Insert a neatly formatted, commented header."
  (interactive "MHeader: ")
  (comment-dwim nil)
  (let* ((header (concat " [ " title " ]"))
         (ndashes (- fill-column (current-column) (length header))))
    (insert (concat (make-string ndashes ?-) header))))

(defun insert-random-password (len)
  "Insert a password-friendly random string of length LEN."
  (interactive "NLength: ")
  (insert
   (random-string
    "!#%+23456789:=?@ABCDEFGHJKLMNPRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    len)))

(defun insert-random-string (len)
  "Insert a random alphanumeric ASCII-string of length LEN."
  (interactive "NLength: ")
  (insert
   (random-string
    (append (number-sequence ?0 ?9)
            (number-sequence ?a ?z)
            (number-sequence ?A ?Z))
    len)))

(defun kill-region-or-backward-delete-sexp (&optional arg region)
  "Kill region if active, else backward delete sexp."
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end) t)
    (let ((end (point)))
      (save-excursion
        (backward-sexp)
        (delete-region (point) end)))))

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

(defun make-list* (n fun &rest args)
  "Call FUN with ARGS N times and return a list of the results."
  (let ((res '()))
    (dotimes (_ n)
      (push (apply fun args) res))
    res))

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

(defun random-string (chars len)
  "Return a string of LEN random characters from CHARS."
  (apply #'string (make-list* len #'seq-random-elt chars)))

(defun revert-buffer-noconfirm ()
  "Like `revert-buffer', but don't ask for confirmation."
  (interactive)
  (revert-buffer nil t))

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

(defun unix-now ()
  "Insert number of seconds since 1970-01-01 00:00:00 UTC."
  (interactive)
  (insert (string-trim (shell-command-to-string "date +%s"))))

(provide 'defuns)

;;; defuns.el ends here
