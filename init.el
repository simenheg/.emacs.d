;; init.el --- Personal Emacs configurations
;; Author: Simen Heggestøyl <simenheg@gmail.com>

(setq
 package-archives
 '(("GNU" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("MELPA" . "http://melpa.org/packages/")))

(defvar extra-packages
  '(ace-jump-mode
    diminish
    elpy
    exec-path-from-shell
    fuel
    geiser
    google-translate
    ido-vertical-mode
    js2-mode
    json-mode
    know-your-http-well
    lua-mode
    magit
    markdown-mode
    multiple-cursors
    paredit
    projectile
    rainbow-mode
    rdf-prefix
    restclient
    smex
    sparql-mode
    ttl-mode
    web-mode))

(package-initialize)

(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; Install missing packages
(dolist (p extra-packages)
  (unless (package-installed-p p)
    (package-install p)))

(set-language-environment "UTF-8")
(exec-path-from-shell-initialize)

(setq-default
 indent-tabs-mode             nil  ; Use spaces for indentation
 major-mode             'org-mode) ; Org-mode as default mode

(setq
 auto-revert-verbose          nil  ; Be quiet about reverts
 comint-input-ignoredups        t  ; Ignore duplicate history
 compare-ignore-whitespace      t  ; Ignore whitespace differences
 default-input-method       "TeX"  ; TeX as default input method
 disabled-command-function    nil  ; Enable disabled commands
 display-time-24hr-format       t  ; 24 hour time format
 eshell-hist-ignoredups         t  ; Ignore duplicate history
 history-delete-duplicates      t  ; Delete duplicate history entries
 inhibit-startup-screen         t  ; No startup screen
 initial-scratch-message      nil  ; No scratch message
 kill-read-only-ok              t  ; Killing read-only text is OK
 password-cache-expiry        nil  ; Cache TRAMP passwords forever
 show-paren-delay               0) ; Don't delay the paren update

(blink-cursor-mode              0) ; No blinking cursor
(column-number-mode             1) ; Show column number
(fset 'yes-or-no-p      'y-or-n-p) ; Make "yes/no" prompts "y/n"
(global-auto-revert-mode        1) ; Reload files after modification
(global-prettify-symbols-mode   1) ; Pretty symbols (e.g. lambda => λ)
(menu-bar-mode                 -1) ; No menu bar
(prefer-coding-system      'utf-8) ; Always prefer UTF-8
(scroll-bar-mode               -1) ; No scroll bar
(show-paren-mode                1) ; Highlight matching parenthesis
(tool-bar-mode                 -1) ; No tool bar

(defun conf (path)
  "Return the absolute path of config file/directory PATH."
  (expand-file-name (concat user-emacs-directory path)))

;; Add site-lisp/ and all subdirectories to the load-path
(let ((default-directory (conf "site-lisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Miscellaneous utility functions
(require 'defuns)

(global-set-key (kbd "C-'")     'org-cycle-agenda-files)
(global-set-key (kbd "C-+")     'dec-next-number)
(global-set-key (kbd "C-=")     'inc-next-number)
(global-set-key (kbd "C-a")     'beginning-of-indentation-or-line)
(global-set-key (kbd "C-c M-$") 'ispell-change-dictionary)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c a")   'org-agenda)
(global-set-key (kbd "C-c c")   'org-capture)
(global-set-key (kbd "C-c d")   'duplicate)
(global-set-key (kbd "C-c e")   'mc/edit-lines)
(global-set-key (kbd "C-c f")   'find-grep)
(global-set-key (kbd "C-c l")   'mc/mark-all-like-this)
(global-set-key (kbd "C-c n")   'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-p") 'list-packages)
(global-set-key (kbd "C-c r")   'rename-buffer)
(global-set-key (kbd "C-c t")   'google-translate-at-point)
(global-set-key (kbd "C-c v")   'magit-status)
(global-set-key (kbd "C-c w")   'compare-windows)
(global-set-key (kbd "C-w")     'kill-region-or-backward-kill-sexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-z")     'bury-buffer)
(global-set-key (kbd "M-x")     'smex)
(global-set-key [C-M-backspace] 'backward-kill-sexp)
(global-set-key [M-down]        'move-line-down)
(global-set-key [M-up]          'move-line-up)
(global-set-key [f5]            'revert-buffer-noconfirm)

;; -------------------------------------------------------- [ BibTeX ]
(setq-default bibtex-dialect 'biblatex)

;; ------------------------------------------------------------- [ C ]
(add-hook
 'c-mode-common-hook
 (lambda ()
   (auto-fill-mode 1)

   (local-set-key (kbd "C-c i") 'c-include-header)
   (local-set-key (kbd "C-c h") 'c-find-corresponding-file)

   ;; Try to create a sensible compilation string when no Makefile is
   ;; found
   (unless (file-exists-p "Makefile")
     (setq-local
      compile-command
      (let ((file (file-name-nondirectory buffer-file-name)))
        (format "%s %s %s -o %s"
                (or (getenv "CC") "gcc")
                file
                (or (getenv "CFLAGS") "-std=c99 -Wall")
                (file-name-sans-extension file)))))

   ;; Try to create a sensible compilation string when a Makefile is
   ;; found in the parent directory
   (when (file-exists-p "../Makefile")
     (setq-local compile-command "make devel -C .."))))

;; ------------------------------------------------------ [ Calendar ]
(setq calendar-location-name  "Oslo, Norway"
      calendar-latitude       60.0
      calendar-longitude      10.7
      calendar-week-start-day 1) ; Weeks start on Monday

;; Display time in 24 hour format
(setq calendar-time-display-form
      '(24-hours "." minutes
                 (if time-zone " (") time-zone (if time-zone ")")))

;; --------------------------------------------------- [ Color-theme ]
(load-theme 'leuven t)

;; --------------------------------------------------- [ Common Lisp ]
(setq inferior-lisp-program "sbcl")

;; More sensible `loop' indentation
(setq lisp-loop-forms-indentation   2
      lisp-simple-loop-indentation  2
      lisp-loop-keyword-indentation 6)

;; Stop SLIME's REPL from grabbing DEL, which is annoying when
;; backspacing over a ')'
(add-hook
 'slime-repl-mode-hook
 (lambda ()
   (defvar slime-repl-mode-map)
   (define-key slime-repl-mode-map
     (read-kbd-macro paredit-backward-delete-key) nil)))

;; -------------------------------------------------------- [ (S)CSS ]
(require 'css-mode-25.1)

(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(add-hook
 'css-mode-hook
 (lambda ()
   (rainbow-mode 1)
   (diminish 'rainbow-mode)))

;; --------------------------------------------------------- [ Dired ]
(add-hook
 'dired-load-hook
 (lambda ()
   (defvar dired-mode-map)

   (load "dired-x")

   (define-key dired-mode-map
     (vector 'remap 'end-of-buffer) 'dired-end-of-buffer)

   (define-key dired-mode-map
     (vector 'remap 'beginning-of-buffer) 'dired-beginning-of-buffer)

   (define-key dired-mode-map
     (vector 'remap 'dired-goto-file) 'find-file)))

(add-hook
 'dired-mode-hook
 (lambda ()
   (declare-function dired-omit-mode "dired-x")
   ;; Omit "uninteresting" files
   (dired-omit-mode 1)
   (diminish 'dired-omit-mode)))

;; Make file sizes human-readable, and hide time stamps
(setq-default dired-listing-switches "-alh --time-style=+")

(setq
 dired-dwim-target t                   ; Let Dired guess target
 dired-omit-verbose nil                ; Be quiet about omitted files
 dired-recursive-copies 'always        ; Don't ask, just copy
 global-auto-revert-non-file-buffers t ; Auto-refresh the file list
 image-dired-show-all-from-dir-max-files 500)

;; ------------------------------------------------------- [ DocView ]
;; Auto-update document files
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(setq doc-view-continuous t) ; Smooth document viewing

;; --------------------------------------------------------- [ Emacs ]
;; Set EDITOR environment variable
(setenv "EDITOR" "emacs -Q")

;; Fix mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Don't bother to ask unless the file *really* is large (100 MB+)
(setq large-file-warning-threshold 100000000)

;; Set current buffer as the window name
(setq-default frame-title-format (list "%b %f"))

;; Electric pairs
(electric-pair-mode 1)

;; Put #autosave# and backup~ files into own directory
(let ((autosave-directory (conf "autosaves/")))
  (setq backup-directory-alist `((".*" . ,autosave-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,autosave-directory t))))

;; Clean up trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ---------------------------------------------------- [ Emacs Lisp ]
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-b") 'eval-buffer)
   (eldoc-mode 1)))

(add-hook 'ielm-mode-hook (lambda () (eldoc-mode 1)))

(add-hook 'after-save-hook 'auto-byte-recompile)

;; ----------------------------------------------------------- [ ERC ]
(setq erc-fill-column fill-column)

;; -------------------------------------------------------- [ Factor ]
(setq fuel-factor-root-dir "~/src/factor")
(add-hook 'factor-mode-hook (lambda () (setq-local fill-column 64)))

;; ------------------------------------------------------- [ Flymake ]
(setq help-at-pt-display-when-idle t ; Activate echoed help messages
      help-at-pt-set-timer        0) ; Echo help instantly

;; ---------------------------------------------- [ Google Translate ]
(autoload 'google-translate-at-point "google-translate" nil t)
(autoload 'google-query-translate "google-translate" nil t)

(setq google-translate-default-source-language "no"
      google-translate-default-target-language "en")

;; ------------------------------------------------------ [ Ido mode ]
(ido-mode 'buffers)
(ido-vertical-mode 1)

;; ---------------------------------------------------- [ JavaScript ]
(setq inferior-js-program-command "nodejs")

(add-hook
 'js2-mode-hook
 (lambda ()
   (setq-local fill-column 79)
   (local-set-key (kbd "RET") 'js2-line-break)
   (yas-minor-mode 1)
   (subword-mode 1)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.webapp\\'" . json-mode))

(setq js2-strict-trailing-comma-warning nil)

;; ---------------------------------------------------------- [ Lisp ]
(eval-after-load "paredit"
  '(diminish 'paredit-mode))

(autoload 'let-fix "autolet" "Automatic let-form fixer" t)

(defun setup-lisp ()
  (auto-fill-mode 1)
  (paredit-mode 1))

(mapc
 (lambda (m) (add-hook m 'setup-lisp))
 '(emacs-lisp-mode-hook
   geiser-repl-mode-hook
   ielm-mode-hook
   inferior-lisp-mode-hook
   inferior-scheme-mode-hook
   lisp-interaction-mode-hook
   lisp-mode-hook
   scheme-mode-hook
   slime-repl-mode-hook))

;; ----------------------------------------------------------- [ Lua ]
;; Basic LӦVE compilation support
(add-hook
 'lua-mode-hook
 (lambda ()
   (global-set-key "\C-c\C-c" 'compile)
   (when (file-exists-p "main.lua")
     (setq-local compile-command "love . ")
     (add-to-list
      'compilation-error-regexp-alist 'love t)
     (add-to-list
      'compilation-error-regexp-alist-alist
      '(love
        "^Error: Syntax error: \\(.*?\\):\\([0-9]+\\):.*$" 1 2) t))))

;; --------------------------------------------------------- [ Magit ]
(setq magit-last-seen-setup-instructions "1.4.0")

(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode 1)))

;; ---------------------------------------------------------- [ Mail ]
(require 'private-stuff)

(setq mail-from-style               'angles
      mail-personal-alias-file      (conf "mail-aliases")
      message-auto-save-directory   (conf "mail-drafts")
      rmail-delete-after-output     t
      rmail-file-name               "~/mail/inbox"
      rmail-default-file            "~/mail/archive"
      rmail-movemail-variant-in-use 'mailutils
      send-mail-function            'smtpmail-send-it
      smtpmail-smtp-server          "smtp.uio.no"
      smtpmail-smtp-service         587
      gnutls-min-prime-bits         nil)

;; Expand mail-aliases on `next-line' and `end-of-buffer'
(add-hook
 'message-mode-hook
 (lambda ()
   (defvar message-mode-map)
   (define-key message-mode-map [remap next-line]
     'mail-abbrev-next-line)
   (define-key message-mode-map [remap end-of-buffer]
     'mail-abbrev-end-of-buffer)))

(setq rmail-mime-attachment-dirs-alist '((".*" "~/tmp" "~" "/tmp")))

;; Additional ignored headers
(setq rmail-ignored-headers
      (apply
       'concat rmail-ignored-headers
       (mapcar (lambda (s) (concat "\\|^" s ":"))
               '("accept-language"
                 "authentication-results"
                 "content-disposition"
                 "content-language"
                 "content-type"
                 "in-reply-to"
                 "received-spf"
                 "thread-index"))))

;; -------------------------------------------------------- [ Octave ]
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; ----------------------------------------------------------- [ Org ]
;; Org agenda
(setq
 org-agenda-files (conf "org-agenda-files")
 org-agenda-start-on-weekday nil
 org-agenda-include-diary t)

;; Ignore irrelevant holidays
(setq
 holiday-bahai-holidays nil
 holiday-hebrew-holidays nil
 holiday-islamic-holidays nil
 holiday-oriental-holidays nil)

;; Capture templates
(setq
 org-capture-templates
 '(("a" "Agenda" entry (file+headline "~/life.org" "Agenda")
    "** %?" :prepend t)
   ("b" "Bug" entry (file+headline "~/bugs/bugs.org" "Bugs")
    "** TODO %?")))

(add-hook
 'org-mode-hook
 (lambda ()
   (require 'org-bibtex)
   ;; Use Evince for PDF previews
   (defvar org-file-apps)
   (add-to-list 'org-file-apps '("pdf" . "evince %s"))
   (defvar org-latex-pdf-process
     '("latexmk -pdflatex='pdflatex -shell-escape -interaction \
nonstopmode' -pdf -f %f"))))

;; ---------------------------------------------------- [ Projectile ]
(projectile-global-mode)
(setq projectile-mode-line
      '(:eval (format " P[%s]" (projectile-project-name))))

;; -------------------------------------------------------- [ Python ]
(add-hook
 'python-mode-hook
 (lambda ()
   (setq-local fill-column 79)
   (declare-function elpy-use-ipython "elpy")

   (elpy-enable)
   (elpy-use-ipython)

   (add-hook
    'elpy-mode-hook
    (lambda ()
      (highlight-indentation-mode 0)
      (subword-mode 1)))

   (elpy-mode 1)))

;; ----------------------------------------------------------- [ RDF ]
(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)

(dolist (ext '("\\.n3" "\\.ttl"))
  (add-to-list 'auto-mode-alist (cons ext 'ttl-mode)))

(setq ttl-electric-semi-mode nil)

;; -------------------------------------------------------- [ Scheme ]
(setq geiser-active-implementations '(guile)
      geiser-repl-query-on-kill-p nil)

;; -------------------------------------------------------- [ SPARQL ]
(dolist (ext '("\\.sparql" "\\.rq"))
  (add-to-list 'auto-mode-alist (cons ext 'sparql-mode)))

;; ----------------------------------------------------- [ Text mode ]
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))

;; ------------------------------------------------------ [ Web mode ]
(dolist (ext '("\\.html" "\\.hbs"))
  (add-to-list 'auto-mode-alist (cons ext 'web-mode)))

(setq
 web-mode-engines-alist '(("django" . "\\.html"))
 web-mode-enable-auto-pairing nil
 web-mode-markup-indent-offset 2)

(add-hook
 'web-mode-hook
 (lambda ()
   (defvar web-mode-django-control-blocks)
   (defvar web-mode-django-control-blocks-regexp)
   (push "editable" web-mode-django-control-blocks)
   (push "endeditable" web-mode-django-control-blocks)
   (setq web-mode-django-control-blocks-regexp
         (regexp-opt web-mode-django-control-blocks t))

   (rainbow-mode 1)
   (subword-mode 1)
   (diminish 'rainbow-mode)))

;;--------------------------------------------------------- [ Custom ]
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "#888a85"))))
 '(erc-my-nick-face ((t (:foreground "#888a85" :weight bold))))
 '(magit-blame-header ((t (:inherit magit-diff-file-header))))
 '(slime-repl-inputed-output-face ((t (:foreground "#729fcf"))) t))
