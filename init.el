;; init.el --- Personal Emacs configurations
;; Author: Simen Heggestøyl <simenheg@ifi.uio.no>

(set-language-environment "UTF-8")
(package-initialize)

(setq-default
 indent-tabs-mode             nil  ; Use spaces for indentation
 major-mode            'text-mode  ; text-mode as the default for new buffers
 split-width-threshold        100  ; Prefer vertical window splits
 fill-column                   76) ; Lines break at column 76

(setq
 comint-input-ignoredups        t  ; Ignore duplicates in Comint history
 compare-ignore-whitespace      t  ; Ignore whitespace differences
 default-input-method       "TeX"  ; TeX is the default toggled input method
 disabled-command-function    nil  ; Enable disabled commands
 display-time-24hr-format       t  ; 24 hour time format
 eshell-hist-ignoredups         t  ; Ignore duplicates in Eshell history
 history-delete-duplicates      t  ; Delete duplicate history entries
 inhibit-startup-screen         t  ; No startup screen
 initial-scratch-message      nil  ; No scratch message
 iswitchb-prompt-newbuffer    nil  ; Create new buffers without confirmation
 kill-read-only-ok              t  ; Killing read-only text is OK
 password-cache-expiry        nil  ; Cache TRAMP passwords forever
 show-paren-delay               0) ; Don't delay the paren update

(blink-cursor-mode              0) ; No blinking cursor
(column-number-mode             1) ; Show column number
(fset 'yes-or-no-p      'y-or-n-p) ; Make "yes/no" prompts "y/n"
(global-auto-revert-mode        1) ; Reload files after modification
(iswitchb-mode                  1) ; Neat buffer switching
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

;; Global keybinds
(global-set-key (kbd "C-+")     '(lambda () (interactive) (inc-next-number -1)))
(global-set-key (kbd "C-=")     'inc-next-number)
(global-set-key (kbd "C-a")     'beginning-of-indentation-or-line)
(global-set-key (kbd "C-c C-c") 'recompile)
(global-set-key (kbd "C-c C-f") 'browse-url-firefox)
(global-set-key (kbd "C-c M-$") 'ispell-change-dictionary)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c T")   'google-translate-query-translate)
(global-set-key (kbd "C-c a")   'org-agenda-list)
(global-set-key (kbd "C-c b")   'org-iswitchb)
(global-set-key (kbd "C-c c")   'org-capture)
(global-set-key (kbd "C-c l")   'org-store-link)
(global-set-key (kbd "C-c m")   'rmail)
(global-set-key (kbd "C-c p")   'list-packages)
(global-set-key (kbd "C-c t")   'google-translate-at-point)
(global-set-key (kbd "C-c v")   'magit-status)
(global-set-key (kbd "C-c w")   'compare-windows)
(global-set-key (kbd "C-w")     'kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-z")     'bury-buffer)
(global-set-key [C-M-backspace] 'backward-kill-sexp)
(global-set-key [f5]            '(lambda () (interactive) (revert-buffer nil t)))

;; ------------------------------------------------------------ [ Assembler ]
(setq asm-comment-char ?#) ; '#' as comment char (default is ';')

;; -------------------------------------------------------------------- [ C ]
(add-hook
 'c-mode-common-hook
 (lambda ()
   (auto-fill-mode 1)

   (local-set-key (kbd "C-c i") 'c-include-header)
   (local-set-key (kbd "C-c h") 'c-find-corresponding-file)

   ;; Try to create a sensible compilation string when no Makefile is found
   (unless (file-exists-p "Makefile")
     (set (make-local-variable 'compile-command)
          (let ((file (file-name-nondirectory buffer-file-name)))
            (format "%s %s %s -o %s"
                    (or (getenv "CC") "gcc")
                    file
                    (or (getenv "CFLAGS") "-std=c99 -Wall")
                    (file-name-sans-extension file)))))

   ;; Try to create a sensible compilation string when a Makefile is found
   ;; in the parent directory
   (when (file-exists-p "../Makefile")
     (set (make-local-variable 'compile-command) "make devel -C .."))))

;; ------------------------------------------------------------- [ Calendar ]
(setq calendar-location-name  "Oslo, Norway"
      calendar-latitude       60.0
      calendar-longitude      10.7
      calendar-week-start-day 1) ; Weeks start on monday

;; Display time in 24 hour format
(setq calendar-time-display-form
      '(24-hours "." minutes
                 (if time-zone " (") time-zone (if time-zone ")")))

;; ---------------------------------------------------------- [ Color-theme ]
(load-theme 'tangotango t)

;; ---------------------------------------------------------- [ Common Lisp ]
(setq inferior-lisp-program "sbcl")
(setq slime-lisp-implementations
      `((sbcl ("sbcl" "--core" ,(conf "sbcl.core-for-slime")))))

(declare-function slime-connected-p "slime")
(declare-function slime "slime")
(add-hook 'slime-mode-hook
          (lambda ()
            (unless (slime-connected-p)
              (save-excursion (slime)))))

;; More sensible `loop' indentation
(setq lisp-loop-forms-indentation   2
      lisp-simple-loop-indentation  2
      lisp-loop-keyword-indentation 6)

;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing
;; over a ')'
(defun override-slime-repl-bindings-with-paredit ()
  (defvar slime-repl-mode-map)
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; ---------------------------------------------------------------- [ Dired ]
;; Make file sizes human-readable, and hide time stamps
(setq-default dired-listing-switches "-alh --time-style=+")

(setq
 dired-dwim-target t                   ; Let Dired guess target directory
 dired-recursive-copies 'always        ; Don't ask, just copy
 global-auto-revert-non-file-buffers t ; Auto-refresh the file list
 image-dired-show-all-from-dir-max-files 500)

;; -------------------------------------------------------------- [ DocView ]
;; Auto-update document files
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(setq doc-view-continuous        t ; Smooth document viewing
      tex-dvi-view-command "xdvi") ; DVI-reader of choice

;; ---------------------------------------------------------------- [ Eldoc ]
;; Show function arglist or variable docstring in echo area
(require 'eldoc)

;; ---------------------------------------------------------------- [ Emacs ]
;; Fix mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; One line at a time
(setq mouse-wheel-progressive-speed nil) ; Don't accelerate scrolling

;; Don't bother unless the file *really* is large (50 MB+)
(setq large-file-warning-threshold 50000000)

;; Set current buffer as the window name
(setq-default frame-title-format (list "%b %f"))

;; Put #autosave# and backup~ files into own directory
(let ((autosave-directory (conf "autosaves/")))
  (setq backup-directory-alist         `((".*" . ,autosave-directory))
        auto-save-file-name-transforms `((".*"   ,autosave-directory t))))

;; ----------------------------------------------------------- [ Emacs Lisp ]
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-b") 'eval-buffer)
   (eldoc-mode)))

(add-hook 'after-save-hook 'auto-byte-recompile)

;; ------------------------------------------------------------------ [ ERC ]
(setq erc-fill-column fill-column)

;; -------------------------------------------------------------- [ Flymake ]
(setq help-at-pt-display-when-idle t ; Activate echoed help messages
      help-at-pt-set-timer        0) ; Echo help instantly

;; ---------------------------------------------------------------- [ Forth ]
(add-to-list 'auto-mode-alist '("\\.fs\\'" . forth-mode))

;; ----------------------------------------------------- [ Google Translate ]
(autoload 'google-translate-at-point "google-translate" nil t)
(autoload 'google-query-translate "google-translate" nil t)

(setq google-translate-default-source-language "no"
      google-translate-default-target-language "en")

;; -------------------------------------------------------------- [ Haskell ]
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ----------------------------------------------------------- [ JavaScript ]
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ----------------------------------------------------------------- [ Lisp ]
(require 'paredit)

(autoload 'let-fix "autolet" "Automatic let-form fixer" t)

;; Ultimate Lisp eye candy: display lambda as λ
(defun sm-lambda-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(dolist (h '(emacs-lisp-mode-hook
             geiser-repl-mode-hook
             ielm-mode-hook
             inferior-lisp-mode-hook
             inferior-scheme-mode-hook
             lisp-interaction-mode-hook
             lisp-mode-hook
             scheme-mode-hook
             slime-repl-mode-hook))
  (add-hook h (lambda () (paredit-mode 1)))
  (add-hook h 'sm-lambda-mode-hook)
  (add-hook h (lambda () (auto-fill-mode 1))))

;; ----------------------------------------------------------------- [ Mail ]
(require 'message)
(require 'nnheader)
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
      message-default-headers       "FCC: ~/mail/sent")

;; Expand mail-aliases on `next-line' and `end-of-buffer'
(add-hook
 'message-mode-hook
 (lambda ()
   (define-key
     message-mode-map [remap next-line] 'mail-abbrev-next-line)
   (define-key
     message-mode-map [remap end-of-buffer] 'mail-abbrev-end-of-buffer)))

(setq rmail-mime-attachment-dirs-alist '((".*" "~/tmp" "~" "/tmp")))

(setq rmail-ignored-headers
      (apply
       'concat rmail-ignored-headers
       (mapcar (lambda (s) (concat "\\|^" s ":")) ; Additional ignored headers
               '("accept-language"
                 "authentication-results"
                 "content-disposition"
                 "content-language"
                 "content-type"
                 "in-reply-to"
                 "received-spf"
                 "thread-index"))))

;; --------------------------------------------------------------- [ Octave ]
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; ------------------------------------------------------------------ [ OWL ]
(autoload 'omn-mode "omn-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.owl$" . omn-mode))

;; ------------------------------------------------------------------ [ Org ]
(setq
 org-agenda-files '("~/life.org")
 org-agenda-start-on-weekday nil
 org-agenda-include-diary t)

;; Ignore irrelevant holidays
(setq
 holiday-bahai-holidays nil
 holiday-hebrew-holidays nil
 holiday-islamic-holidays nil
 holiday-oriental-holidays nil)

;; Add some relevant holidays
(setq
 holiday-other-holidays
 '((holiday-fixed 5 1 "International Workers' Day")
   (holiday-fixed 5 17 "Norwegian Constitution Day")))

;; PDFs visited in Org-mode are opened in Evince (and other file extensions
;; are handled according to the defaults)
(add-hook
 'org-mode-hook
 '(lambda ()
    (setq org-file-apps
          '((auto-mode . emacs)
            ("\\.mm\\'" . default)
            ("\\.x?html?\\'" . default)
            ("\\.pdf\\'" . "evince %s")))))

;; ------------------------------------------------------------- [ Packages ]
;; Extra package repositories
(setq
 package-archives
 '(("gnu"       . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("org"       . "http://orgmode.org/elpa/")
   ("MELPA"     . "http://melpa.milkbox.net/packages/")))

;; ----------------------------------------------------------------- [ Perl ]
(defalias 'perl-mode 'cperl-mode)

;; ------------------------------------------------------------------ [ RDF ]
(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)

(dolist (ext '("\\.n3" "\\.ttl"))
  (add-to-list 'auto-mode-alist (cons ext 'ttl-mode)))

(setq ttl-electric-semi-mode nil)

;; --------------------------------------------------------------- [ Scheme ]
(setq geiser-active-implementations '(guile)
      geiser-repl-query-on-kill-p nil)

;; --------------------------------------------------------------- [ Simula ]
(setq simula-tab-always-indent t) ; Always indent, never insert tabs

;; ------------------------------------------------------------ [ Skeletons ]
(auto-insert-mode 1) ; Auto insert templates into new files ...
(setq auto-insert-query nil) ; ... but don't ask for my permission every time
(setq auto-insert-alist nil) ; Remove predefined skeletons

(require 'skeletons)
(define-auto-insert "\\.c\\'" 'skeleton-c)
(define-auto-insert "\\.java\\'" 'skeleton-java)
(define-auto-insert "\\.tex\\'" 'skeleton-latex)

;; --------------------------------------------------------------- [ SPARQL ]
(dolist (ext '("\\.sparql" "\\.rq"))
  (add-to-list 'auto-mode-alist (cons ext 'sparql-mode)))

;; ------------------------------------------------------------ [ Text mode ]
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))

;; ------------------------------------------------------------- [ Uniquify ]
;; Unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ------------------------------------------------------------- [ Diminish ]
(dolist (mode '(abbrev-mode auto-fill-function eldoc-mode paredit-mode))
  (diminish mode))

;;---------------------------------------------------------------- [ Custom ]
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "#888a85"))) t)
 '(erc-my-nick-face ((t (:foreground "#888a85" :weight bold))) t)
 '(slime-repl-inputed-output-face ((t (:foreground "#729fcf")))))
