;; init.el --- Personal Emacs configurations
;; Author: Simen Heggestøyl <simenheg@gmail.com>

(require 'package)

(setq
 package-archives
 `(("GNU" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("MELPA Stable" . "http://stable.melpa.org/packages/")
   ("MELPA" . "http://melpa.org/packages/")
   ("magit-1" . "http://magit.vc/elpa/v1/packages/")
   ("local" . ,(locate-user-emacs-file "lisp/packages/"))))

(setq
 package-archive-priorities
 '(("local" . 4)
   ("GNU" . 3)
   ("MELPA Stable" . 2)
   ("marmalade" . 2)
   ("MELPA" . 1)))

(setq
 package-selected-packages
 '(anaconda-mode
   avy
   bbdb
   company
   company-anaconda
   company-quickhelp
   csv-mode
   cycle-quotes
   debbugs
   editorconfig
   elpy
   exec-path-from-shell
   flycheck
   focus
   fuel
   geiser
   gnuplot
   google-translate
   ivy
   js2-mode
   json-mode
   know-your-http-well
   lorem-ipsum
   lua-mode
   magit
   markdown-mode
   multiple-cursors
   nodejs-repl
   norwegian-holidays
   paredit
   php-mode
   projectile
   rdf-prefix
   request
   restclient
   restclient-test
   slime
   smex
   sql-indent
   undo-tree
   web-mode
   wgrep
   wgrep-ag
   xref-js2
   yaml-mode))

(setq package-pinned-packages
      '((json-mode . "GNU")
        (magit . "magit-1")
        (slime . "MELPA")))

(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; Install missing packages
(package-install-selected-packages)

(set-language-environment "UTF-8")
(exec-path-from-shell-initialize)

(setq-default
 indent-tabs-mode             nil  ; Use spaces for indentation
 major-mode            'text-mode) ; Text mode as default mode

(setq
 auto-revert-verbose          nil  ; Be quiet about reverts
 comint-input-ignoredups        t  ; Ignore duplicate history
 compare-ignore-whitespace      t  ; Ignore whitespace differences
 default-input-method       "TeX"  ; TeX as default input method
 disabled-command-function    nil  ; Enable disabled commands
 display-time-24hr-format       t  ; 24 hour time format
 eshell-hist-ignoredups         t  ; Ignore duplicate history
 eshell-history-size        10000  ; Lengthen Eshell history
 gc-cons-threshold       20000000  ; Run GC only every 20 MB
 history-delete-duplicates      t  ; Delete duplicate history entries
 inhibit-startup-screen         t  ; No startup screen
 initial-scratch-message      nil  ; No scratch message
 kill-read-only-ok              t  ; Killing read-only text is OK
 password-cache-expiry        nil  ; Cache TRAMP passwords forever
 sentence-end-double-space    nil  ; Fill with single spaces
 show-paren-delay               0) ; Don't delay the paren update

(blink-cursor-mode              0) ; No blinking cursor
(column-number-mode             1) ; Show column number
(editorconfig-mode              1) ; Respect EditorConfig files
(fset 'yes-or-no-p      'y-or-n-p) ; Make "yes/no" prompts "y/n"
(global-auto-revert-mode        1) ; Reload files after modification
(global-prettify-symbols-mode   1) ; Pretty symbols (e.g. lambda => λ)
(global-subword-mode            1) ; Better editing of camelCasedWords
(prefer-coding-system      'utf-8) ; Always prefer UTF-8
(show-paren-mode                1) ; Highlight matching parenthesis
(yas-global-mode                1) ; YASnippet everywhere

;; Add lisp/ and all subdirectories to the load-path
(let ((default-directory (locate-user-emacs-file "lisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Miscellaneous utility functions
(require 'defuns)

(global-set-key (kbd "C-'")     'org-cycle-agenda-files)
(global-set-key (kbd "C-+")     'dec-next-number)
(global-set-key (kbd "C-=")     'inc-next-number)
(global-set-key (kbd "C-M-y")   'counsel-yank-pop)
(global-set-key (kbd "C-S-k")   'kill-whole-line)
(global-set-key (kbd "C-\"")    'cycle-quotes)
(global-set-key (kbd "C-a")     'beginning-of-indentation-or-line)
(global-set-key (kbd "C-c C-'") 'electric-quote-mode)
(global-set-key (kbd "C-c M-$") 'ispell-change-dictionary)
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)
(global-set-key (kbd "C-c a")   'org-agenda)
(global-set-key (kbd "C-c c")   'org-capture)
(global-set-key (kbd "C-c d")   'duplicate)
(global-set-key (kbd "C-c e")   'mc/edit-lines)
(global-set-key (kbd "C-c f")   'find-grep)
(global-set-key (kbd "C-c l")   'mc/mark-all-like-this)
(global-set-key (kbd "C-c m")   'gnus)
(global-set-key (kbd "C-c n")   'mc/mark-next-like-this)
(global-set-key (kbd "C-c r")   'rename-buffer)
(global-set-key (kbd "C-c t")   'google-translate-at-point)
(global-set-key (kbd "C-c v")   'magit-status)
(global-set-key (kbd "C-c w")   'compare-windows)
(global-set-key (kbd "C-j")     'newline)
(global-set-key (kbd "C-w")     'kill-region-or-backward-kill-sexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b")   'ivy-switch-buffer)
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-z")     'bury-buffer)
(global-set-key (kbd "M-i")     'counsel-imenu)
(global-set-key (kbd "M-s l")   'sort-lines)
(global-set-key (kbd "M-x")     'counsel-M-x)
(global-set-key [C-M-backspace] 'backward-kill-sexp)
(global-set-key [M-down]        'move-line-down)
(global-set-key [M-up]          'move-line-up)

;; ------------------------------------------------------- [ Arduino ]
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))

;; ----------------------------------------------------------- [ Avy ]
(setq avy-background t)

;; ---------------------------------------------------------- [ BBDB ]
(setq bbdb-file "~/sync/bbdb")
(setq bbdb-phone-style nil)
(setq bbdb-mua-pop-up nil)
(setq bbdb-completion-display-record nil)

;; -------------------------------------------------------- [ BibTeX ]
(setq-default bibtex-dialect 'biblatex)

;; ------------------------------------------------------------- [ C ]
(add-hook
 'c-mode-common-hook
 (lambda ()
   (auto-fill-mode 1)
   (local-set-key (kbd "C-c h") 'ff-find-other-file)

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

;; ----------------------------------------------- [ Clean Mode Line ]
(defvar clean-mode-line-alist
  '((anaconda-mode . "")
    (auto-fill-function . "")
    (company-mode . "")
    (counsel-mode . "")
    (editorconfig-mode . "")
    (eldoc-mode . "")
    (elpy-mode . "")
    (emacs-lisp-mode . "el")
    (js2-mode "js2")
    (magit-auto-revert-mode . "")
    (paredit-mode . "")
    (projectile-mode . "")
    (python-mode . "🐍")
    (subword-mode . "")
    (yas-minor-mode . "")))

(add-hook
 'after-change-major-mode-hook
 (lambda () (clean-mode-line clean-mode-line-alist)))

;; --------------------------------------------------- [ Color-theme ]
(load-theme 'leuven t)

;; --------------------------------------------------- [ Common Lisp ]
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-repl))
(setq slime-repl-history-size 1000)

;; More sensible `loop' indentation
(setq lisp-loop-forms-indentation   2
      lisp-simple-loop-indentation  2
      lisp-loop-keyword-indentation 6)

(add-hook
 'slime-repl-mode-hook
 (lambda ()
   (defvar slime-repl-mode-map)
   ;; Stop SLIME's REPL from grabbing DEL, which is annoying when
   ;; backspacing over a ')'
   (define-key slime-repl-mode-map
     (read-kbd-macro paredit-backward-delete-key) nil)

   ;; Font Lock mode messes with SLIME's REPL for some reason.
   (font-lock-mode -1)
   (auto-fill-mode -1)))

;; ------------------------------------------------------- [ Company ]
(require 'company)

(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.1)

(add-hook 'prog-mode-hook #'company-mode-on)

;; ----------------------------------------------------------- [ CSV ]
(add-hook
 'csv-mode-hook
 (lambda ()
   (csv-align-mode 1)
   (hl-line-mode)))

;; --------------------------------------------------------- [ Dired ]
(add-hook
 'dired-load-hook
 (lambda ()
   (defvar dired-mode-map)

   (load "dired-x")

   (local-set-key (kbd "C-c C-w") 'wdired-change-to-wdired-mode)

   ;; -h for human-readable file sizes, -v for natural sort.
   (setq dired-listing-switches "-alhv")

   (define-key dired-mode-map
     (vector 'remap 'dired-do-print) 'previous-line)

   (define-key dired-mode-map
     (vector 'remap 'end-of-buffer) 'dired-end-of-buffer)

   (define-key dired-mode-map
     (vector 'remap 'beginning-of-buffer) 'dired-beginning-of-buffer)

   (define-key dired-mode-map
     (vector 'remap 'dired-goto-file) 'find-file)))

(setq
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
(let ((autosave-directory (locate-user-emacs-file "autosaves/")))
  (setq backup-directory-alist `((".*" . ,autosave-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,autosave-directory t))))

;; Clean up trailing whitespace before saving, except at the current
;; line
(add-hook
 'before-save-hook
 (lambda ()
   (unless (eq major-mode 'diff-mode)
     (let ((col (current-column)))
       (delete-trailing-whitespace)
       (indent-to-column col)))))

;; ---------------------------------------------------- [ Emacs Lisp ]
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq sentence-end-double-space t)))

;; ----------------------------------------------------------- [ ERC ]
(setq erc-fill-column fill-column)

;; ----------------------------------------------- [ Eshell commands ]
(define-eshell-command django-server
  "*server*" "./manage.py runserver 0.0.0.0:8000")

(define-eshell-command npm-watch
  "*npm-watch*" "npm run watch")

(define-eshell-command yarn-watch
  "*yarn-watch*" "yarn run watch")

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

;; ----------------------------------------------------------- [ Ivy ]
(require 'counsel)
(ivy-configure 'counsel-M-x :initial-input "")

(defun counsel-ag-projectile
    (&optional initial-input initial-directory extra-ag-args
               ag-prompt)
  (interactive)
  (counsel-ag initial-input (projectile-project-root)
              extra-ag-args ag-prompt))

;; ---------------------------------------------------------- [ Java ]
(add-hook
 'java-mode
 (lambda ()
   (setq-local
    c-offsets-alist
    (append
     '((arglist-intro . +)
       (arglist-cont . 0)
       (arglist-close . 0))
     c-offsets-alist))))

;; ---------------------------------------------------- [ JavaScript ]
(add-hook
 'js2-mode-hook
 (lambda ()
   (setq-local fill-column 79)
   (local-set-key (kbd "C-j") 'js2-line-break)
   (local-set-key (kbd "RET") 'js2-line-break)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.webapp\\'" . json-mode))

;; ---------------------------------------------------------- [ JSON ]
(require 'json-mode)

(dolist (filename '(".arcconfig" ".arclint" ".babelrc" ".bowerrc"
                    ".eslintrc"))
  (add-to-list 'auto-mode-alist (cons filename 'json-mode)))

;; ---------------------------------------------------------- [ Lisp ]
(autoload 'let-fix "autolet" "Automatic let-form fixer" t)

(defun setup-lisp ()
  (auto-fill-mode 1)
  (paredit-mode 1)
  (setq-local
   prettify-symbols-alist
   '(("lambda" . ?λ)
     ("/=" . ?≠)
     ("<=" . ?≤)
     (">=" . ?≥))))

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
        "^Error: Syntax error: \\(.*?\\):\\([0-9]+\\):.*$" 1 2)
      t))))

;; --------------------------------------------------------- [ Magit ]
(setq magit-last-seen-setup-instructions "1.4.0")

(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode 1)))

;; ---------------------------------------------------------- [ Mail ]
(require 'private-stuff nil t)

(setq mail-from-style               'angles
      rmail-delete-after-output     t
      rmail-file-name               "~/mail/inbox"
      rmail-default-file            "~/mail/archive"
      rmail-movemail-variant-in-use 'mailutils
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
     'mail-abbrev-end-of-buffer)
   (flyspell-mode 1)))

(setq rmail-mime-attachment-dirs-alist '((".*" "~/tmp" "~" "/tmp")))

;; Additional ignored headers
(setq rmail-ignored-headers
      (apply
       'concat rmail-ignored-headers
       (mapcar (lambda (s) (concat "\\|^" s ":"))
               '("IronPort-PHdr"
                 "accept-language"
                 "authentication-results"
                 "content-disposition"
                 "content-language"
                 "content-type"
                 "in-reply-to"
                 "msip_labels"
                 "received-spf"
                 "thread-index"))))

;; ------------------------------------------------------ [ Markdown ]
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(declare-function markdown-export-and-preview "markdown-mode")
(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-e") #'markdown-export-and-preview)))

;; ------------------------------------------------------ [ Midnight ]
(midnight-mode 1)

;; -------------------------------------------------------- [ Octave ]
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; ----------------------------------------------------------- [ Org ]
;; Org agenda
(setq
 org-agenda-files (locate-user-emacs-file "org-agenda-files")
 org-agenda-start-on-weekday nil
 org-agenda-include-diary t
 org-clock-clocked-in-display nil)

;; Ignore irrelevant holidays
(setq
 holiday-bahai-holidays nil
 holiday-hebrew-holidays nil
 holiday-islamic-holidays nil
 holiday-oriental-holidays nil)

;; Capture templates
(setq
 org-capture-templates
 '(("a" "Agenda" entry (file+headline "~/sync/life.org" "Agenda")
    "** %?\n" :prepend t)
   ("b" "Bug" entry (file+headline "~/sync/bugs/bugs.org" "Bugs")
    "** TODO %?")
   ("m" "Music" entry (file+headline "~/sync/music.org" "Music")
    "** TODO %?")))

(add-hook
 'org-mode-hook
 (lambda ()
   (defvar org-file-apps)
   ;; Use Evince for PDF previews
   (add-to-list 'org-file-apps '("pdf" . "evince %s"))
   (defvar org-latex-pdf-process
     '("latexmk -pdflatex='pdflatex -shell-escape -interaction \
nonstopmode' -pdf -f %f"))))

;; ------------------------------------------------------ [ Packages ]
(setq package-menu-hide-low-priority t)

;; ---------------------------------------------------- [ Projectile ]
(projectile-global-mode)

(setq projectile-completion-system 'ivy)

(define-key projectile-mode-map (kbd "C-c p")
  'projectile-command-map)

(define-key projectile-command-map (kbd "s")
  'counsel-ag-projectile)

;; -------------------------------------------------------- [ Python ]

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(add-hook
 'python-mode-hook
 (lambda ()
   (setq-local
    prettify-symbols-alist
    '(("!=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)))))

(add-hook
 'anaconda-mode-hook
 (lambda ()
   (defvar python-fill-docstring-style)
   (setq-local fill-column 79)
   (setq-local python-fill-docstring-style 'pep-257-nn)
   (setq-local anaconda-mode-installation-directory
               (locate-user-emacs-file "anaconda-mode"))

   (flycheck-mode 1)

   (setq-local
    prettify-symbols-alist
    '(("!=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)))

   (require 'elpy)
   (declare-function elpy-test-pytest-runner "elpy")
   (local-set-key (kbd "C-c C-t") #'elpy-test-pytest-runner)))

;; --------------------------------------------------- [ REST Client ]
(add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))

(add-hook
 'restclient-mode-hook
 (lambda ()
   (restclient-test-mode 1)
   (local-set-key
    (kbd "C-c C-c") 'restclient-http-send-current-stay-in-window)))

;; -------------------------------------------------------- [ Scheme ]
(setq geiser-active-implementations '(guile)
      geiser-repl-query-on-kill-p nil)

;; ----------------------------------------------------------- [ SQL ]
(setq-default sql-product 'postgres)

;; ----------------------------------------------------- [ Text mode ]
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))

;; ----------------------------------------------------- [ Timeclock ]
(require 'timeclock)

;; Don't ask for a reason when clocking out
(setq timeclock-get-reason-function nil)

(define-key ctl-x-map "ti" 'timeclock-in)
(define-key ctl-x-map "to" 'timeclock-out)

;; ------------------------------------------------------------ [ VC ]
(remove-hook 'log-edit-hook #'log-edit-insert-message-template)
(add-hook 'log-edit-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'log-view-mode-hook (lambda () (hl-line-mode 1)))

;; ------------------------------------------------------ [ Web mode ]
(dolist (ext '("\\.html" "\\.jinja"))
  (add-to-list 'auto-mode-alist (cons ext 'web-mode)))

(setq
 web-mode-enable-auto-indentation nil
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
   (local-set-key
    (kbd "C-c C-o")
    (lambda ()
      (interactive)
      (let ((sgml-tag-alist html-tag-alist))
        (sgml-tag))))))

;;--------------------------------------------------------- [ Custom ]
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t nil)))
 '(avy-lead-face ((t (:background "#fbe448" :foreground "black"))))
 '(erc-default-face ((t (:family "Sans Serif"))))
 '(erc-input-face ((t (:foreground "#888a85" :family "Sans Serif"))))
 '(erc-my-nick-face ((t (:foreground "#888a85" :weight bold))))
 '(erc-nick-default-face ((t (:weight bold :family "Sans Serif"))))
 '(erc-notice-face ((t (:foreground "SlateBlue" :weight bold :height 0.8))))
 '(erc-timestamp-face ((t (:foreground "pale green" :weight bold))))
 '(flymake-error ((t (:underline (:color "salmon1" :style wave)))))
 '(geiser-font-lock-repl-prompt ((t (:inherit comint-highlight-prompt))))
 '(git-commit-summary-face ((t (:foreground "#000000"))))
 '(ivy-current-match ((t (:background "#FFF876" :foreground "black"))))
 '(ivy-minibuffer-match-face-1 ((t nil)))
 '(ivy-minibuffer-match-face-2 ((t (:background "#FBE448" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "#FBE448" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#FBE448" :weight bold))))
 '(magit-blame-header ((t (:inherit magit-diff-file-header))))
 '(magit-diff-file-header ((t (:background "#ffeeff" :foreground "#4183C4" :weight bold :height 1.1 :family "Sans Serif"))))
 '(magit-log-author ((t (:foreground "firebrick"))))
 '(magit-log-message ((t nil)))
 '(nxml-element-local-name ((t (:background "white smoke" :foreground "SteelBlue" :box (:line-width 1 :color "light gray")))))
 '(org-document-title ((t (:foreground "black" :weight bold :height 1.2 :family "Sans Serif"))))
 '(slime-repl-inputed-output-face ((t (:foreground "#729fcf"))) t))
