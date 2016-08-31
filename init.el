;; init.el --- Personal Emacs configurations
;; Author: Simen Heggestøyl <simenheg@gmail.com>

(defun conf-path (path)
  "Return the absolute path of config file/directory PATH."
  (expand-file-name (concat user-emacs-directory path)))

(setq
 package-archives
 `(("GNU" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("MELPA Stable" . "http://stable.melpa.org/packages/")
   ("MELPA" . "http://melpa.org/packages/")
   ("magit-1" . "http://magit.vc/elpa/v1/packages/")
   ("local" . ,(conf-path "lisp/packages/"))))

(setq
 package-archive-priorities
 '(("local" . 4)
   ("GNU" . 3)
   ("MELPA Stable" . 2)
   ("marmalade" . 2)
   ("MELPA" . 1)))

(defvar package-selected-packages
  '(avy
    cider
    cycle-quotes
    debbugs
    editorconfig
    elpy
    exec-path-from-shell
    focus
    fuel
    geiser
    google-translate
    helm
    helm-ag
    helm-projectile
    ido-vertical-mode
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
    projectile
    rainbow-mode
    rdf-prefix
    restclient
    restclient-test
    smex
    undo-tree
    web-mode
    wgrep
    wgrep-ag
    xref-js2
    yaml-mode))

(setq package-pinned-packages
      '((magit . "magit-1")))

(package-initialize)

(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; Install missing packages
(package-install-selected-packages)

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
 eshell-history-size         1000  ; Lengthen Eshell history
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
(electric-quote-mode            1) ; Easier “quote” typing
(fset 'yes-or-no-p      'y-or-n-p) ; Make "yes/no" prompts "y/n"
(global-auto-revert-mode        1) ; Reload files after modification
(global-prettify-symbols-mode   1) ; Pretty symbols (e.g. lambda => λ)
(global-subword-mode            1) ; Better editing of camelCasedWords
(ido-vertical-mode              1) ; Display ido-mode vertically
(menu-bar-mode                 -1) ; No menu bar
(prefer-coding-system      'utf-8) ; Always prefer UTF-8
(scroll-bar-mode               -1) ; No scroll bar
(show-paren-mode                1) ; Highlight matching parenthesis
(tool-bar-mode                 -1) ; No tool bar

;; Add lisp/ and all subdirectories to the load-path
(let ((default-directory (conf-path "lisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Miscellaneous utility functions
(require 'defuns)

(global-set-key (kbd "C-'")     'org-cycle-agenda-files)
(global-set-key (kbd "C-+")     'dec-next-number)
(global-set-key (kbd "C-=")     'inc-next-number)
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
(global-set-key (kbd "M-o")     'other-window)
(global-set-key (kbd "M-x")     'counsel-M-x)
(global-set-key [C-M-backspace] 'backward-kill-sexp)
(global-set-key [C-tab]         'tidy-buffer)
(global-set-key [M-down]        'move-line-down)
(global-set-key [M-up]          'move-line-up)

;; ------------------------------------------------------- [ Arduino ]
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))

;; ----------------------------------------------------------- [ Avy ]
(setq avy-background t)

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

;; ----------------------------------------------- [ Clean Mode Line ]
(defvar clean-mode-line-alist
  '((auto-fill-function . "")
    (company-mode . "")
    (dired-omit-mode . "")
    (elpy-mode . "")
    (emacs-lisp-mode . "Elisp")
    (js2-mode "js2")
    (magit-auto-revert-mode . "")
    (paredit-mode . "")
    (python-mode . "Py")
    (rainbow-mode . "")
    (subword-mode . "")
    (undo-tree-mode "")
    (yas-minor-mode . "")))

(add-hook
 'after-change-major-mode-hook
 (lambda () (clean-mode-line clean-mode-line-alist)))

;; ------------------------------------------------------- [ Clojure ]
(require 'clojure-mode)

;; Extra macro indentation directives
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2)
  (render 1))

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

;; ------------------------------------------------------- [ Company ]
(require 'company)

(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.1)

(add-hook 'prog-mode-hook #'company-mode-on)

;; -------------------------------------------------------- [ (S)CSS ]
(require 'css-lookup)

(add-hook
 'css-mode-hook
 (lambda ()
   (rainbow-mode 1)
   (yas-minor-mode 1)))

;; --------------------------------------------------------- [ Dired ]
(add-hook
 'dired-load-hook
 (lambda ()
   (defvar dired-mode-map)

   (load "dired-x")

   (local-set-key (kbd "C-c C-w") 'wdired-change-to-wdired-mode)

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
   (dired-omit-mode 1)))

;; Make file sizes human-readable, and hide time stamps
(setq-default dired-listing-switches "-alh --time-style=+")

(setq
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
(let ((autosave-directory (conf-path "autosaves/")))
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
   (setq sentence-end-double-space t)
   (setq-local
    prettify-symbols-alist
    '(("lambda" . ?λ)
      ("/=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)))
   (setq-local fill-column 72)))

(add-hook 'after-save-hook 'auto-byte-recompile)

;; ----------------------------------------------------------- [ ERC ]
(setq erc-fill-column fill-column)

;; ----------------------------------------------- [ Eshell commands ]
(define-eshell-command grunt
  "*grunt*" "grunt")

(define-eshell-command django-server
  "*server*" "./manage.py runserver 0.0.0.0:8000")

(define-eshell-command ember-server
  "*ember-server*" "ember serve")

(define-eshell-command admin-server
  "*admin-server*" "ember serve")

(define-eshell-command quiz-server
  "*quiz-server*" "ember serve")

(define-eshell-command arc-diff
  "*arc*" "arc diff HEAD^")

(define-eshell-command arc-diff-qw
  "*arc*" "./arc-diff HEAD^")

(define-eshell-command arc-land
  "*arc*" "arc land")

(define-eshell-command maildev
  "*maildev*" "maildev")

;; Workaround for bug #21417, can be removed once it's resolved
(add-hook
 'eshell-mode-hook
 (lambda () (setq-local paragraph-separate "workaround-for-bug")))

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
(require 'ivy)
(push '(counsel-M-x . "") ivy-initial-inputs-alist)

;; ---------------------------------------------------- [ JavaScript ]
(setq inferior-js-program-command "nodejs")

(add-hook
 'js2-mode-hook
 (lambda ()
   (setq-local fill-column 79)
   (local-set-key (kbd "C-j") 'js2-line-break)
   (local-set-key (kbd "RET") 'js2-line-break)
   (yas-minor-mode 1)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.webapp\\'" . json-mode))

(setq js2-strict-trailing-comma-warning nil)

;; ---------------------------------------------------------- [ JSON ]
(require 'json-mode)

(dolist (filename '(".arcconfig" ".bowerrc" ".eslintrc"))
  (add-to-list 'auto-mode-alist (cons filename 'json-mode)))

;; ---------------------------------------------------------- [ Lisp ]
(autoload 'let-fix "autolet" "Automatic let-form fixer" t)

(defun setup-lisp ()
  (auto-fill-mode 1)
  (paredit-mode 1))

(mapc
 (lambda (m) (add-hook m 'setup-lisp))
 '(cider-repl-mode
   clojure-mode
   clojurescript-mode
   emacs-lisp-mode-hook
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
      mail-personal-alias-file      (conf-path "mail-aliases")
      message-auto-save-directory   (conf-path "mail-drafts")
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

;; ------------------------------------------------------ [ Markdown ]
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(declare-function markdown-export-and-preview "markdown-mode")
(add-hook
 'markdown-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c C-e") #'markdown-export-and-preview)))

;; -------------------------------------------------------- [ Octave ]
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; ----------------------------------------------------------- [ Org ]
;; Org agenda
(setq
 org-agenda-files (conf-path "org-agenda-files")
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
 '(("a" "Agenda" entry (file+headline "~/life.org" "Agenda")
    "** %?" :prepend t)
   ("b" "Bug" entry (file+headline "~/bugs/bugs.org" "Bugs")
    "** TODO %?")
   ("m" "Music" entry (file+headline "~/music.org" "Music")
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

;; ------------------------------------------------------ [ Packages ]
(setq package-menu-hide-low-priority t)

;; ---------------------------------------------------- [ Projectile ]
(require 'grep)

(projectile-global-mode)
(helm-projectile-on)
(setq projectile-mode-line "")

(add-hook
 'projectile-mode-hook
 (lambda ()
   (define-key projectile-command-map (kbd "s") 'helm-projectile-ag)
   (define-key projectile-command-map (kbd "f") 'helm-projectile-find-file)))

;; -------------------------------------------------------- [ Python ]
(add-hook
 'python-mode-hook
 (lambda ()
   (defvar python-fill-docstring-style)
   (setq-local fill-column 79)
   (setq-local python-fill-docstring-style 'pep-257-nn)
   (declare-function elpy-use-ipython "elpy")

   (setq-local
    prettify-symbols-alist
    '(("!=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)))

   (elpy-enable)

   (add-hook
    'elpy-mode-hook
    (lambda ()
      (declare-function elpy-set-test-runner "elpy")
      (elpy-set-test-runner 'elpy-test-pytest-runner)
      (highlight-indentation-mode 0)))

   (elpy-mode 1)))

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

;; ----------------------------------------------------- [ Text mode ]
(add-hook 'text-mode-hook (lambda () (auto-fill-mode 1)))

;; ------------------------------------------------------ [ Web mode ]
(dolist (ext '("\\.html" "\\.hbs" "\\.jinja"))
  (add-to-list 'auto-mode-alist (cons ext 'web-mode)))

(setq
 web-mode-engines-alist '(("django" . "\\.html"))
 web-mode-enable-auto-pairing nil
 web-mode-attr-indent-offset 2
 web-mode-markup-indent-offset 2)

(add-hook
 'web-mode-hook
 (lambda ()
   (defvar web-mode-django-control-blocks)
   (defvar web-mode-django-control-blocks-regexp)
   (push "editable" web-mode-django-control-blocks)
   (push "endeditable" web-mode-django-control-blocks)
   (setq web-mode-django-control-blocks-regexp
         (regexp-opt web-mode-django-control-blocks t))))

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
 '(flymake-errline ((t (:underline (:color "salmon1" :style wave)))))
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
 '(slime-repl-inputed-output-face ((t (:foreground "#729fcf"))) t))
