;; init.el --- Personal Emacs configurations
;; Author: Simen Heggestøyl <simenheg@runbox.com>

(require 'package)

(setq
 package-archives
 '(("GNU" . "https://elpa.gnu.org/packages/")
   ("NonGNU" . "https://elpa.gnu.org/nongnu/")
   ("MELPA Stable" . "https://stable.melpa.org/packages/")
   ("MELPA" . "https://melpa.org/packages/")))

(setq
 package-archive-priorities
 '(("GNU" . 4)
   ("NonGNU" . 3)
   ("MELPA" . 2)
   ("MELPA Stable" . 1)))

(setq
 package-selected-packages
 '(avy
   bbdb
   blacken
   company
   company-quickhelp
   counsel
   csv-mode
   cycle-quotes
   exec-path-from-shell
   flymake-eslint
   focus
   jit-spell
   js2-mode
   json-mode
   know-your-http-well
   lorem-ipsum
   magit
   markdown-mode
   multiple-cursors
   nodejs-repl
   norwegian-holidays
   paredit
   pyvenv
   rdf-prefix
   restclient
   restclient-test
   slime
   smex
   sql-indent
   terraform-mode
   web-mode
   yasnippet))

(setq package-pinned-packages
      '((hcl-mode . "MELPA")
        (json-mode . "GNU")
        (magit . "MELPA")))

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
 history-delete-duplicates      t  ; Delete duplicate history entries
 inhibit-startup-screen         t  ; No startup screen
 initial-scratch-message      nil  ; No scratch message
 kill-read-only-ok              t  ; Killing read-only text is OK
 password-cache-expiry        nil  ; Cache TRAMP passwords forever
 sentence-end-double-space    nil  ; Fill with single spaces
 show-paren-delay               0  ; Don't delay the paren update
 sort-fold-case                 t  ; Don't care about case when sorting
 use-short-answers              t) ; Make "yes/no" prompts "y/n"

(blink-cursor-mode              0) ; No blinking cursor
(column-number-mode             1) ; Show column number
(editorconfig-mode              1) ; Respect EditorConfig files
(global-auto-revert-mode        1) ; Reload files after modification
(global-prettify-symbols-mode   1) ; Pretty symbols (e.g. lambda => λ)
(global-subword-mode            1) ; Better editing of camelCasedWords
(prefer-coding-system      'utf-8) ; Always prefer UTF-8
(yas-global-mode                1) ; YASnippet everywhere

;; Add lisp/ and all subdirectories to the load-path
(let ((default-directory (locate-user-emacs-file "lisp/")))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Miscellaneous utility functions
(load "defuns")

(global-set-key (kbd "C-'")     'org-cycle-agenda-files)
(global-set-key (kbd "C-+")     'dec-next-number)
(global-set-key (kbd "C-=")     'inc-next-number)
(global-set-key (kbd "C-?")     'eglot-help-at-point)
(global-set-key (kbd "C-M-y")   'counsel-yank-pop)
(global-set-key (kbd "C-S-k")   'kill-whole-line)
(global-set-key (kbd "C-\"")    'cycle-quotes)
(global-set-key (kbd "C-a")     'beginning-of-indentation-or-line)
(global-set-key (kbd "C-c M-$") 'ispell-change-dictionary)
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)
(global-set-key (kbd "C-c a")   'org-agenda)
(global-set-key (kbd "C-c c")   'org-capture)
(global-set-key (kbd "C-c d")   'duplicate-dwim)
(global-set-key (kbd "C-c e")   'mc/edit-lines)
(global-set-key (kbd "C-c f")   'find-grep)
(global-set-key (kbd "C-c l")   'mc/mark-all-like-this)
(global-set-key (kbd "C-c m")   'gnus)
(global-set-key (kbd "C-c n")   'mc/mark-next-like-this)
(global-set-key (kbd "C-c r")   'rename-buffer)
(global-set-key (kbd "C-c w")   'compare-windows)
(global-set-key (kbd "C-j")     'newline)
(global-set-key (kbd "C-w")     'kill-region-or-backward-delete-sexp)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b")   'ivy-switch-buffer)
(global-set-key (kbd "C-x k")   'kill-current-buffer)
(global-set-key (kbd "C-x p f") 'counsel-project-find-file)
(global-set-key (kbd "C-x p m") 'magit-status)
(global-set-key (kbd "C-x p p") 'counsel-project-switch-project)
(global-set-key (kbd "C-x p s") 'counsel-ag)
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
(require 'norwegian-holidays)

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
    (blacken-mode . "")
    (company-mode . "")
    (counsel-mode . "")
    (eldoc-mode . "")
    (emacs-lisp-mode . "el")
    (flymake-mode . "")
    (jit-spell-mode . " ✅")
    (js2-mode "js2")
    (magit-auto-revert-mode . "")
    (org-mode . "🦄")
    (paredit-mode . "")
    (python-mode . "🐍")
    (subword-mode . "")
    (with-editor-mode . "")
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
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.05)

(add-hook 'prog-mode-hook (lambda () (company-mode 1)))

;; ----------------------------------------------------------- [ CSS ]
(add-hook
 'css-mode-hook
 (lambda ()
   (setq css-indent-offset 2)))

;; ----------------------------------------------------------- [ CSV ]
(add-hook
 'csv-mode-hook
 (lambda ()
   (setq csv-confirm-region nil)
   (csv-guess-set-separator)
   (declare-function csv-align-fields "csv-mode" (hard beg end))
   (csv-align-fields nil (point-min) (point-max))
   (hl-line-mode)))

;; ------------------------------------------------------- [ Counsel ]
(setq counsel-ag-base-command "ag --vimgrep --hidden %s")

;; ---------------------------------------------------------- [ Diff ]
(setq diff-font-lock-prettify t)

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
     (vector 'remap 'dired-goto-file) 'find-file)))

(setq dired-movement-style 'cycle)
(setq dired-recursive-copies 'always)
(setq global-auto-revert-non-file-buffers t)

;; ---------------------------------------------------- [ Dockerfile ]
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))

;; ------------------------------------------------------- [ DocView ]
;; Auto-update document files
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(setq doc-view-continuous t) ; Smooth document viewing

;; --------------------------------------------------------- [ Eglot ]
(with-eval-after-load 'eglot
  (defvar eglot-ignored-server-capabilites)
  (push :documentHighlightProvider eglot-ignored-server-capabilites))

(add-hook
 'eglot-managed-mode-hook
 (lambda ()
   (setq mode-line-misc-info nil)))

;; --------------------------------------------------------- [ ElDoc ]
(setq eldoc-echo-area-use-multiline-p nil)

;; --------------------------------------------------------- [ Emacs ]
;; Set EDITOR environment variable
(setenv "EDITOR" "emacs -Q")

;; Use pixel scrolling
(pixel-scroll-precision-mode 1)

;; Move point to the first newly duplicated line
(setq duplicate-line-final-position 1)

;; Show opening parenthesis when it is offscreen
(setq show-paren-context-when-offscreen 'overlay)

;; Don't bother to ask unless the file *really* is large (100 MB+)
(setq large-file-warning-threshold 100000000)

(setq max-mini-window-height 0.2)

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

(add-hook
 'paredit-mode-hook
 (lambda ()
   (define-key paredit-mode-map (kbd "RET") nil)
   (define-key paredit-mode-map (kbd "C-j") nil)))

;; ----------------------------------------------------------- [ ERC ]
(setq erc-fill-column fill-column)

;; ----------------------------------------------- [ Eshell commands ]
(define-eshell-command django-server
  "*server*" "./manage.py runserver 0.0.0.0:8000")

(define-eshell-command npm-serve
  "*npm-serve*" "npm run serve")

(define-eshell-command npm-watch
  "*npm-watch*" "npm run watch")

(define-eshell-command simple-http-server
  "*simple-http-server*" "python3 -m http.server")

(define-eshell-command yarn-watch
  "*yarn-watch*" "yarn run watch")

;; ------------------------------------------------------- [ Flymake ]
(with-eval-after-load 'flymake
  (defvar flymake-mode-map)

  (define-key flymake-mode-map (kbd "M-g M-n")
    'flymake-goto-next-error)

  (define-key flymake-mode-map (kbd "M-g M-p")
    'flymake-goto-prev-error))

;; ----------------------------------------------------------- [ Ivy ]
(with-eval-after-load 'counsel
  (ivy-configure 'counsel-M-x :initial-input ""))

;; ---------------------------------------------------------- [ Java ]
(add-hook
 'java-mode
 (lambda ()
   (defvar c-offsets-alist)
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
   (setq-local js2-ignored-warnings '("msg.missing.semi"))
   (local-set-key (kbd "C-j") 'js2-line-break)
   (local-set-key (kbd "RET") 'js2-line-break)))

(add-hook 'js-mode-hook (lambda () (setq-local fill-column 79)))

(add-hook 'js-mode-hook #'flymake-eslint-enable)
(add-hook 'mhtml-mode-hook #'flymake-eslint-enable)
(add-hook 'web-mode-hook #'flymake-eslint-enable)

;; ---------------------------------------------------------- [ JSON ]
(dolist (filename '(".arcconfig" ".arclint" ".babelrc" ".bowerrc"
                    ".eslintrc"))
  (add-to-list 'auto-mode-alist (cons filename 'json-mode)))

;; --------------------------------------------------------- [ LaTeX ]
(add-hook
 'text-mode-hook
 (lambda ()
   (setq-local prettify-symbols-alist
               (assoc-delete-all "\\newline" prettify-symbols-alist))))

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
   ielm-mode-hook
   inferior-lisp-mode-hook
   inferior-scheme-mode-hook
   lisp-interaction-mode-hook
   lisp-mode-hook
   scheme-mode-hook
   slime-repl-mode-hook))

;; ----------------------------------------------------------- [ Lua ]
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))

;; --------------------------------------------------------- [ Magit ]
(setq git-commit-summary-max-length 50)

(add-hook 'git-commit-mode-hook (lambda () (jit-spell-mode 1)))

(add-hook
 'magit-diff-mode-hook
 (lambda ()
   (define-key magit-diff-section-base-map (kbd "C-<return>")
     'magit-diff-visit-file)
   (define-key magit-diff-section-base-map [remap magit-visit-thing]
     'magit-diff-visit-worktree-file)))

;; ---------------------------------------------------------- [ Mail ]
(load "private-stuff" t)

(setq user-mail-address "simenheg@runbox.com")
(setq mail-user-agent 'gnus-user-agent)

(setq gnus-use-full-window nil)

;; IMAP
(setq gnus-select-method '(nnimap "mail.runbox.com"))
(setq gnus-message-archive-group "nnimap:Sent")

;; SMTP
(setq smtpmail-smtp-server "mail.runbox.com")
(setq smtpmail-smtp-service 587)
(setq send-mail-function #'smtpmail-send-it)

;; Expand mail-aliases on `next-line' and `end-of-buffer'
(add-hook
 'message-mode-hook
 (lambda ()
   (defvar message-mode-map)
   (define-key message-mode-map [remap next-line]
     'mail-abbrev-next-line)
   (define-key message-mode-map [remap end-of-buffer]
     'mail-abbrev-end-of-buffer)
   (jit-spell-mode 1)))

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
(add-hook 'markdown-mode-hook #'jit-spell-mode)

;; ------------------------------------------------------ [ Midnight ]
(midnight-mode 1)

;; ----------------------------------------------------------- [ Org ]
;; Org agenda
(setq
 org-agenda-files (locate-user-emacs-file "org-agenda-files")
 org-agenda-start-on-weekday nil
 org-agenda-include-diary t
 org-clock-clocked-in-display nil)

(setq org-startup-folded t)

;; Ignore irrelevant holidays
(setq
 holiday-bahai-holidays nil
 holiday-christian-holidays nil
 holiday-general-holidays nil
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

;; ------------------------------------------------------- [ Parquet ]
(define-derived-mode parquet-mode csv-mode "Parquet"
  (call-process-region (point-min) (point-max) "parq2csv" t t)
  (goto-char (point-min))
  (csv-align-fields nil (point-min) (point-max))
  (read-only-mode 1)
  (set-buffer-modified-p nil))
(add-to-list 'auto-mode-alist '("\\.parq$" . parquet-mode))
(add-to-list 'auto-mode-alist '("\\.parquet$" . parquet-mode))

;; ------------------------------------------------------- [ Project ]
(defun counsel-project-find-file ()
  (interactive)
  (let ((completing-read-function 'ivy-completing-read))
    (project-find-file)))

(defun counsel-project-switch-project ()
  (interactive)
  (let ((completing-read-function 'ivy-completing-read))
    (call-interactively 'project-switch-project)))

(defun project-magit ()
  (interactive)
  (magit-status (project-root (project-current))))

(setq
 project-switch-commands
 '((counsel-project-find-file "Find file")
   (project-find-regexp "Find regexp")
   (project-find-dir "Find directory")
   (project-vc-dir "VC-Dir")
   (project-eshell "Eshell")
   (project-magit "Magit" ?m)))

;; -------------------------------------------------------- [ Python ]
(exec-path-from-shell-copy-env "WORKON_HOME")

(add-hook 'python-mode-hook 'eglot-ensure)

(add-hook
 'python-mode-hook
 (lambda ()
   (defvar python-fill-docstring-style)
   (setq-local fill-column 79)
   (setq-local python-fill-docstring-style 'pep-257-nn)
   (setq-local
    prettify-symbols-alist
    '(("!=" . ?≠)
      ("<=" . ?≤)
      (">=" . ?≥)))
   (blacken-mode 1)))

(add-hook
 'pyvenv-post-activate-hooks
 (lambda ()
   (when (fboundp 'eglot-reconnect)
     (call-interactively #'eglot-reconnect))))

;; --------------------------------------------------- [ REST Client ]
(add-to-list 'auto-mode-alist '("\\.http" . restclient-mode))

(add-hook
 'restclient-mode-hook
 (lambda ()
   (restclient-test-mode 1)
   (local-set-key
    (kbd "C-c C-c") 'restclient-http-send-current-stay-in-window)))

(add-hook 'restclient-test-mode-hook #'flymake-mode)

;; ----------------------------------------------------------- [ SQL ]
(setq-default sql-product 'postgres)

;; ----------------------------------------------------- [ Terraform ]
(declare-function terraform-format-on-save-mode "terraform-mode"
                  (&optional ARG))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; ----------------------------------------------------- [ Timeclock ]
;; Don't ask for a reason when clocking out
(setq timeclock-get-reason-function nil)

;; --------------------------------------------------- [ Tree-sitter ]
(setq
 treesit-language-source-alist
 '((dockerfile
    "https://github.com/camdencheek/tree-sitter-dockerfile")
   (go "https://github.com/tree-sitter/tree-sitter-go")
   (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
   (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; ------------------------------------------------------------ [ VC ]
(declare-function log-edit-insert-message-template "log-edit" ())
(remove-hook 'log-edit-hook #'log-edit-insert-message-template)
(add-hook
 'log-edit-mode-hook
 (lambda ()
   (jit-spell-mode 1)
   (auto-fill-mode 1)))
(add-hook 'log-view-mode-hook (lambda () (hl-line-mode 1)))

;; ----------------------------------------------------------- [ Vue ]
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

;; ------------------------------------------------------ [ Web mode ]
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))

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
   (setq-local fill-column 79)
   (push "editable" web-mode-django-control-blocks)
   (push "endeditable" web-mode-django-control-blocks)
   (setq web-mode-django-control-blocks-regexp
         (regexp-opt web-mode-django-control-blocks t))))

;; ---------------------------------------------------------- [ YAML ]
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

;;--------------------------------------------------------- [ Custom ]
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t nil)))
 '(ivy-current-match ((t (:background "#FFF876" :foreground "black"))))
 '(ivy-minibuffer-match-face-1 ((t nil)))
 '(ivy-minibuffer-match-face-2 ((t (:background "#FBE448" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:background "#FBE448" :weight bold))))
 '(ivy-minibuffer-match-face-4 ((t (:background "#FBE448" :weight bold))))
 '(terraform-resource-name-face ((t (:foreground "deep pink")))))
