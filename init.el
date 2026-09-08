;;; -*- lexical-binding: t -*-

(add-to-list
  'load-path
  (expand-file-name "lisp" user-emacs-directory))

(setq gc-cons-threshold (or spy--initial-gc-threshold 800000))

;; Define custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap package.el + use-package (we no longer use straight.el)
(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "https://melpa.org/packages/")
  t)
(package-initialize)
;; First run: fetch archive contents so `use-package-always-ensure' can install
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)

(use-package gcmh :init (gcmh-mode 1))

(use-package no-littering :init (no-littering-theme-backups))

(require 'init-emacs)
(require 'toggler)
(require 'init-evil)

(use-package avy)

(use-package
  ef-themes
  :config (setq ef-themes-to-toggle '(ef-dream ef-frost))
  ; ef-elea-dark ef-duo-dark
  (modus-themes-select 'ef-dream))

(use-package
  doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-column-zero-based nil)
  :init (doom-modeline-mode 1))

(use-package
  elisp-autofmt
  :custom
  (elisp-autofmt-style 'fixed)
  (elisp-autofmt-python-bin python-bin)
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package nerd-icons)

(use-package
  dashboard
  :config (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title
    "010010000110010101101100011011000110111100001010") ;; "Hello"
  (dashboard-projects-backend 'project-el)
  (dashboard-startup-banner 'logo)
  (dashboard-display-icons-p t) ; display icons on both GUI and terminal
  (dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents . 5) (projects . 5))))

(use-package
  nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook
    'marginalia-mode-hook
    #'nerd-icons-completion-marginalia-setup))

(use-package which-key :init (which-key-mode))

(use-package undo-fu)
(use-package vundo)

(use-package
  vertico
  :custom
  (vertico-cycle t)
  (vertico-mouse-mode t)
  (vertico-indexed-mode t)
  :init (vertico-mode))

(use-package
  orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq
    completion-styles '(orderless partial-completion basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package
  marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind
  (:map
    minibuffer-local-map
    ("M-c" . marginalia-cycle)
    :map
    completion-list-mode-map
    ("M-c" . marginalia-cycle))
  :init (marginalia-mode))

;; Persist minibuffer history over Emacs restarts. Vertico sorts by history position.
(use-package savehist :init (savehist-mode))

(use-package
  consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind
  ( ;; C-c bindings in `mode-specific-map'
    ("C-c M-x" . consult-mode-command)
    ("C-c h" . consult-history)
    ("C-c k" . consult-kmacro)
    ("C-c m" . consult-man)
    ("C-c i" . consult-info)
    ([remap Info-search] . consult-info)
    ;; C-x bindings in `ctl-x-map'
    ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
    ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
    ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
    ;; M-g bindings in `goto-map'
    ("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
    ("M-g g" . consult-goto-line) ;; orig. goto-line
    ("M-g M-g" . consult-goto-line) ;; orig. goto-line
    ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
    ("M-g m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ("M-g i" . consult-imenu)
    ("M-g I" . consult-imenu-multi)
    ;; M-s bindings in `search-map'
    ;; ("M-s d" . consult-fd)
    ;; ("M-s c" . consult-locate)
    ;; ("M-s g" . consult-grep)
    ;; ("M-s G" . consult-git-grep)
    ;; ("M-s r" . consult-ripgrep)
    ;; ("M-s l" . consult-line)
    (:map evil-normal-state-map ("/" . consult-line))
    ;; ("M-s L" . consult-line-multi)
    ;; ("M-s k" . consult-keep-lines)
    ;; ("M-s u" . consult-focus-lines)
    ;; Isearch integration
    ;; ("M-s e" . consult-isearch-history)
    ;; :map isearch-mode-map
    ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
    ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
    ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
    ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
    ;; ;; Minibuffer history
    ;; :map minibuffer-local-map
    ;; ("M-r" . consult-history)
    ) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :custom
  (consult-preview-excluded-files '("\\.gpg\\'"))
  (register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))


(use-package
  ruff-format
  :hook (python-base-mode . ruff-format-on-save-mode))

(use-package
  envrc
  :custom
  (envrc-remote 't)
  (envrc-supported-tramp-methods '("ssh" "docker"))
  :hook (after-init . envrc-mode)
  :hook (change-major-mode-after-body . envrc-mode))

(use-package
  corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  :bind
  (:map
    corfu-map
    ("SPC" . corfu-insert-separator)
    ("C-n" . corfu-next)
    ("C-p" . corfu-previous))

  :init (global-corfu-mode)
  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Part of corfu
(use-package
  corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config (corfu-popupinfo-mode))

;; Pretty icons for corfu
(use-package
  kind-icon
  :if (display-graphic-p)
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Fancy completion-at-point functions
(use-package
  cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-emoji)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(when (string= system-type "darwin")
  (use-package ultra-scroll :config (ultra-scroll-mode 1)))

(use-package
  ghostel
  ;; :bind (:map project-prefix-map ("c" . ghostel-compile))
  :custom
  (ghostel-tramp-shells
    '
    (("ssh" "zsh" login-shell)
      ("scp" login-shell)
      ("docker" "zsh" "/bin/sh")
      ("dockercp" "zsh" "/bin/sh")
      ("rpcdocker" "zsh" "/bin/sh")))

  ;; `envrc-mode' injects direnv's bookkeeping vars (DIRENV_DIFF, ...)
  ;; into the buffer-local `process-environment'.  Ghostel spawns its
  ;; shell from that environment, so the shell's own direnv hook sees
  ;; a stale DIRENV_DIFF and fails to Revert() it: direnv: error
  ;; Revert() failed: unmarshal() base64 decoding: illegal base64 data
  ;; at input byte 600

  ;; By naming me in ghostel-environment we effectively `unset` them
  ;; so it's fine.
  (ghostel-environment
    '
    ("DIRENV_DIFF"
      "DIRENV_WATCHES"
      "DIRENV_DIR"
      "DIRENV_FILE"
      "DIRENV_DEFAULT_CONFIG"))
  :config
  (let*
    (
      (elems
        '
        (
          ;; Put ghostel in bottom
          (
            (lambda (buffer-or-name _)
              (let*
                (
                  (buffer (get-buffer buffer-or-name))
                  (name (buffer-name buffer)))
                (with-current-buffer buffer
                  (and (equal major-mode 'ghostel-mode)
                    (not (string-prefix-p "Claude-" name))))))
            (display-buffer-reuse-window display-buffer-at-bottom)
            (direction . bottom)
            (reusable-frames . visible)
            (window-height . 0.3))
          ;; Put claude (spawned with ghostel-project-claude) to the right
          (
            (lambda (buffer-or-name _)
              (let*
                (
                  (buffer (get-buffer buffer-or-name))
                  (name (buffer-name buffer)))
                (with-current-buffer buffer
                  ;; fundamental mode because ghostel hasn't taken
                  ;; over yet when this runs
                  (and
                    (or (equal major-mode 'fundamental-mode)
                      (equal major-mode 'ghostel-mode))
                    (string-match-p ".*Claude Code.*" name)))))
            (display-buffer-reuse-window display-buffer-in-direction)
            (direction . rightmost)
            (reusable-frames . visible)
            (window-width . 0.5)))))
    (dolist (elem elems)
      (add-to-list 'display-buffer-alist elem))))

(use-package
  evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(defun ghostel-project-claude ()
  "Spawn a new claude session inside ghostel"
  (interactive)
  (let*
    (
      ;; ghostel starts the terminal in `default-directory',
      ;; so naming here is important
      (default-directory (project-root (project-current)))
      (buffer-name (format "Claude-%s" default-directory))
      (b (get-buffer-create buffer-name)))
    (pop-to-buffer b)
    (ghostel-exec b "claude")
    b))

(use-package
  otpp
  :after project
  :init
  ;; If you like to define some aliases for better user experience
  (defalias 'one-tab-per-project-mode 'otpp-mode)
  (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
  ;; Enable `otpp-mode` globally
  (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands`
  ;; to be run in the current's tab (so, current project's) root directory
  (otpp-override-mode -1))

;; format: off
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
    '
    ("--" "---" "==" "===" "!=" "!==" "=!="
      "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
      "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
      "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
      "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
      "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
      "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
      "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
      "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
      "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
      "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
      ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
      "<:<" ";;;"))
  (global-ligature-mode t))
;; format: on

(use-package helpful)

(use-package
  magit
  :custom
  (magit-display-buffer-function
    #'magit-display-buffer-fullframe-status-v1)
  (magit-format-file-function #'magit-format-file-nerd-icons))

;; Detected by eglot so that it prints pretty docs w/ eldoc 
(use-package markdown-mode :mode ("README\\.md\\'" . gfm-mode))

(use-package
  rust-mode
  :hook (rust-mode . eglot-ensure)
  :init
  (setq rust-mode-treesitter-derive t)
  (setq rust-format-on-save t))

(use-package
  org-roam
  :custom (org-roam-directory (file-truename "~/Documents/roam"))
  :config (org-roam-db-autosync-mode))

(use-package
  nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker :bind ("C-c d" . docker))

(use-package
  exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package
  smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :init
  (dolist (mode '(prog-mode-hook text-mode-hook markdown-mode-hook))
    (add-hook mode #'smartparens-mode))
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package git-gutter :init (global-git-gutter-mode +1))

(use-package lua-mode)

;; clojure-mode is still needed for some cider things,
;; but clojure-ts-mode is mainly at use
(use-package clojure-mode)
(use-package clojure-ts-mode :hook (clojure-ts-mode . cider-mode))

(use-package cider)

(use-package embark :bind (("C-c ." . embark-act)))

(use-package
  embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

(use-package hl-todo :config (global-hl-todo-mode))

;; Center buffer
(use-package olivetti)

(use-package just-ts-mode)

(use-package
  symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :init (setq symbol-overlay-scope t))

(use-package breadcrumb :config (breadcrumb-mode))

(use-package
  scratchpads
  :vc
  (:url
    "https://github.com/SpyrosRoum/emacs-scratchpads"
    :rev
    :newest
    :branch "master")
  :bind
  (("C-c s n" . scratchpad-new)
    ("C-c s o" . scratchpad-open)
    ("C-c s d" . scratchpad-delete)))

(use-package
  ledger-mode
  :custom
  (ledger-binary-path "hledger")
  (ledger-mode-should-check-version nil)
  (ledger-report-auto-width nil)
  (ledger-report-links-in-register nil)
  (ledger-report-native-highlighting-arguments '("--color=always"))
  (ledger-default-date-string "%Y-%m-%d")
  :mode ("\\.hledger\\'" "\\.ledger\\'"))

(use-package
  emacs-solo-dired-gutter
  :ensure nil
  :no-require t
  :defer t
  :init (setq emacs-solo-dired-gutter-enabled t)

  (defvar emacs-solo/dired-git-status-overlays nil
    "List of active overlays in Dired for Git status.")

  (defun emacs-solo/dired--git-status-face (code)
    "Return a cons cell (STATUS . FACE) for a given Git porcelain CODE."
    (let*
      (
        (git-status-untracked "??")
        (git-status-modified " M")
        (git-status-modified-alt "M ")
        (git-status-deleted "D ")
        (git-status-added "A ")
        (git-status-renamed "R ")
        (git-status-copied "C ")
        (git-status-ignored "!!")
        (status
          (cond
            ((string-match-p "\\?\\?" code)
              git-status-untracked)
            ((string-match-p "^ M" code)
              git-status-modified)
            ((string-match-p "^M " code)
              git-status-modified-alt)
            ((string-match-p "^D" code)
              git-status-deleted)
            ((string-match-p "^A" code)
              git-status-added)
            ((string-match-p "^R" code)
              git-status-renamed)
            ((string-match-p "^C" code)
              git-status-copied)
            ((string-match-p "\\!\\!" code)
              git-status-ignored)
            (t
              "  ")))

        (face
          (cond
            ((string= status git-status-ignored)
              'shadow)
            ((string= status git-status-untracked)
              'warning)
            ((string= status git-status-modified)
              'font-lock-function-name-face)
            ((string= status git-status-modified-alt)
              'font-lock-function-name-face)
            ((string= status git-status-deleted)
              'error)
            ((string= status git-status-added)
              'success)
            (t
              'font-lock-keyword-face))))
      (cons status face)))

  (defun emacs-solo/dired-git-status-overlay ()
    "Overlay Git status indicators on the first column in Dired."
    (interactive)
    (require 'vc-git)
    (let
      (
        (git-root
          (ignore-errors
            (vc-git-root default-directory))))
      (when
        (and git-root
          (not (file-remote-p default-directory))
          emacs-solo-dired-gutter-enabled)
        (setq git-root (expand-file-name git-root))
        (let*
          (
            (git-status
              (vc-git--run-command-string
                nil
                "status"
                "--porcelain"
                "--ignored"
                "--untracked-files=normal"))
            (status-map (make-hash-table :test 'equal)))
          (mapc #'delete-overlay emacs-solo/dired-git-status-overlays)
          (setq emacs-solo/dired-git-status-overlays nil)
          (dolist (line (split-string git-status "\n" t))
            (when (string-match "^\\(..\\) \\(.+\\)$" line)
              (let*
                (
                  (code (match-string 1 line))
                  (file (match-string 2 line))
                  (fullpath (expand-file-name file git-root))
                  (status-face
                    (emacs-solo/dired--git-status-face code)))
                (puthash fullpath status-face status-map))))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let*
                (
                  (file
                    (ignore-errors
                      (expand-file-name (dired-get-filename nil t)))))
                (when file
                  (setq file
                    (if (file-directory-p file)
                      (concat file "/")
                      file))
                  (let*
                    (
                      (status-face
                        (gethash file status-map
                          (cons "  " 'font-lock-keyword-face)))
                      (status (car status-face))
                      (face (cdr status-face))
                      (status-str
                        (propertize (format " %s " status)
                          'face
                          face))
                      (ov
                        (make-overlay
                          (line-beginning-position)
                          (1+ (line-beginning-position)))))
                    (overlay-put ov 'before-string status-str)
                    (push ov emacs-solo/dired-git-status-overlays))))
              (forward-line 1)))))))
  (add-hook
    'dired-after-readin-hook
    #'emacs-solo/dired-git-status-overlay))
