(add-to-list
  'load-path
  (expand-file-name "lisp" user-emacs-directory))
(require 'init-emacs)
(require 'init-evil)

(use-package no-littering :init (no-littering-theme-backups))
(use-package gcmh :init (gcmh-mode 1))

(use-package avy)

(use-package ef-themes :config (ef-themes-select 'ef-elea-dark))

(use-package
  ef-themes
  :config (setq ef-themes-to-toggle '(ef-dream ef-elea-dark)))

(use-package
  doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-column-zero-based nil)
  :init (doom-modeline-mode 1))

(use-package
  elisp-autofmt
  :custom (elisp-autofmt-style 'fixed)
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

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
  ;; Replace bindings. Lazily loaded due by `use-package'.
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
    ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
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
  (setq
    register-preview-delay 0.5
    register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq
    xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref))

;; (use-package
;;   auto-virtualenvwrapper
;;   :init
;;   (add-hook 'python-base-mode-hook #'auto-virtualenvwrapper-activate
;;     -10))

(use-package
  ruff-format
  :hook (python-base-mode . ruff-format-on-save-mode))

(use-package
  pet
  :config
  (add-hook 'python-base-mode-hook
    (lambda ()
      (when-let*
        ((ipython-executable (pet-executable-find "ipython")))
        (setq-local python-shell-interpreter ipython-executable))

      (pet-mode))
    -5))

(use-package
  company-mode
  :bind (:map company-active-map ("C-y" . company-complete-selection))
  :custom (company-selection-wrap-around t)
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-box :hook (company-mode . company-box-mode))

(use-package vterm)
(use-package
  vterm-toggle
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-use-dedicated-buffer t)
  (vterm-toggle-hide-method 'delete-window)
  (vterm-toggle-fullscreen-p nil)

  :init
  ;; Show it to the bottom
  ;; Coppied from vterm-toggle README's,
  ;; I can maybe make it simpler using prot's video?
  (add-to-list
    'display-buffer-alist
    '
    (
      (lambda (buffer-or-name _)
        (let ((buffer (get-buffer buffer-or-name)))
          (with-current-buffer buffer
            (or (equal major-mode 'vterm-mode)
              (string-prefix-p
                vterm-buffer-name
                (buffer-name buffer))))))
      (display-buffer-reuse-window display-buffer-at-bottom)
      ;;(display-buffer-reuse-window display-buffer-in-direction)
      ;;display-buffer-in-direction/direction/dedicated is added in emacs27
      ;;(direction . bottom)
      ;;(dedicated . t) ;dedicated is supported in emacs27
      (reusable-frames . visible)
      (window-height . 0.3))))

(use-package
  otpp
  :straight t
  :after project
  :init
  ;; If you like to define some aliases for better user experience
  (defalias 'one-tab-per-project-mode 'otpp-mode)
  (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
  ;; Enable `otpp-mode` globally
  (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands`
  ;; to be run in the current's tab (so, current project's) root directory
  (otpp-override-mode 1))

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

;; (use-package
;;   solaire-mode
;;   :init
;;   (defun my/solaire-real-buffer-p ()
;;     (cond
;;       ((string= (buffer-name (buffer-base-buffer)) "*dashboard*")
;;         t)
;;       ((solaire-mode-real-buffer-p)
;;         t)
;;       (t
;;         nil)))

;;   (setq solaire-mode-real-buffer-fn 'my/solaire-real-buffer-p)
;;   (solaire-global-mode +1))

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
  org-modern
  :init
  (with-eval-after-load 'org
    (global-org-modern-mode)))

(use-package dired-sidebar :commands (dired-sidebar-toggle-sidebar))

(use-package
  nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package docker :bind ("C-c d" . docker))

;; Get color support in compilation mode
;; via built-in ansi-color.
;; Check out https://codeberg.org/ideasman42/emacs-fancy-compilation maybe
(use-package
  ansi-color
  :straight nil
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

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

(use-package clojure-mode)
(use-package cider)

(use-package embark :bind (("C-c ." . embark-act)))

(use-package
  embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

(use-package hl-todo :config (global-hl-todo-mode))

;; Center buffer
(use-package olivetti)

;; Typst setup
(add-to-list
  'treesit-language-source-alist
  '(typst "https://github.com/uben0/tree-sitter-typst"))

(use-package
  typst-ts-mode
  :straight
  (:type
    git
    :host sourcehut
    :repo "meow_king/typst-ts-mode"
    :files (:defaults "*.el"))
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open"))

(with-eval-after-load 'eglot
  (add-to-list
    'eglot-server-programs
    `
    ((typst-ts-mode)
      .
      ,
      (eglot-alternatives
        `(,typst-ts-lsp-download-path "tinymist" "typst-lsp")))))

;; /Typst

(use-package just-ts-mode)
