(add-to-list
  'load-path
  (expand-file-name "lisp" user-emacs-directory))
(require 'init-emacs)
(require 'init-evil)

(use-package no-littering :init (no-littering-theme-backups))
(use-package gcmh :init (gcmh-mode 1))

(use-package avy)

(use-package
  doom-themes
  :config
  ;; Global settings (defaults)
  (setq
    doom-themes-enable-bold t ; if nil, bold is universally disabled
    doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
  (dashboard-projects-switch-function
    'tabspaces-switch-or-create-workspace)
  (dashboard-startup-banner 'logo)
  (dashboard-display-icons-p t) ; display icons on both GUI and terminal
  (dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items
    '
    ((recents . 5)
      ;; (bookmarks . 5)
      (projects . 5)
      ;; (agenda   kj . 5)
      )))

(use-package nerd-icons-completion :init (nerd-icons-completion-mode))

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

(use-package
  auto-virtualenvwrapper
  :init
  (add-hook 'python-base-mode-hook #'auto-virtualenvwrapper-activate
    -10))

(use-package
  ruff-format
  :hook (python-base-mode . ruff-format-on-save-mode))

(use-package
  pet
  :config
  (add-hook 'python-base-mode-hook
    (lambda ()
      (when-let ((ipython-executable (pet-executable-find "ipython")))
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

;; -- Tabspaces --
(use-package
  tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup. 
  :commands
  (tabspaces-switch-or-create-workspace
    tabspaces-open-or-create-project-and-workspace)
  :bind
  (:map
    project-prefix-map
    ("p" . tabspaces-open-or-create-project-and-workspace))
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t))

(defvar tabspaces-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'tabspaces-clear-buffers)
    (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
    (define-key map (kbd "d") 'tabspaces-close-workspace)
    (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
    (define-key
      map
      (kbd "o")
      'tabspaces-open-or-create-project-and-workspace)
    (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
    (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
    (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
    (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
    map)
  "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")
;; -- /Tabspaces --

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
    'magit-display-buffer-fullframe-status-v1))

(use-package
  solaire-mode
  :init
  (defun my/solaire-real-buffer-p ()
    (cond
      ((string= (buffer-name (buffer-base-buffer)) "*dashboard*")
        t)
      ((solaire-mode-real-buffer-p)
        t)
      (t
        nil)))

  (setq solaire-mode-real-buffer-fn 'my/solaire-real-buffer-p)

  (solaire-global-mode +1))

;; Detected by eglot so that it prints pretty docs w/ eldoc 
(use-package markdown-mode :mode ("README\\.md\\'" . gfm-mode))

(use-package rust-mode :init (setq rust-mode-treesitter-derive t))

(use-package
  rustic
  :after (rust-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-trigger 'on-save)
  (setq rustic-format-on-save-method 'rustic-format-buffer)
  :custom (rustic-cargo-use-last-stored-arguments t))

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

(use-package
  eglot
  :straight nil
  :ensure nil
  :config
  ;; Used basedpyright when it's available instead of pyright
  (when (executable-find "basedpyright-langserver")
    (add-to-list
      'eglot-server-programs
      '(python-base-mode . ("basedpyright-langserver" "--stdio")))))

(use-package dockerfile-mode)

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
  :init (exec-path-from-shell-initialize))

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
