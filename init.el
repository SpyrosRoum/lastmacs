(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(setq straight-use-package-by-default t)

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

(add-to-list
  'default-frame-alist
  '(font . "JetBrains Mono Nerd Font-14"))
(set-face-attribute 'default t :font "JetBrains Mono Nerd Font-14")

(defun my/set-relative-nums ()
  (display-line-numbers-mode 1)
  (setq display-line-numbers 'relative))
(add-hook 'prog-mode-hook #'my/set-relative-nums)

(use-package doom-modeline :init (doom-modeline-mode 1))

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
  (dashboard-projects-backend 'projectile)
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

(use-package
  emacs
  :init

  (setq major-mode-remap-alist
    '
    ((python-mode . python-ts-mode)
      (rust-mode . rust-ts-mode)
      (c-mode . c-ts-mode)))

  (setq initial-buffer-choice
    (lambda () (get-buffer-create dashboard-buffer-name)))

  ;; (add-variable-watcher
  ;;   'doom-modeline-mode
  ;;   (lambda (symbol newval operation where)
  ;;     (message (format "%s changed to %s" symbol newval))))

  (global-hl-line-mode)

  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (blink-cursor-mode 0)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Set better undo limits
  (setq undo-limit 67108864) ; 64mb.
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960) ; 960mb.

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate
    #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  (setq scroll-margin 5))

(use-package nerd-icons-completion :init (nerd-icons-completion-mode))

(use-package which-key :init (which-key-mode))

(use-package
  evil
  :custom
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-fu)
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil)
  :init (evil-mode 1))

(use-package undo-fu)
(use-package vundo)

(use-package
  evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

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
  lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (c-ts-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package
  pet
  :config (add-hook 'python-base-mode-hook 'pet-mode -10))
;; (use-package poetry :init (poetry-tracking-mode))

(use-package
  lsp-pyright
  :hook
  (python-ts-mode
    .
    (lambda ()
      (require 'lsp-pyright)
      (lsp-deferred))))

(use-package
  lsp-ui
  :custom (lsp-ui-doc-position 'at-point)
  :commands lsp-ui-mode)

(use-package
  company-mode
  :bind (:map company-active-map ("C-y" . company-complete-selection))
  :custom (company-selection-wrap-around t)
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-box :hook (company-mode . company-box-mode))


;; We use built in -ts- modes
;; (use-package
;;   tree-sitter
;;   :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
;;   :init (global-tree-sitter-mode))

;; (use-package tree-sitter-langs)

;; (use-package
;;   treesit-auto
;;   :custom (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; To ensure projectile uses ripgrep:
(use-package rg)

(use-package
  projectile
  :init
  ;; Search for projects in ~/code with depth 2
  (setq projectile-project-search-path
    '(("~/code" . 2) "~/build/lastmacs" ("~/Programming" . 2)))
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map ("C-c p" . projectile-command-map))
  (:map projectile-command-map ("b" . consult-project-buffer))
  (:map projectile-command-map ("<ESC>" . nil)))

;; Borrowed from doom emacs ui.el :D
(defun doom/window-maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while
      (ignore-errors
        (windmove-left))
      (delete-window))
    (while
      (ignore-errors
        (windmove-right))
      (delete-window))))

(defun doom/window-maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while
      (ignore-errors
        (windmove-up))
      (delete-window))
    (while
      (ignore-errors
        (windmove-down))
      (delete-window))))

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

(use-package dirvish :init (dirvish-override-dired-mode))

(defun my/print-workspaces-indexed ()
  (interactive)
  (let
    (
      (names (nreverse (append (persp-names) nil)))
      (len (safe-length (persp-names)))
      (i 0)
      (str "")
      (name))
    (dolist (name names)
      (setq str (concat str (format "%d: %s | " i name)))
      (setq i (+ 1 i)))
    (message "%s" str)))

(defun my/worspace-switch-to-n (n)
  (let ((len (safe-length (persp-names))))
    (persp-switch-by-number (- len n))))

(defun my/worspace-switch-to-0 nil
  (interactive)
  (my/worspace-switch-to-n 0))

(defun my/worspace-switch-to-1 nil
  (interactive)
  (my/worspace-switch-to-n 1))

(defun my/worspace-switch-to-2 nil
  (interactive)
  (my/worspace-switch-to-n 2))

(defun my/worspace-switch-to-3 nil
  (interactive)
  (my/worspace-switch-to-n 3))

(defun my/worspace-switch-to-4 nil
  (interactive)
  (my/worspace-switch-to-n 4))

;; format: off
(use-package
  general
  :after evil
  :config
  (general-define-key
    :states '(normal insert motion emacs)
    :keymaps 'override
    :prefix-map 'spyros-map
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer spyros-def :keymaps 'spyros-map)
  (spyros-def "" nil)

  (general-def :states '(normal emacs) :keymap lsp-command-map "K" '("Check lsp docs" . lsp-ui-doc-glance))

  (spyros-def
    "f" (cons "File" (make-sparse-keymap))
    "fs" '("Save" . save-buffer)

    "p" (cons "Projects" projectile-command-map)

    "<TAB>" (cons "Workspaces" (make-sparse-keymap))
    "<TAB><TAB>" '("List spaces" . my/print-workspaces-indexed)
    "<TAB>0" '("Switch to 0" . my/worspace-switch-to-0)
    "<TAB>1" '("Switch to 1" . my/worspace-switch-to-1)
    "<TAB>2" '("Switch to 2" . my/worspace-switch-to-2)
    "<TAB>3" '("Switch to 3" . my/worspace-switch-to-3)
    "<TAB>4" '("Switch to 4" . my/worspace-switch-to-4)

    "h" (cons "Help" (make-sparse-keymap))
    "hf" '("Function" . helpful-callable)
    "hv" '("Variable" . helpful-variable)
    "hk" '("Key" . helpful-key)

    "b" (cons "Buffers" (make-sparse-keymap))
    "bd" 'kill-current-buffer

    "o" (cons "Open" (make-sparse-keymap))
    "ot" '("Toggle terminal" . vterm-toggle-cd)
    "op" '("Toggle Treemacs" . treemacs)

    "w" (cons "Windows" (make-sparse-keymap))
    "wq" 'evil-quit
    ;; --- Movement ---
    "wh" '("Focus left" . evil-window-left)
    "wj" '("Focus down" . evil-window-down)
    "wk" '("Focus up" . evil-window-up)
    "wl" '("Focus right" . evil-window-right)
    ;; --- /Movement ---
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    "wT" '("Tear window to new frame" . tear-off-window)
    "wmm" '("Maximise window" . delete-other-windows)))
;; format: on


;; --- Treemacs things ---
(use-package
  treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq
      treemacs-collapse-dirs
      (if treemacs-python-executable
        3
        0)
      treemacs-deferred-git-apply-delay 0.5
      treemacs-directory-name-transformer #'identity
      treemacs-display-in-side-window t
      treemacs-eldoc-display 'simple
      treemacs-file-event-delay 2000
      treemacs-file-extension-regex treemacs-last-period-regex-value
      treemacs-file-follow-delay 0.2
      treemacs-file-name-transformer #'identity
      treemacs-follow-after-init t
      treemacs-expand-after-init t
      treemacs-find-workspace-method 'find-for-file-or-pick-first
      treemacs-git-command-pipe ""
      treemacs-goto-tag-strategy 'refetch-index
      treemacs-header-scroll-indicators '(nil . "^^^^^^")
      treemacs-hide-dot-git-directory t
      treemacs-indentation 2
      treemacs-indentation-string " "
      treemacs-is-never-other-window nil
      treemacs-max-git-entries 5000
      treemacs-missing-project-action 'ask
      treemacs-move-forward-on-expand nil
      treemacs-no-png-images nil
      treemacs-no-delete-other-windows t
      treemacs-project-follow-cleanup nil
      treemacs-persist-file
      (expand-file-name ".cache/treemacs-persist"
        user-emacs-directory)
      treemacs-position 'left
      treemacs-read-string-input 'from-child-frame
      treemacs-recenter-distance 0.1
      treemacs-recenter-after-file-follow nil
      treemacs-recenter-after-tag-follow nil
      treemacs-recenter-after-project-jump 'always
      treemacs-recenter-after-project-expand 'on-distance
      treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
      treemacs-project-follow-into-home nil
      treemacs-show-cursor nil
      treemacs-show-hidden-files t
      treemacs-silent-filewatch nil
      treemacs-silent-refresh nil
      treemacs-sorting 'alphabetic-asc
      treemacs-select-when-already-in-treemacs 'move-back
      treemacs-space-between-root-nodes t
      treemacs-tag-follow-cleanup t
      treemacs-tag-follow-delay 1.5
      treemacs-text-scale nil
      treemacs-user-mode-line-format nil
      treemacs-user-header-line-format nil
      treemacs-wide-toggle-width 70
      treemacs-width 35
      treemacs-width-increment 1
      treemacs-width-is-initially-locked t
      treemacs-workspace-switch-cleanup nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase
      (cons
        (not (null (executable-find "git")))
        (not (null treemacs-python-executable)))
      (`(t . t) (treemacs-git-mode 'deferred))
      (`(t . _) (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)))

(use-package treemacs-evil :after (treemacs evil))

(use-package treemacs-projectile :after (treemacs projectile))

(use-package
  treemacs-perspective
  :after (treemacs perspective)
  :config (treemacs-set-scope-type 'Perspectives))

;; (use-package treemacs-magit
;;   :after (treemacs magit))
;; --- /Treemacs things ---

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

(use-package
  perspective
  :bind
  :custom
  (persp-sort 'created)
  (persp-mode-prefix-key (kbd "C-c M-p")) ; pick your own prefix key here
  :init (persp-mode))

(use-package persp-projectile :init (require 'persp-projectile))

(use-package helpful)
