(use-package
  evil
  :init
  ;; I initialize these in init with custom-set-variables because they
  ;; need to be defined before evil loads, and using :custom runs in :config
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (evil-mode 1))

(use-package
  evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config (evil-collection-init))

(use-package
  evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; format: off
(use-package
  general
  :after (evil tabspaces magit helpful)
  :config
  (general-define-key
    :states '(normal insert motion emacs consult)
    :keymaps 'override
    :prefix-map 'spyros-map
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer spyros-def :keymaps 'spyros-map)
  (spyros-def "" nil)

  (general-def :states '(normal emacs) "g s SPC" '("Go to chars" . avy-goto-char-timer))

  (spyros-def
    "f" (cons "File" (make-sparse-keymap))
    "fs" '("Save" . save-buffer)

    "p" (cons "Projects" project-prefix-map) ;; projectile-command-map)

    "<SPC>" '("Find file in project" . project-find-file)
    "<TAB>" (cons "Workspaces" tabspaces-command-map)

    "h" (cons "Help" (make-sparse-keymap))
    "hf" '("Function" . helpful-callable)
    "hv" '("Variable" . helpful-variable)
    "hk" '("Key" . helpful-key)

    "g" (cons "Git" (make-sparse-keymap))
    "gg" '("Status" . magit-status)

    "b" (cons "Buffers" (make-sparse-keymap))
    "bb" 'consult-project-buffer
    "bd" 'kill-current-buffer

    "o" (cons "Open" (make-sparse-keymap))
    "ot" '("Toggle terminal" . vterm-toggle)
    "op" '("Toggle sidebar" . dired-sidebar-toggle-sidebar)

    "s" (cons "Search" (make-sparse-keymap))
    "sp" '("Search Project" . consult-ripgrep)

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

(provide 'init-evil)
