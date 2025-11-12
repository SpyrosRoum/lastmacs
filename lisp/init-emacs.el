(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(setq straight-use-package-by-default t)

;; This needs to be one of the first things to ensure there is no
;; version missmatch with org-roam
(straight-use-package '(org :type git :depth 1))

(add-to-list
  'default-frame-alist
  '(font . "JetBrains Mono Nerd Font-14"))
(set-face-attribute 'default t :font "JetBrains Mono Nerd Font-14")

;; No beeps and boops on C-g etc when on emacs
(setq ring-bell-function 'ignore)

(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-alh --group-directories-first")
(when-let* ((uls-path (executable-find "uls")))
  (setq insert-directory-program uls-path))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(winner-mode 1)
(global-subword-mode 1)
(xterm-mouse-mode 1)
(global-hl-line-mode 1)
(column-number-mode 1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Remove line numbers from some modes
(dolist
  (mode
    '
    (term-mode-hook
      vterm-mode-hook
      shell-mode-hook
      eshell-mode-hook
      dired-mode-hook
      compilation-mode-hook
      comint-mode-hook
      helpful-mode-hook
      pdf-view-mode-hook
      image-mode-hook
      cider-repl-mode-hook
      cider-stacktrace-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Configure backups to all be in one place.
;; Also keep more of them
(setq backup-directory-alist `(("." . "~/.cache/emacs-saves")))
(setq backup-by-copying t)
(setq
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
;; -- /backups --

;; Prefer treesitter for some modes
(setq major-mode-remap-alist
  '((python-mode . python-ts-mode) (c-mode . c-ts-mode)))

;; Use the dashboard as initial buffer
(setq initial-buffer-choice
  (lambda () (get-buffer-create dashboard-buffer-name)))

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

(setq scroll-margin 5)
(setq scroll-conservatively most-positive-fixnum)

(defun my/clear-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

(define-key minibuffer-local-map (kbd "C-u") 'my/clear-line)

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

;; compilation-read-command uses `read-shell-command` by default, which doesn't use
;; completion at all. So I overwrite it to use `completing-read` instead, which seems to work great.
(defun compilation-read-command-with-autocomplete (command)
  "Use `completing-read` to add autocomplete powers to compilation read"
  (completing-read "Compile command: " compile-history
    nil nil command
    (if (equal (car compile-history) command)
      '(compile-history . 1)
      'compile-history)))

(advice-add
  #'compilation-read-command
  :override #'compilation-read-command-with-autocomplete)

(defun spy/compilation-mode-set-scrolling ()
  "Set some scrolling related vars to prefered values for the *compilation* buffer"
  (with-current-buffer (compilation-find-buffer)
    (setq-local
      compilation-scroll-output t
      scroll-margin 0)))

(add-hook 'compilation-mode-hook #'spy/compilation-mode-set-scrolling)
(add-hook 'comint-mode-hook #'spy/compilation-mode-set-scrolling)

(defun spy/fix-project ()
  (load "project.elc")
  (load "xref.elc"))

(add-hook 'after-init-hook #'spy/fix-project)

;; Copied from magit-extras
(defun magit-project-status ()
  "Run `magit-status' in the current project's root."
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))

(use-package
  project
  :straight nil
  :custom
  (project-switch-commands
    '
    ((project-find-file "Find file")
      (consult-ripgrep "Find regex" "g")
      (magit-project-status "Magit" "v"))))

(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe")
  (deactivate-mark))

;; (when (string-match ".*-WSL2" operating-system-release)
;;   (global-set-key (kbd "C-c C-c") 'wsl-copy))

;; Configure ispell to use hunspell with Greek and English dicts
(with-eval-after-load 'ispell
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,el_GR")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,el_GR")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.local/share/hunspell_personal")

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))
;; /ispell


(keymap-global-set "C-c c" #'compile)

;; Save some space on the laptop by always hidding the tab-bar
;; and adding a clock in the mode line to make it easier
;; running full-screen emacs
(when (string= (system-name) "conquest")
  (display-battery-mode 1)
  (setopt display-time-24hr-format t)
  (display-time-mode 1)
  (setopt tab-bar-show nil))

(provide 'init-emacs)
