;; -*- lexical-binding: t; -*-

;; Startup speed, annoyance suppression
(setq spy--initial-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Tell use-package to install if missing by default
;; Use `:ensure nil' in packages you *don't* want to install
(setq use-package-always-ensure t)

;; Setting *-resize-pixelwise to `t' lets frames/windows resize
;; smoothly at sub-character increments
(setq frame-resize-pixelwise t)
; (setq window-resize-pixelwise t)

(when (boundp 'tool-bar-mode) ; When in a GUI, disable tool bar;
  (tool-bar-mode -1)) ; all these tools are in the menu-bar anyway

(setq default-frame-alist
  '
  ((fullscreen . maximized)

    ;; You can turn off scroll bars by uncommenting these lines:
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)

    ;; Setting the face in here prevents flashes of
    ;; color as the theme gets activated
    (background-color . "#000000")
    (foreground-color . "#ffffff")
    (ns-appearance . dark)
    (ns-transparent-titlebar . t)))

;; Move the location of the native comp cache
(when
  (and (fboundp 'startup-redirect-eln-cache)
    (fboundp 'native-comp-available-p)
    (native-comp-available-p))
  (startup-redirect-eln-cache
    (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))
