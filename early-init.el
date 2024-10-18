;; We use straight.el here, not package.el
(setq package-enable-at-startup nil)

;; Move the location of the native comp cache
(when
  (and (fboundp 'startup-redirect-eln-cache)
    (fboundp 'native-comp-available-p)
    (native-comp-available-p))
  (startup-redirect-eln-cache
    (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let
  (
    (bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
          user-emacs-directory)))
    (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent
        'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
