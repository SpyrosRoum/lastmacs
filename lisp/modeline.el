(defvar-local sep " ")
(defvar-local thin-sep
  (propertize " " 'display '(space :relative-width 0.5)))
(defvar-local wide-sep "  ")

(defun put-buffer-name ()
  (concat
    (format "%s" (nerd-icons-icon-for-buffer))
    thin-sep
    (propertize (my-get-buffer-name) 'face 'bold)))

(defun put-major-mode ()
  (capitalize
    (string-remove-suffix "-mode" (symbol-name major-mode))))

(put 'thin-sep 'risky-local-variable t)

(defun my-get-buffer-name ()
  (if-let
    (
      (file-name (buffer-file-name))
      (proj (project-current))
      (proj-root (project-root proj)))

    ;; We are in a file in a project
    (file-relative-name file-name
      (file-name-parent-directory proj-root))

    ;; Not in a project or not in a file
    (buffer-name)))

;; TODO: Remove the setq
(setq mode-line-format
  (setq-default mode-line-format
    '("%e" (:eval (put-buffer-name)) sep (:eval (put-major-mode)))))


(provide 'modeline)
