;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'project)

(defun toggler--managed-buffer-p (buf name per-project)
  (and (buffer-local-boundp 'toggler--managed buf)
    (buffer-local-value 'toggler--managed buf)
    (buffer-local-boundp 'toggler--name buf)
    (string-equal (buffer-local-value 'toggler--name buf) name)
    ;; When per-project, only match buffers scoped to the current project.
    ;; toggler--handle-toggle guarantees a current project exists here.
    (if per-project
      (and (buffer-local-boundp 'toggler--project buf)
        (string-equal
          (buffer-local-value 'toggler--project buf)
          (project-root (project-current))))
      t)))

(defun toggler--find-active-window (name per-project)
  (cl-find-if
    (lambda (w)
      (toggler--managed-buffer-p (window-buffer w) name per-project))
    (window-list nil 0 nil)))

(defun toggler--find-managed-buffer (name per-project)
  (cl-find-if
    (lambda (buf) (toggler--managed-buffer-p buf name per-project))
    (buffer-list)))

(defun toggler--show (name show-func per-project)
  "Check if a buffer already exists and if not call show-func"
  (if-let* ((buf (toggler--find-managed-buffer name per-project)))
    (progn
      (pop-to-buffer buf)
      ;; nil means the current active window
      ;; (set-window-dedicated-p nil t))
      )
    (let* ((buf (funcall show-func)))
      (with-current-buffer buf
        (set-local 'toggler--managed t)
        (set-local 'toggler--name name)
        (when per-project
          (set-local
            'toggler--project
            (project-root (project-current))))
        (pop-to-buffer buf)
        ;; (set-window-dedicated-p nil t)))))
        ))))

(defun toggler--handle-toggle (name show-func per-project)
  (when (and per-project (not (project-current)))
    (user-error
      "toggle-%s is project-scoped, but there is no current project"
      name))
  (if-let* ((win (toggler--find-active-window name per-project)))
    ;; TODO: Give options for how to hide window. E.g. by deleting
    ;; the buffer for example
    (delete-window win)
    (toggler--show name show-func per-project)))

;;;###autoload
(defmacro toggler-create-toggle (name show-func per-project)
  "A new interactive function name `toggle-NAME' will be created.
   SHOW-FUNC is expected to be a function that returns a buffer.

   Note that toggler (at the moment) can't take over already created
   buffers. It will start to manage the first buffer returned by
   SHOW-FUNC."
  `
  (defun ,(intern (format "toggle-%s" name)) ()
    (interactive)
    (toggler--handle-toggle ,name ,show-func ,per-project)))


(provide 'toggler)
