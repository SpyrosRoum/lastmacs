;; scratchpads.el --- Persistent scratchpad handling  -*- lexical-binding: t -*-

;; Author: Spyros Roum <spyros.roum@posteo.net>
;; Maintainer: Spyros Roum <spyros.roum@posteo.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/SpyrosRoum/emacs-scratchpads
;; Keywords: tools


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Scratchapads helps you handle multiple persistant scratch files
;; on a project-by-project or global basis.
;; This can be useful for anything from jotting some quick notes down,
;; to writing API calls for your project with Org mode and Verb.

;;; Code:

(defgroup scratchpad nil
  "Handle persistent scratchpads by the project.")

(defcustom scratchpad-base-dir
  (expand-file-name "scratches" user-emacs-directory)
  "The base directory for scratchpads.
A sub-directory is created for each project, along with one
more for all scratchpads that are created outside of one."
  :group 'scratchpad)

(defvar scratchpad-projectless-dir "misc--scratches"
  "Directory for projectless scratchpads.
A \"projectless\" scratchpad is any scratchpad that was created
outside of any project.
This directory is created inside the base-dir.")

(defun sp--path-with-num (path num)
  "Adds the given `num' in the `path', respecting file extensions."
  (if-let ((ext (file-name-extension path 't)))
    ;; `string-remove-suffix' is used instead of `file-name-base'
    ;; so that we can handle path-like names without "eating" the path.
    (concat (string-remove-suffix ext path) (format "%s" num) ext)
    (concat path (format "%s" num))))

(defun sp--generate-unique-name (name dir)
  "Generate a unique name for the scratchpad.
Returns the expanded dir+name which is certain not to exist.
When the given name already exists a unique one is generated
by appending a number starting from 2, until one that doesn't
exist is found.
The number is appended at the end of the name unless it
includes a dot (`.'), then it's added before the dot."
  (let ((num 1))
    (while (f-exists? (expand-file-name name dir))
      (setq num (1+ num))
      (setq name (sp--path-with-num name num)))
    (expand-file-name name dir)))

(defun scratchpad-new (name)
  "Opens a new scratchpad with a name based on the name given.
If a file already exists with the given name then it's altered by appending
a number to the end of it, but before the file extension if it exists.
The number starts from `2' and increments until a unique file name is found.

For example if the given name is `foo.py', but there already is a scratchpad
named `foo.py', the final file name could be `foo2.py'.

If no extension is given then `initial-major-mode' is used as the mode of the
file.

Returns the resulting buffer object."
  (interactive (list (read-string "Enter a name:" "scratch")))
  (let*
    (
      (scratch-dir
        (if-let ((proj (project-current)))
          (project-name proj)
          scratchpad-projectless-dir))
      (base-dir (expand-file-name scratch-dir scratchpad-base-dir))
      (path (sp--generate-unique-name name base-dir)))
    (mkdir base-dir 't)
    (let ((buff (find-file path)))
      (when (string= (symbol-name major-mode) "fundamental-mode")
        (funcall initial-major-mode))
      buff)))

(defun scratchpad-open (&optional name)
  (interactive)
  (message "TODO open"))

(defun scratchpad-delete (&optional name)
  (interactive)
  (message "TODO delete"))
