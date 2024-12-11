;;; smart-space.el --- Smart space insertion for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/laluxx/smart-space

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a minor mode that enhances space insertion in Emacs Lisp mode.
;; It inserts comments on empty lines and adds new string entries in lists of strings.

;;; Code:

(defgroup smart-space nil
  "Smart space insertion for Emacs Lisp."
  :group 'convenience
  :prefix "smart-space-")

(defcustom smart-space-comment-string ";; "
  "String to insert as a comment on empty lines."
  :type 'string
  :group 'smart-space)

(defun smart-space-get-list-at-point ()
  "Get the list at point if it exists and is a quoted list of strings."
  (save-excursion
    (when (eq (char-before) ?\))
      (backward-list)
      (when (and (eq (char-after) ?\()
                 (eq (char-before) ?'))
        (let ((start (point))
              (end (save-excursion (forward-list) (point))))
          (buffer-substring-no-properties start end))))))

(defun smart-space-list-of-strings-p (list-string)
  "Check if LIST-STRING represents a list of only strings."
  (with-temp-buffer
    (insert list-string)
    (goto-char (point-min))
    (condition-case nil
        (let ((list-content (read (current-buffer))))
          (and (listp list-content)
               (cl-every #'stringp list-content)))
      (error nil))))

;;;###autoload
(defun smart-space-insert ()
  "Insert space with smart behavior based on context."
  (interactive)
  (let ((last-command-was-smart-space (eq last-command 'smart-space-insert)))
    (cond
     ;; Don't do anything special if the last command was smart-space-insert
     (last-command-was-smart-space
      (insert " ")
      (setq this-command 'self-insert-command))

     ;; On empty line
     ((save-excursion
        (beginning-of-line)
        (looking-at "^[[:space:]]*$"))
      (beginning-of-line)
      (insert smart-space-comment-string)
      (setq this-command 'smart-space-insert))

     ;; At the end of a list of strings
     ((let ((list-string (smart-space-get-list-at-point)))
        (and list-string
             (smart-space-list-of-strings-p list-string)))
      (insert " \"\"")
      (backward-char)
      (setq this-command 'smart-space-insert))

     ;; Default case
     (t (insert " ")
        (setq this-command 'self-insert-command)))))

;;;###autoload
(define-minor-mode smart-space-mode
  "Minor mode for smart space insertion in Emacs Lisp mode."
  :lighter " SmartSpace"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") #'smart-space-insert)
            map))

;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'smart-space-mode)

(provide 'smart-space)

;;; smart-space.el ends here
