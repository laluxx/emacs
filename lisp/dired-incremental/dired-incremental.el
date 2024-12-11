;; dired-incremental.el --- Incremental narrowing for dired buffers -*- lexical-binding: t -*-

;; Author: Laluxx
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (orderless "1.0"))

;;; Commentary:
;; Provides incremental narrowing in dired buffers using orderless for matching.
;; As you type in the minibuffer, the dired buffer narrows to matching entries
;; and applies orderless highlighting faces.

;;; Code:

(require 'dired)
(require 'orderless)

(defgroup dired-incremental nil
  "Incremental narrowing for dired buffers."
  :group 'dired)

(defvar-local dired-incremental--original-buffer nil
  "The dired buffer being filtered.")

(defvar-local dired-incremental--invisible-overlays nil
  "Overlays for hiding non-matching entries.")

(defvar-local dired-incremental--highlight-overlays nil
  "Overlays for highlighting matches.")

(defun dired-incremental--clear-overlays ()
  "Clear all overlays."
  (mapc #'delete-overlay dired-incremental--invisible-overlays)
  (mapc #'delete-overlay dired-incremental--highlight-overlays)
  (setq dired-incremental--invisible-overlays nil
        dired-incremental--highlight-overlays nil))

(defun dired-incremental--hide-line (beg end)
  "Hide line from BEG to END."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'invisible t)
    (push ov dired-incremental--invisible-overlays)))

(defun dired-incremental--highlight-match (beg end face)
  "Highlight region from BEG to END with FACE."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'face face)
    (push ov dired-incremental--highlight-overlays)))

(defun dired-incremental--get-entries ()
  "Get list of entries with their positions."
  (let (entries)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((name (dired-get-filename nil t))
                    (beg (line-beginning-position))
                    (end (line-end-position)))
          (push (list name beg end) entries))
        (forward-line 1)))
    (nreverse entries)))

(defun dired-incremental--apply-face (filename pos matches faces)
  "Apply FACES to parts of FILENAME matching MATCHES at position POS."
  (cl-loop for (mbeg . mend) in matches
           for face in faces
           do (dired-incremental--highlight-match 
               (+ pos mbeg) (+ pos mend) face)))

(defun dired-incremental--narrow (pattern)
  "Narrow dired buffer to entries matching PATTERN."
  (let* ((components (orderless-pattern-compiler pattern))
         (faces (cl-loop for i below 10
                         collect (aref orderless-match-faces 
                                       (mod i (length orderless-match-faces))))))
    (dired-incremental--clear-overlays)
    (dolist (entry (dired-incremental--get-entries))
      (pcase-let ((`(,filename ,beg ,end) entry))
        (if (or (string-empty-p pattern)
                (when-let* ((matches (orderless--match components filename)))
                  (dired-incremental--apply-face 
                   filename
                   (- end (length (file-name-nondirectory filename)))
                   matches
                   faces)
                  t))
            nil  ; Keep visible
          (dired-incremental--hide-line beg (1+ end)))))))

(defun dired-incremental--minibuffer-setup ()
  "Set up minibuffer for incremental filtering."
  (add-hook 'after-change-functions
            (lambda (&rest _)
              (when (buffer-live-p dired-incremental--original-buffer)
                (with-current-buffer dired-incremental--original-buffer
                  (dired-incremental--narrow (minibuffer-contents)))))
            nil t))

(defun dired-incremental--cleanup ()
  "Clean up after filtering."
  (when (buffer-live-p dired-incremental--original-buffer)
    (with-current-buffer dired-incremental--original-buffer
      (dired-incremental--clear-overlays))))

;;;###autoload
(defun dired-incremental-filter ()
  "Begin incremental filtering in current dired buffer."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  
  (setq dired-incremental--original-buffer (current-buffer))
  (unwind-protect
      (let ((minibuffer-setup-hook (cons #'dired-incremental--minibuffer-setup
                                         minibuffer-setup-hook)))
        (read-string "Filter dired: "))
    (dired-incremental--cleanup)))

(provide 'dired-incremental)
;;; dired-incremental.el ends here
