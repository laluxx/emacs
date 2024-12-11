;;; dired-git-info.el --- Show git info in dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Laluxx
;; URL: https://github.com/laluxx/dired-git-info
;; Version: 0.3.1
;; Package-Requires: ((emacs "25"))
;; Keywords: dired, files

;;; Commentary:
;;
;; Minor mode which shows last commit message and date (info shown is
;; configurable) for git project files in dired.
;;

;;; Code:

(require 'dired)

(defgroup dired-git-info nil
  "Show git info in dired."
  :group 'files
  :prefix "dgi-")

(defface dgi-commit-message-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message overlays.")

(defcustom dgi-commit-message-format "%cr"
  "Format of the commit timestamp.
Uses the relative committer date (%cr) by default.
See git-log PRETTY FORMATS for other options."
  :type 'string)

(defvar-local dgi--commit-ovs nil
  "Overlays which show the commit timestamps.")

(defun dgi--command-to-string (program &rest args)
  "Execute PROGRAM with arguments ARGS and return output string.
If program returns non-zero exit code, return nil."
  (let* ((ecode nil)
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (setq ecode (apply #'process-file program nil t nil args))))))
    (when (eq ecode 0)
      output)))

(defun dgi--get-commit-info (&optional file gitf)
  "Get commit timestamp info for FILE using format GITF."
  (let* ((tfile (or file (dired-get-file-for-visit)))
         (file (or (file-remote-p tfile 'localname) tfile)))
    (when file
      (let ((msg (dgi--command-to-string
                  "git" "log" "-1"
                  (concat "--pretty=" (or gitf dgi-commit-message-format))
                  file)))
        (when (and msg (not (string= "" msg)))
          (substring msg 0 -1))))))  ; Remove trailing newline

(defun dgi--cleanup ()
  "Remove commit overlays."
  (dolist (ov dgi--commit-ovs)
    (delete-overlay ov))
  (setq dgi--commit-ovs nil))

(defun dgi--format-timestamp (timestamp)
  "Format TIMESTAMP for display."
  (propertize (format " [%s]" timestamp)
              'face 'dgi-commit-message-face))

(defun dgi--add-timestamp-overlay ()
  "Add timestamp overlay to the current line."
  (let* ((timestamp (dgi--get-commit-info))
         (formatted-timestamp (when timestamp (dgi--format-timestamp timestamp)))
         (eol (line-end-position))
         (ov (make-overlay eol eol)))
    (when formatted-timestamp
      (overlay-put ov 'after-string formatted-timestamp)
      (overlay-put ov 'dgi-timestamp t)
      (push ov dgi--commit-ovs))))

;;;###autoload
(define-minor-mode dired-git-info-mode
  "Toggle git timestamp info in current dired buffer."
  :lighter " dgi"
  (if dired-git-info-mode
      (progn
        (unless (derived-mode-p 'dired-mode)
          (user-error "Not in a dired buffer"))
        (unless (locate-dominating-file "." ".git")
          (user-error "Not inside a git repository"))
        (dgi--cleanup)
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when (dired-move-to-filename)
              (dgi--add-timestamp-overlay))
            (forward-line 1)))
        (add-hook 'dired-after-readin-hook #'dired-git-info-mode nil t))
    (dgi--cleanup)
    (remove-hook 'dired-after-readin-hook #'dired-git-info-mode t)))

;;;###autoload
(defun dired-git-info-auto-enable ()
  "Enable `dired-git-info-mode' if current dired buffer is in a git repo."
  (when (locate-dominating-file "." ".git")
    (dired-git-info-mode 1)))

(provide 'dired-git-info)
;;; dired-git-info.el ends here
