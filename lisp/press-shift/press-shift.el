;;; press-shift.el --- Quick shift actions -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Laluxx <laluxx@example.com>
;; Maintainer: Laluxx <laluxx@example.com>
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/laluxx/press-shift
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; press-shift lets you bind quick press shift actions while keeping
;; normal shift functionality when held.  A clean, minimal solution
;; using core Emacs functionality.

;; Usage:
;;   (require 'press-shift)
;;   (press-shift-mode 1)

;;; Code:

(defgroup press-shift nil
  "Quick shift press actions."
  :prefix "press-shift-"
  :group 'convenience)

(defcustom press-shift-timeout 0.2
  "Seconds to wait before canceling quick press detection."
  :type 'number
  :group 'press-shift)

(defvar press-shift--active nil
  "Track if we're handling a shift press.")

(defvar press-shift--timer nil
  "Timer for shift press detection.")

(defun press-shift--handle-event ()
  "Handle shift key press/release."
  (interactive)
  (if (not press-shift--active)
      (progn
        (setq press-shift--active t)
        (setq press-shift--timer (current-time)))
    (when (< (float-time (time-subtract (current-time) press-shift--timer))
             press-shift-timeout)
      (find-file))
    (setq press-shift--active nil)))

;;;###autoload
(define-minor-mode press-shift-mode
  "Toggle `press-shift-mode'.
When enabled, quick shift presses trigger `find-file'."
  :global t
  :group 'press-shift
  (if press-shift-mode
      (define-key special-event-map [shift] #'press-shift--handle-event)
    (define-key special-event-map [shift] nil)))

(provide 'press-shift)
;;; press-shift.el ends here
