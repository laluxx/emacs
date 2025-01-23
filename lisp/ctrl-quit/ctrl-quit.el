;;; ctrl-quit.el --- Quit Emacs commands on Ctrl release -*- lexical-binding: t -*-

;; Author: Laluxx
;; Maintainer: you lmao
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/laluxx/ctrl-quit
;; Keywords: convenience

;;; Code:

(eval-when-compile (require 'subr-x)) ; for string-empty-p

(defgroup ctrl-quit nil
  "Call keyboard-quit on Ctrl release."
  :group 'convenience
  :prefix "ctrl-quit-")

(defcustom ctrl-quit-enabled t
  "Whether ctrl-quit is enabled."
  :type 'boolean
  :group 'ctrl-quit)

(defvar ctrl-quit--module-path
  (expand-file-name 
   "ctrl-quit-module.so"
   (file-name-directory
    (or load-file-name buffer-file-name)))
  "Path to the ctrl-quit native module.")

(declare-function ctrl-quit-init-module "ctrl-quit-module")
(declare-function ctrl-quit-cleanup-module "ctrl-quit-module")

;;;###autoload
(define-minor-mode ctrl-quit-mode
  "Minor mode to quit on Ctrl release."
  :global t
  :group 'ctrl-quit
  (if ctrl-quit-mode
      (condition-case err
          (progn
            (module-load ctrl-quit--module-path)
            (ctrl-quit-init-module))
        (error
         (setq ctrl-quit-mode nil)
         (message "Failed to initialize ctrl-quit: %s" (error-message-string err))))
    (when (featurep 'ctrl-quit-module)
      (ctrl-quit-cleanup-module))))

(defun ctrl-quit-handler ()
  "Handler called from C when Ctrl is released."
  (when ctrl-quit-enabled
    (keyboard-quit)))

(provide 'ctrl-quit)
;;; ctrl-quit.el ends here
