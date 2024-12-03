;;; navi-mode.el --- A minor mode for effortless navigation/reading -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.1
;; Homepage: https://github.com/laluxx/navi-mode
;; Keywords: convenience navigation movement

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package is primarily designed
;; to give the `ESC` key a purpose in Emacs by mimicking how it works in Vi.
;; 'navi-mode' is meant for effortless navigation and reading.

;;; Code:

(defvar navi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "f") 'forward-char)
    (define-key map (kbd "b") 'backward-char)
    (define-key map (kbd "a") 'beginning-of-line)
    (define-key map (kbd "0") 'beginning-of-line)
    (define-key map (kbd "e") 'end-of-line)
    (define-key map (kbd "g") 'beginning-of-buffer)
    (define-key map (kbd "G") 'end-of-buffer)
    (define-key map (kbd "C-g") 'navi-mode)
    (define-key map (kbd "C-m") 'navi-mode)
    (define-key map (kbd "m") 'navi-mode)
    (define-key map (kbd "i") 'navi-mode)
    (define-key map (kbd "w") 'forward-word)
    (define-key map (kbd "W") 'backward-word)
    (define-key map (kbd "TAB") 'eyebrowse-last-window-config)
    map)
  "Keymap for `navi-mode'.")

;;;###autoload
(define-minor-mode navi-mode
  "A minor mode for navigating the buffer."
  :init-value nil
  :lighter " navi"
  :keymap navi-mode-map
  (if navi-mode
      (message "Navi Mode activated")
    (message "Navi Mode deactivated")))

;;;###autoload
(defun toggle-navi-mode ()
  "Toggle `navi-mode'."
  (interactive)
  (if navi-mode
      (navi-mode -1)
    (navi-mode 1)))

(global-set-key (kbd "<escape>") 'toggle-navi-mode)
;; (global-set-key (kbd "C-m") 'toggle-navi-mode)

(provide 'navi)

;;; navi.el ends here
