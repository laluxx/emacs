;;; unmapped-keys.el --- Display available key combinations -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laluxx

;; Author: Laluxx
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, keybindings
;; URL: https://github.com/Laluxx/unmapped-keys

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a way to find unmapped keychords for any buffer.
;; M-x unmapped-keys-show to display all available key combinations.
;; Navigate between sections using 'n' and 'p' keys.

;;; Code:

(defgroup unmapped-keys nil
  "Display available key combinations."
  :group 'convenience
  :prefix "unmapped-keys-")

(defface unmapped-keys-header-face
  '((t :inherit font-lock-keyword-face :height 1.3 :weight bold))
  "Face for main headers.")

(defface unmapped-keys-subheader-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for combination group headers.")

(defface unmapped-keys-key-face
  '((t :inherit font-lock-constant-face))
  "Face for key combinations.")

(defface unmapped-keys-status-face
  '((t :inherit font-lock-comment-face))
  "Face for binding status.")

(defvar unmapped-keys-buffer-name "*Unmapped Key Combinations*"
  "Name of the buffer for displaying unmapped key combinations.")

(defvar-local unmapped-keys-source-buffer nil
  "Buffer for which we're showing unmapped key combinations.")

(defvar unmapped-keys-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'unmapped-keys-refresh)
    (define-key map (kbd "n") 'unmapped-keys-next-section)
    (define-key map (kbd "p") 'unmapped-keys-previous-section)
    (define-key map (kbd "RET") 'unmapped-keys-copy-combination)
    (define-key map (kbd "f") 'unmapped-keys-toggle-filter)
    (define-key map (kbd "t") 'unmapped-keys-test-combination)
    (define-key map (kbd "?") 'unmapped-keys-show-help)
    map)
  "Keymap for unmapped keys buffer.")

(define-derived-mode unmapped-keys-mode special-mode "Unmapped-Keys"
  "Major mode for displaying unmapped key combinations.
\\{unmapped-keys-mode-map}"
  :group 'unmapped-keys
  (setq truncate-lines t)
  (setq-local line-spacing 0.2))

(defun unmapped-keys-insert-with-face (text face)
  "Insert TEXT with specified FACE."
  (let ((start (point)))
    (insert text)
    (put-text-property start (point) 'face face)))

(defun unmapped-keys-get-combinations (buffer)
  "Get a list of all unmapped key combinations in BUFFER."
  (with-current-buffer buffer
    (let ((all-chars (append (number-sequence ?a ?z)
                             (number-sequence ?A ?Z)
                             (number-sequence ?0 ?9)))
          (modifiers '("" "C-" "M-" "s-" "H-" "C-M-" "C-s-" "M-s-" "C-M-s-"
                       "S-" "C-S-" "M-S-" "C-M-S-"))
          (result nil)
          (special-keys '("<left>" "<right>" "<up>" "<down>"
                          "<home>" "<end>" "<prior>" "<next>"
                          "<insert>" "<delete>" "<escape>"
                          "RET" "TAB" "SPC"))
          (punct-chars "!@#$%^&*()_+{}|:\"<>?`~-=[];',./"))
      
      ;; Check regular chars and punctuation
      (dolist (char (append all-chars (append punct-chars nil)))
        (dolist (mod modifiers)
          (let* ((key-str (format "%s%c" mod char))
                 (key-vec (kbd key-str)))
            (unless (key-binding key-vec)
              (push key-str result)))))
      
      ;; Check function keys
      (dotimes (i 12)
        (let ((fkey (format "<f%d>" (1+ i))))
          (dolist (mod modifiers)
            (let* ((key-str (format "%s%s" mod fkey))
                   (key-vec (kbd key-str)))
              (unless (key-binding key-vec)
                (push key-str result))))))
      
      ;; Check special keys
      (dolist (special special-keys)
        (dolist (mod modifiers)
          (let* ((key-str (format "%s%s" mod special))
                 (key-vec (kbd key-str)))
            (unless (key-binding key-vec)
              (push key-str result)))))
      
      (sort result #'string<))))

(defun unmapped-keys-refresh ()
  "Refresh the unmapped keys buffer."
  (interactive)
  (when unmapped-keys-source-buffer
    (let ((combinations (unmapped-keys-get-combinations unmapped-keys-source-buffer)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Main header
        (unmapped-keys-insert-with-face "Unmapped Key Combinations\n" 'unmapped-keys-header-face)
        (insert "======================\n\n")
        
        ;; Buffer info
        (insert "Source Buffer: ")
        (unmapped-keys-insert-with-face (buffer-name unmapped-keys-source-buffer) 'unmapped-keys-key-face)
        (insert "\nMajor Mode:   ")
        (unmapped-keys-insert-with-face 
         (symbol-name (with-current-buffer unmapped-keys-source-buffer major-mode))
         'unmapped-keys-key-face)
        (insert "\n\n")
        
        ;; Keys help
        (unmapped-keys-insert-with-face "Keys:\n" 'unmapped-keys-subheader-face)
        (insert "  q   - quit\n")
        (insert "  g   - refresh\n")
        (insert "  n/p - next/previous section\n")
        (insert "  f   - toggle filters\n")
        (insert "  t   - test combination\n")
        (insert "  RET - copy combination at point\n")
        (insert "  ?   - show help\n\n")
        
        ;; Group combinations by modifier prefix
        (let ((groups (make-hash-table :test 'equal)))
          (dolist (combo combinations)
            (let* ((parts (split-string combo "-"))
                   (mod (if (= (length parts) 1)
                            ""
                          (concat (mapconcat 'identity (butlast parts) "-") "-"))))
              (push combo (gethash mod groups nil))))
          
          ;; Display grouped combinations
          (maphash
           (lambda (mod keys)
             (when keys
               ;; Insert section header with face
               (insert "\n")
               (unmapped-keys-insert-with-face
                (format "%s combinations:\n"
                        (if (string-empty-p mod) "Unmodified" (substring mod 0 -1)))
                'unmapped-keys-subheader-face)
               (insert (make-string (+ (length mod) 13) ?-))
               (insert "\n")
               ;; Insert keys with faces
               (dolist (key (sort keys #'string<))
                 (unmapped-keys-insert-with-face (format "%-15s" key) 'unmapped-keys-key-face)
                 (unmapped-keys-insert-with-face " (no binding)\n" 'unmapped-keys-status-face))))
           groups)))
      (goto-char (point-min)))))

(defun unmapped-keys-next-section ()
  "Move to the next section in the buffer."
  (interactive)
  (forward-char)
  (if (re-search-forward "^[A-Z][A-Za-z- ]+ combinations:$" nil t)
      (beginning-of-line)
    (goto-char (point-min))))

(defun unmapped-keys-previous-section ()
  "Move to the previous section in the buffer."
  (interactive)
  (forward-line -1)
  (if (re-search-backward "^[A-Z][A-Za-z- ]+ combinations:$" nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (re-search-backward "^[A-Z][A-Za-z- ]+ combinations:$" nil t)))

(defun unmapped-keys-copy-combination ()
  "Copy the key combination at point to the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^\\([^ ]+\\)" (line-end-position) t)
      (let ((combination (match-string 1)))
        (kill-new combination)
        (message "Copied: %s" combination)))))

(defun unmapped-keys-test-combination ()
  "Test the key combination at point interactively."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^\\([^ ]+\\)" (line-end-position) t)
      (let* ((key-str (match-string 1))
             (key-vec (kbd key-str)))
        (with-current-buffer unmapped-keys-source-buffer
          (message "Press %s to test... (C-g to cancel)" key-str)
          (let ((event (read-event)))
            (message "Key %s %s"
                     key-str
                     (if (key-binding (vector event))
                         "is now bound"
                       "is still unbound"))))))))

(defun unmapped-keys-toggle-filter ()
  "Toggle filtering of key combinations."
  (interactive)
  (let* ((choices '("Show all" "Only C- combinations" "Only M- combinations"
                    "Only C-M- combinations" "Only function keys" "Only special keys"))
         (choice (completing-read "Filter: " choices nil t)))
    (unmapped-keys-refresh)))

(defun unmapped-keys-show-help ()
  "Show help for unmapped-keys mode."
  (interactive)
  (with-help-window (help-buffer)
    (princ "Unmapped Keys Mode\n")
    (princ "=================\n\n")
    (princ "Available commands:\n\n")
    (princ "  n      - Move to next section\n")
    (princ "  p      - Move to previous section\n")
    (princ "  g      - Refresh the buffer\n")
    (princ "  q      - Quit window\n")
    (princ "  RET    - Copy key combination at point\n")
    (princ "  f      - Filter combinations\n")
    (princ "  t      - Test key combination at point\n")
    (princ "  ?      - Show this help\n\n")
    (princ "This mode shows all unmapped key combinations in the source buffer.\n")
    (princ "You can use these combinations for new key bindings in your Emacs configuration.")))

;;;###autoload
(defun unmapped-keys-show ()
  "Show buffer with all unmapped key combinations."
  (interactive)
  (let ((source (current-buffer)))
    (with-current-buffer (get-buffer-create unmapped-keys-buffer-name)
      (unmapped-keys-mode)
      (setq unmapped-keys-source-buffer source)
      (unmapped-keys-refresh))
    (switch-to-buffer-other-window unmapped-keys-buffer-name)
    (goto-char (point-min))))

(provide 'unmapped-keys)
;;; unmapped-keys.el ends here
