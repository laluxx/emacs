;;; unicode-tables.el --- Convert ASCII tables to Unicode -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: Laluxx
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience
;; URL: https://github.com/laluxx/unicode-tables

;;; Commentary:

;; TODO [ ] It doesn't work 
;; TODO [ ] It should be a global `minor-mode'
;; independent and not depend on `org'

;; Converts ASCII tables to pretty Unicode tables in any buffer.
;; Works with read-only buffers and calfw calendars.

;;; Code:

(defgroup unicode-tables nil
  "Unicode table conversion settings."
  :group 'editing)

(defvar unicode-tables-symbols
  '(("+" . "┼")
    ("-" . "─")
    ("|" . "│")
    ("," . "╭")
    ("." . "╮")
    ("`" . "╰")
    ("'" . "╯"))
  "Mapping of ASCII characters to Unicode box-drawing characters.")

(defun unicode-tables-replace-region (start end)
  "Replace ASCII table characters with Unicode in region from START to END."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char start)
      ;; First row special treatment
      (when (looking-at-p ".*[+][-+]+[+].*")
        (let ((line (buffer-substring-no-properties 
                     (line-beginning-position) 
                     (line-end-position))))
          (delete-region (line-beginning-position) (line-end-position))
          (insert (unicode-tables--convert-first-row line))))
      
      ;; Process remaining lines
      (while (< (point) end)
        (forward-line)
        (when (looking-at-p ".*[|+][-|+]+[|+].*")
          (let ((line (buffer-substring-no-properties 
                       (line-beginning-position) 
                       (line-end-position))))
            (delete-region (line-beginning-position) (line-end-position))
            (insert (unicode-tables--convert-line line))))))))

(defun unicode-tables--convert-first-row (line)
  "Convert the first row of an ASCII table LINE to Unicode."
  (let ((chars (string-to-list line))
        (prev-char nil)
        (result ""))
    (dolist (char chars)
      (let ((char-str (char-to-string char)))
        (setq result
              (concat result
                      (cond
                       ;; First +
                       ((and (string= char-str "+")
                             (not prev-char))
                        "╭")
                       ;; Last +
                       ((and (string= char-str "+")
                             (string= prev-char "-"))
                        "╮")
                       ;; Middle +
                       ((string= char-str "+")
                        "┬")
                       ;; Horizontal lines
                       ((string= char-str "-")
                        "─")
                       ;; Everything else
                       (t char-str))))
        (setq prev-char char-str)))
    result))

(defun unicode-tables--convert-line (line)
  "Convert an ASCII table LINE to Unicode."
  (let ((chars (string-to-list line))
        (prev-char nil)
        (first-char t)
        (result ""))
    (dolist (char chars)
      (let ((char-str (char-to-string char)))
        (setq result
              (concat result
                      (cond
                       ;; First |
                       ((and first-char
                             (string= char-str "|"))
                        "│")
                       ;; Last |
                       ((and (string= char-str "|")
                             (string= prev-char " "))
                        "│")
                       ;; Middle +
                       ((string= char-str "+")
                        "┼")
                       ;; Horizontal lines
                       ((string= char-str "-")
                        "─")
                       ;; Vertical lines
                       ((string= char-str "|")
                        "│")
                       ;; Everything else
                       (t char-str))))
        (setq prev-char char-str
              first-char nil)))
    result))

(defun unicode-tables-convert-at-point ()
  "Convert ASCII table at point to Unicode."
  (interactive)
  (save-excursion
    (when (looking-at-p ".*[|+][-|+]+[|+].*")
      ;; Find table boundaries
      (let (start end)
        ;; Find start
        (while (and (not (bobp))
                    (looking-at-p ".*[|+][-|+]+[|+].*"))
          (forward-line -1))
        (unless (looking-at-p ".*[|+][-|+]+[|+].*")
          (forward-line 1))
        (setq start (point))
        
        ;; Find end
        (while (and (not (eobp))
                    (looking-at-p ".*[|+][-|+]+[|+].*"))
          (forward-line 1))
        (setq end (point))
        
        ;; Convert the table
        (unicode-tables-replace-region start end)))))

(defun unicode-tables-convert-buffer ()
  "Convert all ASCII tables in current buffer to Unicode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^.*[|+][-|+]+[|+].*$" nil t)
      (unicode-tables-convert-at-point))))

;;;###autoload
(define-minor-mode unicode-tables-mode
  "Convert ASCII tables to Unicode automatically."
  :lighter " UTables"
  :global nil
  (if unicode-tables-mode
      (progn
        (unicode-tables-convert-buffer)
        ;; Hook for calfw
        (when (derived-mode-p 'calfw-calendar-mode)
          (add-hook 'calfw-calendar-create-contents-hook 
                    #'unicode-tables-convert-buffer nil t)))
    (when (derived-mode-p 'calfw-calendar-mode)
      (remove-hook 'calfw-calendar-create-contents-hook 
                   #'unicode-tables-convert-buffer t))))

;;;###autoload
(define-globalized-minor-mode global-unicode-tables-mode
  unicode-tables-mode
  (lambda ()
    (unicode-tables-mode 1)))

(provide 'unicode-tables)
;;; unicode-tables.el ends here
