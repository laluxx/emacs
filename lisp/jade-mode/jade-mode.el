;;; jade-mode.el --- Major mode for editing Jade programming language files -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Keywords: languages
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/laluxx/jade-mode

;;; Commentary:

;; This package provides a major mode for editing Jade programming language files.
;; Features include syntax highlighting, indentation, and navigation support.

;;; Code:

(require 'cl-lib)
(require 'rx)

(defgroup jade nil
  "Major mode for editing Jade source code."
  :group 'languages
  :prefix "jade-")

(defcustom jade-indent-offset 4
  "Number of spaces for each indentation step in `jade-mode'."
  :type 'integer
  :safe 'integerp
  :group 'jade)

;; Define keyword lists
(defconst jade-keywords
  '("fn" "type" "impl" "let" "rec" "match" "return" "if" "else"
    "for" "while" "obj" "asm" "struct" "self"))

(defconst jade-types
  '("i32" "f32" "str" "char" "Void" "Point" "Camera" "Player"))

(defconst jade-constants
  '("true" "false" "null" "NONE"))

;; Create the regex for different syntax components
(defconst jade-font-lock-keywords
  (let ((kw-re (regexp-opt jade-keywords 'words))
        (type-re (regexp-opt jade-types 'words))
        (const-re (regexp-opt jade-constants 'words)))
    `(
      ;; Type definitions
      ("\\<type\\s-+\\([A-Z][A-Za-z0-9_]*\\)"
       (1 font-lock-type-face))
      
      ;; Function definitions
      ("\\<fn\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (1 font-lock-function-name-face))
      
      ;; Implementation blocks
      ("\\<impl\\s-+\\([A-Z][A-Za-z0-9_]*\\)"
       (1 font-lock-type-face))
      
      ;; Keywords
      (,kw-re . font-lock-keyword-face)
      
      ;; Built-in types
      (,type-re . font-lock-type-face)
      
      ;; Constants
      (,const-re . font-lock-constant-face)
      
      ;; Attributes
      ("#\\[.*?\\]" . font-lock-preprocessor-face)
      
      ;; Numbers
      ("\\<[0-9]+\\(\\.[0-9]+\\)?\\(f\\|i32\\)?\\>" . font-lock-constant-face)
      
      ;; String literals
      ("\"[^\"]*\"" . font-lock-string-face)
      
      ;; Comments
      ("//.*$" . font-lock-comment-face)
      )))

(defvar jade-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comment syntax
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)
    
    ;; String syntax
    (modify-syntax-entry ?\" "\"" table)
    
    ;; Operator symbols
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    
    ;; Underscore can be part of a word
    (modify-syntax-entry ?_ "w" table)
    
    table))

(defun jade-indent-line ()
  "Indent current line as Jade code."
  (interactive)
  (let ((indent-col 0)
        (pos (- (point-max) (point)))
        beg)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (skip-chars-forward " \t")
      (if (not (or (looking-at "}")
                   (looking-at ")")))
          (save-excursion
            (if (jade--previous-indentation)
                (setq indent-col (+ (current-indentation)
                                    (if (or (looking-at "{")
                                            (looking-at "("))
                                        jade-indent-offset 0)))
              (setq indent-col 0)))))
    (if (> indent-col 0)
        (indent-line-to indent-col))
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

(defun jade--previous-indentation ()
  "Return the indentation of the previous non-empty line."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (or (looking-at "^[ \t]*$")
                    (looking-at "^[ \t]*//.*$")))
      (forward-line -1))
    (if (bobp)
        0
      (current-indentation))))

;;;###autoload
(define-derived-mode jade-mode prog-mode "Jade"
  "Major mode for editing Jade programming language code."
  :syntax-table jade-mode-syntax-table
  (setq-local font-lock-defaults '(jade-font-lock-keywords))
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local indent-line-function 'jade-indent-line)
  
  ;; For block comments
  (setq-local comment-start-skip "/[*/]+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*+/\\)")
  
  ;; Indent settings
  (setq-local indent-tabs-mode nil)
  
  ;; For electric-pair-mode
  (setq-local electric-indent-chars
              (append "{}()[],;" electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode))

(provide 'jade-mode)

;;; jade-mode.el ends here
