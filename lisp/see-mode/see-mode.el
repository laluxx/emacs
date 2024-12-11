;;; see-mode.el --- A custom C programming mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Keywords: languages, c
;; Version: 0.5
;; Package-Requires: ((emacs "26.1") (flycheck "32") (yasnippet "0.14.0"))

;;; Commentary:

;; TITLE [6/11] 
;; [x] DONE Color ! with the `font-lock-negation-char-face'
;; [x] DONE Add support for header file auto-completion (if they are called the same)
;; [x] DONE Implement `electric-pair-mode'
;; [x] DONE Continue the comment when pressing `C-j' implement `see-electric-newline'
;; [x] DONE Color # using `font-lock-preprocessor-face'
;; [ ] TODO Ensure it takes over `c-mode'
;; [ ] TODO M-. should work types that we wrote, it should also try to find it anyways
;; [ ] TODO if `hide-comments-mode' is installed enable it by default if not don't 
;; [ ] TODO Add snippet expansion support
;; [ ] TODO Add support for semantic parsing and smart completion
;; [ ] TODO auto pair `' in comments and buttonize them to jump to definition or whatever
;; [ ] TODO Be smart about TODO's chains like this one it could even  
;;          hold a position or a function name to jump to when pressed

;;; Code:

(use-package yasnippet
  :ensure t)

(require 'prog-mode)
(require 'flycheck)
(require 'yasnippet)

(defgroup see-mode nil
  "Major mode for editing C source code."
  :group 'languages
  :prefix "see-")

(defface see-builtin-type-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face for C built-in types.")

(defface see-library-type-face
  '((t (:inherit font-lock-type-face :slant italic)))
  "Face for types imported from libraries.")

(defface see-user-type-face
  '((t (:inherit font-lock-type-face :underline t)))
  "Face for user-defined types.")

(defvar see-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?! "." table)
    table)
  "Syntax table for `see-mode'.")

(defconst see-keywords
  '("auto" "break" "case" "const" "continue" "default" "do"
    "else" "enum" "extern" "for" "goto" "if" "inline"
    "register" "restrict" "return" "sizeof" "static" "struct"
    "switch" "typedef" "union" "volatile" "while")
  "C keywords for syntax highlighting.")

(defconst see-builtin-types
  '("void" "char" "short" "int" "long" "float" "double"
    "signed" "unsigned")
  "C built-in types for syntax highlighting.")

(defconst see-library-types
  '("_Bool" "_Complex" "_Imaginary" "size_t" "ssize_t" "intptr_t" "uintptr_t" "ptrdiff_t"
    "int8_t" "int16_t" "int32_t" "int64_t" "uint8_t" "uint16_t" "uint32_t" "uint64_t"
    "int_least8_t" "int_least16_t" "int_least32_t" "int_least64_t"
    "uint_least8_t" "uint_least16_t" "uint_least32_t" "uint_least64_t"
    "int_fast8_t" "int_fast16_t" "int_fast32_t" "int_fast64_t"
    "uint_fast8_t" "uint_fast16_t" "uint_fast32_t" "uint_fast64_t"
    "intmax_t" "uintmax_t" "FILE" "fpos_t" "DIR" "va_list"
    "time_t" "clock_t" "struct tm" "wchar_t" "wint_t" "wctype_t"
    "pthread_t" "pthread_attr_t" "pthread_mutex_t" "pthread_cond_t"
    "atomic_bool" "atomic_char" "atomic_schar" "atomic_uchar"
    "atomic_short" "atomic_ushort" "atomic_int" "atomic_uint"
    "atomic_long" "atomic_ulong" "atomic_llong" "atomic_ullong"
    "atomic_intptr_t" "atomic_uintptr_t" "atomic_size_t" "atomic_ptrdiff_t"
    "atomic_intmax_t" "atomic_uintmax_t" "bool" "true" "false")
  "Types imported from C libraries for syntax highlighting.")

(defconst see-constants
  '("NULL" "EOF")
  "Common C constants.")

(defvar see-font-lock-keywords
  `(
    ("^\\s-*\\(#\\)\\s-*\\([a-zA-Z_]+\\)" (1 font-lock-preprocessor-face) (2 font-lock-preprocessor-face))
    ("!" . font-lock-negation-char-face)
    (,(regexp-opt see-keywords 'words) . font-lock-keyword-face)
    (,(regexp-opt see-builtin-types 'words) . 'see-builtin-type-face)
    (,(regexp-opt see-library-types 'words) . 'see-library-type-face)
    ("\\<[A-Z][a-zA-Z0-9_]*\\>" . 'see-user-type-face)
    (,(regexp-opt see-constants 'words) . font-lock-constant-face)
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>" 1 font-lock-variable-name-face)
    )
  "Highlighting expressions for `see-mode'.")

(defun see-indent-line ()
  "Indent current line for `see-mode'."
  (interactive)
  (let ((indent 0)
        (pos (- (point-max) (point))))
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          (setq indent 0)
        (let ((line-start (point))
              prev-indent prev-line-start)
          (forward-line -1)
          (setq prev-indent (current-indentation)
                prev-line-start (point))
          (goto-char line-start)
          (cond
           ((looking-at "^\\s-*}")
            (setq indent (- prev-indent tab-width)))
           ((looking-at "^\\s-*{")
            (setq indent prev-indent))
           (t
            (setq indent prev-indent)
            (save-excursion
              (goto-char prev-line-start)
              (when (looking-at ".*{\\s-*$")
                (setq indent (+ indent tab-width)))))))))
    (when (< indent 0)
      (setq indent 0))
    (if (> (current-column) (current-indentation))
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))
    (when (> (- (point-max) pos) (point))
      (goto-char (- (point-max) pos)))))

(defun see-electric-newline ()
  "Insert a newline and continue the comment if in a comment."
  (interactive)
  (let ((in-comment (nth 4 (syntax-ppss))))
    (newline-and-indent)
    (when in-comment
      (insert "// ")
      (indent-according-to-mode))))

(defvar see-electric-pairs '((?{ . ?}) (?\( . ?\)) (?\[ . ?\]) (?< . ?>))
  "Electric pairs for `see-mode'.")

(defun see-electric-pair-inhibit (char)
  "Prevent electric pairing of quotes in comments and strings."
  (or (char-equal char ?\')
      (char-equal char ?\")
      (let ((syntax (syntax-ppss)))
        (or (nth 3 syntax) (nth 4 syntax)))))

(defun see-header-file-completion ()
  "Provide completion for header files."
  (when (looking-back "#include *[<\"]\\([^>\"]*\\)" (line-beginning-position))
    (let* ((start (match-beginning 1))
           (end (match-end 1))
           (prefix (buffer-substring-no-properties start end))
           (dir (if (eq (char-before start) ?<) "/usr/include/" default-directory))
           (files (directory-files dir nil ".*\\.h$")))
      (list start end files))))

(flycheck-define-checker see-gcc
  "A C syntax checker using gcc."
  :command ("gcc" "-fsyntax-only" "-Wall" "-Wextra" source)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column ": " (or "fatal error" "error") ": " (message) line-end))
  :modes see-mode)

(add-to-list 'flycheck-checkers 'see-gcc)

(defvar see-mode-hook nil
  "Hook run when entering SEE mode.")

;;;###autoload
(define-derived-mode see-mode prog-mode "SEE"
  "Major mode for editing C code with enhanced type highlighting."
  :syntax-table see-mode-syntax-table

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local indent-line-function 'see-indent-line)
  (setq-local font-lock-defaults '(see-font-lock-keywords nil nil nil nil))
  (setq-local electric-pair-pairs see-electric-pairs)
  (setq-local electric-pair-text-pairs see-electric-pairs)
  (setq-local electric-pair-inhibit-predicate #'see-electric-pair-inhibit)
  (setq-local parse-sexp-ignore-comments t)

  (electric-pair-local-mode 1)
  (add-hook 'completion-at-point-functions #'see-header-file-completion nil t)
  (yas-minor-mode 1)

  ;; Set up keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'comment-region)
    (define-key map (kbd "C-c C-u") 'uncomment-region)
    (define-key map (kbd "C-j") 'see-electric-newline)
    (use-local-map map)))

(defun see-mode-install ()
  "Install see-mode and ensure it takes precedence over c-mode."
  (interactive)
  (setq auto-mode-alist
        (cl-remove-if (lambda (pair)
                        (and (string-match "\\.c\\'" (car pair))
                             (eq (cdr pair) 'c-mode)))
                      auto-mode-alist))
  (setq auto-mode-alist
        (cl-remove-if (lambda (pair)
                        (and (string-match "\\.h\\'" (car pair))
                             (eq (cdr pair) 'c-mode)))
                      auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.c\\'" . see-mode) t)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . see-mode) t)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eq major-mode 'c-mode)
                 (or (string-match "\\.c\\'" (buffer-file-name))
                     (string-match "\\.h\\'" (buffer-file-name))))
        (see-mode)))))

(see-mode-install)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.c\\'" . see-mode) t)
(add-to-list 'auto-mode-alist '("\\.h\\'" . see-mode) t)

(provide 'see-mode)

;;; see-mode.el ends here
