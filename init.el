;; [ ] TODO A list of commands where 'vertico-mode' should be disabled
;; [ ] TODO 'C-o' in read-only should fake opening a line by scrolling one line down
;; [ ] TODO color python 'nerd-icon'  with 2 colors Blue and Yellow somehow
;; [ ] TODO a minor-mode that automatically generate an .org file
;;     for the project gathering all the TODO's and interact witht the agenda
;;     maybe only for a list of project, and a manual way.
;; [ ] TODO 'consult-apropos-documentation'
;; [ ] TODO Make 'next-error-no-select' pulse the line
;; [ ] TODO custom *Messages* mode when you can click each line to copy it,
;; and custom line background depending if it is an error warning or success
;; use the correct faces, on the left show an icon for each of the 3
;; [ ] TODO in `see-mode' auto insert the library if i wrote enough of it and move the cursor to EOL
;; [ ] TODO trash-ring insert from the trash like it was a kill-ring inside `dired'
;; [ ] TODO dired-undo
;; [ ] TODO Now i have to make a mode for 0x0.st too 💀
;; [ ] TODO C-k in 'iedit-mode' should kill until the last char of the iedit
;; [ ] TODO a version of `iedit' that works on all visible windows
;; [ ] TODO IF we are in `fundemental-mode'
;; AFTER we `yank' check| the first line of the buffer if a shebang is found
;; switch to the correct mode like `shell-script-mode' and `python-mode'
;; [ ] TODO when we enter dired move the point to the most recent
;; modified file in the file system, not the last point we were in in `save-place'
;; [ ] TODO `laluxx/mark-scope' function for `c-mode'
;; [ ] TODO show the value of point in the modeline
;; [ ] TODO smart `delete-indentation' that join comments nicely
;; [ ] TODO take a look into `edebug'
;; [ ] TODO keep track of the last point you were
;;     for each info manual you start reading
;; [ ] TODO Configure Header2
;; [ ] TODO track the time spent on each info file and save it to a file
;;     and show it in the modeline
;; [ ] TODO `cli-modes' package on M-! parse the command
;; and do something special for each command slowly add things
;; until emacs know how to do anything, for programs that output something
;; make sure to make a major mode for each cli that output
;; add syntax highlighting keys..
;; after one command output make a function to pipe
;; that into another thing withotu rewriting it

;;[ ] TODO bind q in `read-only-mode' to `kill-current-buffer'

;;; Code:

;;; COMPILATION
(setq compilation-auto-jump-to-first-error t)
;; (setq compilation-scroll-output 'first-error)
;; (setq compilation-scroll-output 'nil)

(defun laluxx/save-and-compile ()
  "Save the current buffer and then compile.
For .lisp files, use custom Lisp interpreter, otherwise use default compile-command."
  (interactive)
  (save-buffer)
  (if (and buffer-file-name (string-match "\\.lisp$" buffer-file-name))
      (let ((original-command compile-command))
        (compile 
         (format "/home/l/xos/projects/zig/lyra/zig-out/bin/lyra -e \"%s\""
                 (with-current-buffer (current-buffer)
                   (buffer-string))))
        (setq compile-command original-command))
    (compile compile-command)))

(defun laluxx/save-and-compile ()
  "Save the current buffer and then compile without asking."
  (interactive)
  (save-buffer)  ; Save the current buffer
  (compile compile-command))

(global-set-key (kbd "C-x c") 'laluxx/save-and-compile)
;; (global-set-key (kbd "C-j") 'laluxx/save-and-compile)

(defun laluxx/save-and-set-compile ()
  "Save the current buffer and compile interactively."
  (interactive)
  (save-buffer)
  (call-interactively 'compile))

(global-set-key (kbd "C-x C-c") 'laluxx/save-and-set-compile)

(require 'compile)
;;[ ] TODO HERE
(defun laluxx/update-compilation-header ()
  "Update the header line with the number of errors, warnings, and successes in the compilation buffer."
  (when (derived-mode-p 'compilation-mode)  ; Ensure it's a compilation buffer
    (save-excursion
      (goto-char (point-min))
      (let ((errors (count-matches "^[^ \n].*[0-9]+:\\([0-9]+:\\)? error:"))
            (warnings (count-matches "^[^ \n].*[0-9]+:\\([0-9]+:\\)? warning:"))
            (successes (count-matches "build successful")))  ; Customize the success message pattern as needed
        (setq header-line-format
              (concat
               (propertize (format "Errors: %d" errors) 'face 'compilation-error) ", "
               (propertize (format "Warnings: %d" warnings) 'face 'compilation-warning) ", "
               (propertize (format "Info: %d" successes) 'face 'compilation-info)))))))  ; Define or customize 'compilation-info' face as needed

(defun laluxx/enable-header-in-compilation ()
  "Enable custom header line in compilation mode."
  (setq header-line-format nil)  ; Clear any existing header line format
  (laluxx/update-compilation-header))

(add-hook 'compilation-mode-hook 'laluxx/enable-header-in-compilation)
(add-hook 'compilation-filter-hook 'laluxx/update-compilation-header)

(define-key compilation-mode-map (kbd "C-j") 'recompile)
(define-key compilation-mode-map (kbd "j") 'recompile)
(define-key compilation-mode-map (kbd "k") 'quit-window)
(define-key compilation-mode-map (kbd "c") 'recompile)
(define-key compilation-mode-map (kbd "y") 'laluxx/copy-buffer)


(setq user-full-name "Laluxx")

;;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;;; KEYBINDS

(global-set-key (kbd "C-c C-l") (lambda () (interactive) (insert "lambda "))) ;; TODO Make it msarte if it is on an empty line insert (lambda )

(global-set-key (kbd "M-C-1") 'eval-expression)
(global-set-key (kbd "M-e") 'forward-word)
(global-set-key (kbd "M-a") 'backward-word)
(global-set-key (kbd "C-h C-d") 'Info-goto-emacs-command-node)
(global-set-key (kbd "C-x i") 'package-install)
(global-set-key (kbd "C-x C-i") 'list-packages)
(global-set-key (kbd "C-c C-j") 'msh-toggle)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-s") 'consult-line)
(global-set-key (kbd "C-h C-l") 'find-library)
(global-set-key (kbd "C-h M-h") 'remove-hook)
(global-set-key (kbd "C-h C-b") 'button-describe)
(global-set-key (kbd "C-h C-v") 'set-variable)
(global-set-key (kbd "C-x C-g") 'find-grep)
(global-set-key (kbd "C-c p") 'beginning-of-buffer)
(global-set-key (kbd "C-c n") 'end-of-buffer)
(global-set-key (kbd "C-t") #'transpose-words)
(global-set-key (kbd "M-t") #'transpose-chars)
(global-set-key (kbd "C-S-d") 'kill-word)
(global-set-key (kbd "C-S-o") 'duplicate-line)
(global-set-key (kbd "C-c C-p") 'laluxx/find-package-source-code)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-h C-f") 'describe-face)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c i") 'consult-imenu)
(global-set-key (kbd "C-x f") 'consult-find)
(global-set-key (kbd "M-n") 'forward-paragraph) 
(global-set-key (kbd "M-p") 'backward-paragraph)

;;; MACROS
(global-set-key (kbd "C-M-m") 'kmacro-end-and-call-macro)
(global-set-key (kbd "C-M-S-m") 'kmacro-start-macro)

;;; THEMES

(use-package kaolin-themes
  :ensure t
  :config
  (setq kaolin-themes-modeline-border nil)
  (setq kaolin-themes-bold t
        kaolin-themes-italic t
        kaolin-themes-underline t)
  (load-theme 'kaolin-dark t))

(use-package ef-themes
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-bar-width 0)
  (custom-set-variables '(doom-modeline-workspace-name nil))
  ;; (setq doom-modeline-workspace-name nil)
  (doom-modeline-mode))


;;; EWAL
;; Force Emacs to use your system theme
(use-package ewal
  :ensure t
  :init
  (setq ewal-use-built-in-always-p nil
        ewal-use-built-in-on-failure-p t
        ewal-built-in-palette "sexy-material"))

;; Force your system to use the current Emacs theme
(use-package theme-magic
  :ensure t)


(use-package ewal-doom-themes
  :ensure t
  :init
  ;; If you've set ewal-use-built-in-always-p to nil in ewal configuration
  ;; This might be unnecessary, but doesn't hurt to ensure.
  (setq ewal-use-built-in-always-p nil
        ewal-use-built-in-on-failure-p t
        ewal-built-in-palette "sexy-material")
  :config
  ;; (load-theme 'ewal-doom-one t)
  )


;; Setup file watcher for `~/.cache/wal/colors'

(run-with-idle-timer
 1 nil
 (lambda ()
   (file-notify-add-watch
    "~/.cache/wal/colors"
    '(change)
    (lambda (event)
      (ewal-load-colors)  ; Add this line
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme 'ewal-doom-one t)
      (enable-theme 'ewal-doom-one)))))


;; NERD-ICONS

;; TODO Make me into a package
;; TODO when we are in dired and there is a .git directory show information at the bottom like dired-auto-readme does 

(defun laluxx/add-or-update-alist (alist-var key value)
  "Add or update KEY with VALUE in ALIST-VAR.
If KEY exists, its value is updated to VALUE.
If KEY doesn't exist, a new entry is added.
Returns the updated value."
  (setf (alist-get key (symbol-value alist-var)) value))

(defun laluxx/dired-nerd-icon-for-path (path)
  "Return appropriate icon and face for dired PATH."
  (cond
   ((string-match-p "/Downloads/?$" path)
    '("nf-md-folder_download" . nerd-icons-blue))
   ((string-match-p "/Documents/?$" path)
    '("nf-md-folder_file" . nerd-icons-yellow))
   ((string-match-p "/Pictures/?$" path)
    '("nf-md-folder_image" . nerd-icons-purple))
   ((string-match-p "/Music/?$" path)
    '("nf-md-folder_music" . nerd-icons-green))
   ((string-match-p "/Videos/?$" path)
    '("nf-md-folder_play" . nerd-icons-red))
   ((string-match-p "/Desktop/?$" path)
    '("nf-md-desktop_classic" . nerd-icons-silver))
   (t
    '("nf-md-folder" . nerd-icons-yellow))))

(use-package nerd-icons
  :ensure t
  :config
  (global-set-key (kbd "C-x 8 n") 'nerd-icons-insert)

  (laluxx/add-or-update-alist 'nerd-icons-dir-icon-alist
                              "\\.git"
                              '(nerd-icons-codicon "nf-cod-github_alt" :face font-lock-comment-face :weight bold))
  
  (laluxx/add-or-update-alist 'nerd-icons-extension-icon-alist
                              "ssa"
                              '(nerd-icons-mdicon "nf-md-cube_outline" :face nerd-icons-blue-alt))

  (laluxx/add-or-update-alist 'nerd-icons-extension-icon-alist
                              "mate"
                              '(nerd-icons-flicon "nf-linux-libreofficemath" :face nerd-icons-blue-alt))

  (laluxx/add-or-update-alist 'nerd-icons-extension-icon-alist
                              "s"
                              '(nerd-icons-sucicon "nf-seti-asm" :face nerd-icons-silver))


  (laluxx/add-or-update-alist 'nerd-icons-extension-icon-alist
                              "diff"
                              '(nerd-icons-octicon "nf-oct-diff" :face nerd-icons-green))

  ;; By name
  (laluxx/add-or-update-alist 'nerd-icons-regexp-icon-alist
                              "^places$"
                              '(nerd-icons-faicon "nf-fa-arrow_pointer" :fg default))
  (laluxx/add-or-update-alist 'nerd-icons-regexp-icon-alist
                              "^recentf$"
                              '(nerd-icons-faicon "nf-fa-file_arrow_up" :fg default))
  (laluxx/add-or-update-alist 'nerd-icons-regexp-icon-alist
                              "^history$"
                              '(nerd-icons-mdicon "nf-md-progress_clock" :face nerd-icons-blue-alt))



  (laluxx/add-or-update-alist 'nerd-icons-extension-icon-alist
                              "jade"
                              '(nerd-icons-pomicon "nf-pom-external_interruption" :face nerd-icons-yellow))


  (laluxx/add-or-update-alist 'nerd-icons-mode-icon-alist
                              'erc-mode
                              '(nerd-icons-mdicon "nf-md-chat_processing"))

  (laluxx/add-or-update-alist 'nerd-icons-mode-icon-alist
                              'hexl-mode
                              '(nerd-icons-mdicon "nf-md-hexadecimal" :face nerd-icons))

  (laluxx/add-or-update-alist 'nerd-icons-mode-icon-alist
                              'objdump-mode
                              '(nerd-icons-faicon "nf-fae-hexagon" :face nerd-icons-red))

  (laluxx/add-or-update-alist 'nerd-icons-mode-icon-alist
                              'haskell-interactive-mode
                              '(nerd-icons-faicon "nf-fa-undo" :face nerd-icons-red))


  (defun project-main-language (dir)
    "Detect main language of project in DIR."
    (cond
     ;; Systems languages
     ((directory-files dir nil "\\.\\(c\\|h\\)$") 'c)
     ((directory-files dir nil "\\.\\(cpp\\|hpp\\)$") 'cpp)
     ((directory-files dir nil "\\.rs$") 'rust)
     ((file-exists-p (expand-file-name "go.mod" dir)) 'go)
     ((directory-files dir nil "\\.zig$") 'zig)
     ;; Lisps
     ((directory-files dir nil "\\.el$") 'elisp)
     ((directory-files dir nil "\\.clj$") 'clojure)
     ((directory-files dir nil "\\.rkt$") 'racket)
     ;; Functional
     ((directory-files dir nil "\\.hs$") 'haskell)
     ((directory-files dir nil "\\.ml$") 'ocaml)
     ;; Scripting
     ((directory-files dir nil "\\.py$") 'python)
     ((directory-files dir nil "\\.rb$") 'ruby)
     ((directory-files dir nil "\\.lua$") 'lua)
     ((or (directory-files dir nil "\\.js$")
          (file-exists-p (expand-file-name "package.json" dir))) 'javascript)
     ((directory-files dir nil "\\.ts$") 'typescript)
     ;; JVM
     ((directory-files dir nil "\\.java$") 'java)
     ((directory-files dir nil "\\.scala$") 'scala)
     ((directory-files dir nil "\\.kt$") 'kotlin)
     ;; Others
     ((directory-files dir nil "\\.cr$") 'crystal)
     ((directory-files dir nil "\\.ex$") 'elixir)
     ((directory-files dir nil "\\.nim$") 'nim)
     (t 'default)))

  (defun get-language-face (lang)
    "Get face color based on project LANG."
    (pcase lang
      ;; Systems Languages
      ('c 'nerd-icons-blue)          ; C
      ('cpp 'nerd-icons-dblue)       ; C++
      ('rust 'nerd-icons-dred)       ; Rust
      ('go 'nerd-icons-lcyan)        ; Go
      ('zig 'nerd-icons-yellow)      ; Zig
      ('elisp 'nerd-icons-purple)    ; Emacs Lisp
      ('clojure 'nerd-icons-lgreen)  ; Clojure
      ('racket 'nerd-icons-red)      ; Racket
      ('haskell 'nerd-icons-lpurple) ; Haskell
      ('ocaml 'nerd-icons-dorange)   ; OCaml
      ('python 'nerd-icons-dgreen)   ; Python
      ('ruby 'nerd-icons-red)        ; Ruby
      ('lua 'nerd-icons-blue)        ; Lua
      ('javascript 'nerd-icons-yellow) ; JavaScript
      ('typescript 'nerd-icons-lblue) ; TypeScript
      ('java 'nerd-icons-dmaroon)    ; Java
      ('scala 'nerd-icons-dred)      ; Scala
      ('kotlin 'nerd-icons-lpurple)  ; Kotlin
      ('crystal 'nerd-icons-silver)  ; Crystal
      ('elixir 'nerd-icons-dpurple)  ; Elixir
      ('nim 'nerd-icons-lyellow)     ; Nim
      (_ 'nerd-icons-orange)))       ; Default - Orange

  (defun git-directory-p (dir)
    "Check if DIR has a .git subdirectory."
    (file-exists-p (expand-file-name ".git" dir)))

  (advice-add 'nerd-icons-icon-for-dir :around
              (lambda (orig-fn dir &rest args)
                (if (and dir (git-directory-p dir))
                    (apply #'nerd-icons-octicon "nf-oct-package" 
                           :face (get-language-face (project-main-language dir)) 
                           args)
                  (apply orig-fn dir args))))


  ;; This is the icons that `doom-modeline' display for `dired-mode'
  ;; Let's update it live as we move arround the file system.
  ;; Add advice to nerd-icons-icon-for-mode for dired-mode.
  (advice-add 'nerd-icons-icon-for-mode
              :around
              (lambda (orig-fn mode &rest args)
                (if (eq mode 'dired-mode)
                    (let* ((path default-directory)
                           (icon-info (laluxx/dired-nerd-icon-for-path path)))
                      (apply #'nerd-icons-mdicon 
                             (car icon-info)
                             :face (cdr icon-info)
                             args))
                  (apply orig-fn mode args))))
  
  (laluxx/add-or-update-alist 'nerd-icons-mode-icon-alist
                              'dired-mode
                              '(nerd-icons-mdicon "nf-md-folder" :face nerd-icons-yellow)); _->
  )



(use-package 0x0
  :ensure t)

;;; EDITING
(setq-default truncate-lines t)
(recentf-mode)
(savehist-mode)
(electric-pair-mode)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(save-place-mode t)
(recentf-mode)
(savehist-mode)

(defun laluxx/kill-string ()
  (let ((ppss (syntax-ppss)))
    (if (nth 3 ppss)  ;; Check if inside a string
        (let ((start (point)))
          (skip-syntax-forward "^\"")
          (kill-region start (point)))
      (kill-line))))

;; [ ] TODO Ignore comments
(defun laluxx/kill-wim ()
  "'kill-line' if no active region, otherwise 'kill-region'.
If point is inside a string, 'kill-string' instead.
If at beginning of word and preceded by /, kills all text until space or line end.
If at beginning of word, kills just that word.
Preserves }, ;, ), ], or ' ()' at the end of the line. Also preserves ); and ) { when together.
If line ends with multiple ), preserves all of them.
If cursor is directly before preserved characters, deletes them all.
If at beginning of line, kills the entire line including newline."
  (interactive)
  (cond
   ;; Handle region if active
   ((use-region-p)
    (call-interactively 'kill-region))
   
   ;; Handle strings
   ((nth 3 (syntax-ppss))
    (laluxx/kill-string))
   
   ;; Handle word beginning after slash
   ((and (not (bolp))
         (eq (char-before) ?/)
         (looking-at-p "\\<\\w"))
    (let ((start (point)))
      (skip-syntax-forward "w_")
      (kill-region start (point))))
   
   ;; Handle word beginning
   ((and (not (bolp))
         (looking-at-p "\\<\\w"))
    (kill-word 1))
   
   ;; Handle all other cases
   (t
    (let* ((end (save-excursion
                  (end-of-line)
                  (point)))
           (preserve-chars (list ?} ?\; ?\) ?\]))
           (rest-of-line (buffer-substring-no-properties (point) end)))
      (cond
       ;; If at beginning of line, kill entire line including newline
       ((bolp)
        (kill-whole-line))
       ;; Check for " ()" pattern first
       ((string-match "\\s-*().*$" rest-of-line)
        (kill-region (point) (+ (point) (match-beginning 0))))
       ;; Check for multiple closing parentheses at end of line
       ((and (> end (point))
             (string-match "\\()\\)+$" rest-of-line))
        (let ((parens-start (match-beginning 0)))
          (kill-region (point) (+ (point) parens-start))))
       ;; Check for ) { at end of line
       ((and (> end (+ (point) 2))
             (eq (char-before end) ?{)
             (eq (char-before (1- end)) ? )
             (eq (char-before (- end 2)) ?\)))
        (kill-region (point) (- end 3)))
       ;; Check for ); at end of line
       ((and (> end (+ (point) 1))
             (eq (char-before end) ?\;)
             (eq (char-before (1- end)) ?\)))
        (kill-region (point) (- end 2)))
       ;; Check for single special char at end
       ((and (> end (point))
             (memq (char-before end) preserve-chars))
        (kill-region (point) (1- end)))
       ;; Default case
       (t (kill-line)))))))

(global-set-key (kbd "C-k") 'laluxx/kill-wim)


;; Emacs has always been modal peasants
(defun insert-or (key action)
  "Bind KEY to insert itself or run ACTION on region."
  (global-set-key (kbd key)
                  `(lambda ()
                     (interactive)
                     (if (use-region-p)
                         (call-interactively ',action)
                       (insert ,key)))))

(insert-or "j" 'laluxx/generate-face-colors)
(insert-or "J" 'generate-face-definitions)
(insert-or "s" 'laluxx/kill-strings-in-region)
(insert-or "g" 'laluxx/google-this)
(insert-or "c" 'comment-dwim)
(insert-or "f" 'laluxx/format-region)

(defun laluxx/insert-or-copy-region ()
  "Insert the character 'y' if no active region, otherwise copy the region and reset `copied-line`."
  (interactive)
  (if (use-region-p)
      (progn
	(call-interactively 'kill-ring-save)
	(setq copied-line nil))
    (insert "y")))

(global-set-key (kbd "y") 'laluxx/insert-or-copy-region)

(defun laluxx/insert-or-eval-region ()
  "Insert the character 'e' if no active region, otherwise evaluate the region.
In .lisp files, use custom lisp evaluator, otherwise use default eval-region."
  (interactive)
  (if (use-region-p)
      (progn
        (if (and buffer-file-name (string-match "\\.lisp$" buffer-file-name))
            (call-interactively 'laluxx/misp-eval-region)
          (call-interactively 'eval-region))
        (deactivate-mark))
    (insert "e")))

(global-set-key (kbd "e") 'laluxx/insert-or-eval-region)

(defun laluxx/misp-eval-region (start end)
  "Evaluate the region using the misp command-line tool and display result as a message."
  (interactive "r")
  (let* ((region-content (buffer-substring-no-properties start end))
         (output (with-temp-buffer
                   (call-process "misp" nil t nil "-e" region-content)
                   (buffer-string))))
    (if (string-empty-p output)
        (message "No output from misp")
      (message "%s" (string-trim output)))))




;; TODO A list of modes where it should not indent
;; like Makefiles
(defun laluxx/yank-indent ()
  "Yank content and indent in programming modes only."
  (interactive)
  (let ((start (point)))
    (yank)
    (when (derived-mode-p 'prog-mode)
      (indent-region start (point)))))

(global-set-key (kbd "C-y") 'laluxx/yank-indent)

(use-package iedit
  :ensure t
  :config

  (with-eval-after-load 'iedit
    (set-face-attribute 'iedit-occurrence nil :inherit 'menu))
  
  (defun laluxx/iedit-forward-word()
    "Activate iedit-mode and go to the end of the current word."
    (interactive)
    (iedit-mode)
    (forward-word))

  (defun laluxx/iedit-backward-word()
    "Activate iedit-mode and go to the end of the current word."
    (interactive)
    (iedit-mode)
    (backward-word))

  (global-set-key (kbd "M-I") 'laluxx/iedit-backward-word)
  (global-set-key (kbd "M-i") 'laluxx/iedit-forward-word))

;;; TREESITTER

(setopt treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


;;; ORG-MODE
;; TODO Better org tables

;; (setq org-ellipsis " ")

(defun laluxx/org-auto-tangle-on-save()
  "Automatically tangle org file when saving if #+auto_tangle: t is present."
  (when (eq major-mode 'org-mode)
    (let ((case-fold-search t))  ; Make search case-insensitive
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+auto_tangle:[ \t]*t" nil t)
          (let ((org-confirm-babel-evaluate nil))
            (org-babel-tangle)))))))

(add-hook 'before-save-hook #'laluxx/org-auto-tangle-on-save)

(defun laluxx/org-move-to-begin-src ()
  "Move cursor to the line below #+BEGIN_SRC."
  (interactive)
  (let ((original-pos (point)))
    (search-backward "#+BEGIN_SRC")
    (forward-line 1)
    (when (and (evil-visual-state-p) (evil-visual-line))
      (evil-visual-select original-pos (point) 'line))))

(defun laluxx/org-move-to-end-src ()
  "Move cursor to the line above #+end_src."
  (interactive)
  (let ((original-pos (point)))
    (search-forward "#+end_src")
    (forward-line -1)
    (when (and (evil-visual-state-p) (evil-visual-line))
      (evil-visual-select original-pos (point) 'line))))

;; TODO bind 'laluxx/org-move-to-end-src' and 'laluxx/org-move-to-begin-src'
;; to C-e and C-a only when we are inside a src-block 

(require 'org-tempo)
(setq org-hide-emphasis-markers t)
(setq org-agenda-files '("~/org/"))
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-link-file-completion-list nil)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "•")))

(with-eval-after-load 'org
  ;; First, let's define wrapper functions for recursive movement
  (defun laluxx/org-metaright-recursive ()
    "Move subtree right, including children"
    (interactive)
    (org-metaright '(4)))  ; The '(4) simulates C-u prefix

  (defun laluxx/org-metaleft-recursive ()
    "Move subtree left, including children"
    (interactive)
    (org-metaleft '(4)))

  ;; Now we'll set up all your keybindings, replacing the relevant ones
  ;; with our new recursive versions
  (define-key org-mode-map (kbd "M-n") 'org-metadown)
  (define-key org-mode-map (kbd "M-p") 'org-metaup)
  (define-key org-mode-map (kbd "M-f") 'laluxx/org-metaright-recursive)  ; Modified
  (define-key org-mode-map (kbd "M-b") 'laluxx/org-metaleft-recursive)   ; Modified
  (define-key org-mode-map (kbd "M-N") 'org-shiftdown)
  (define-key org-mode-map (kbd "M-P") 'org-shiftup)
  (define-key org-mode-map (kbd "M-F") 'org-shiftright)
  (define-key org-mode-map (kbd "M-B") 'org-shiftleft)
  (define-key org-mode-map (kbd "C-c C-n") 'laluxx/org-next-visible-heading-and-recenter)
  (define-key org-mode-map (kbd "C-c C-p") 'laluxx/org-previous-visible-heading-and-recenter)

  (setq org-startup-indented t
	org-enforce-todo-dependencies t
	org-priority-faces
	'((?A . error)
	  (?B . warning)
	  (?C . success))
	org-enforce-todo-dependencies t))

(defun laluxx/in-haskell-src-block-p ()
  "Check if point is in a haskell source block."
  (let ((case-fold-search t))
    (save-excursion
      (beginning-of-line)
      (or (looking-at "^[ \t]*#\\+begin_src[ \t]+haskell")  ; Check if we're on begin_src line
          (save-excursion                                    ; Or between begin_src and end_src
            (re-search-backward "^[ \t]*#\\+\\(begin\\|end\\)_src" nil t)
            (and (looking-at "^[ \t]*#\\+begin_src[ \t]+haskell")
                 (re-search-forward "^[ \t]*#\\+end_src" nil t)))))))

(defun laluxx/update-haskell-keys ()
  "Update C-j binding based on whether we're in a haskell block."
  (if (laluxx/in-haskell-src-block-p)
      (local-set-key (kbd "C-j") 'laluxx/haskell-smart-newline)
    (local-set-key (kbd "C-j") 'org-return-and-maybe-indent)))

;; Add to post-command-hook in org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'post-command-hook #'laluxx/update-haskell-keys nil t)))


;;; COMPLETION

(setq completion-auto-select t)
(setq completion-show-help nil)
(setq enable-recursive-minibuffers t)
(setq tab-always-indent 'complete)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(defvar laluxx/completing-read-excluded-commands
  '(consult-dark-themes consult-light-themes consult-ugly-theme consult-line load-theme consult-theme)
  "List of commands where completing-read should not use default value automatically.")

(defun laluxx/completing-read-auto-default (orig-fun prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Advice for `completing-read' to instantly use default value if available.
Disabled for commands in `laluxx/completing-read-excluded-commands'."
  (if (and def 
           (null initial-input)
           (not (memq this-command laluxx/completing-read-excluded-commands)))
      def
    (funcall orig-fun prompt collection predicate require-match initial-input hist def inherit-input-method)))

(advice-add 'completing-read :around #'laluxx/completing-read-auto-default)



;; (defun laluxx/completing-read-auto-default (orig-fun prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
;;   "Advice for `completing-read' to instantly use default value if available."
;;   (if (and def (null initial-input))
;;       def  ; Return default instantly if there's a default and no initial input
;;     (funcall orig-fun prompt collection predicate require-match initial-input hist def inherit-input-method)))

;; (advice-add 'completing-read :around #'laluxx/completing-read-auto-default)



(use-package nerd-icons-completion
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; CORFU

(use-package corfu-terminal
  :ensure t
  :config 
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package corfu
  ;; Optional customization's
  :custom


  (completion-highlight-region-function nil)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-bar-width 0.3)

  :ensure t
  :init
  (corfu-history-mode)
  (global-corfu-mode))


(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer."
  (when (local-variable-p 'completion-at-point-functions)
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)



;; (setq corfu-auto        t
;;       corfu-auto-delay  0 ;; Note: A delay of 0 is typically not recommended as it can be disruptive.
;;       corfu-auto-prefix 1 ;; Note: A prefix of 1 is also generally considered too small.
;;       completion-styles '(basic))


;; TODO Make this a function so its easy to activate on modes i want
;; (setq-local corfu-auto        t
;;             corfu-auto-delay  0 ;; TOO SMALL - NOT RECOMMENDED
;;             corfu-auto-prefix 1 ;; TOO SMALL - NOT RECOMMENDED
;;             completion-styles '(basic))

(use-package nerd-icons-corfu
  :ensure t
  :config

  ;; OPTIONALLY:
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
          (color :style "cod" :icon "symbol_color" :face success)
          (command :style "cod" :icon "terminal" :face default)
          (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
          (constructor :style "cod" :icon "triangle_right" :face font-lock-function-name-face)
          (enummember :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
          (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
          (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
          (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
          (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
          (file :style "cod" :icon "symbol_file" :face font-lock-string-face)
          (folder :style "cod" :icon "folder" :face font-lock-doc-face)
          (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
          (keyword :style "cod" :icon "symbol_keyword" :face font-lock-keyword-face)
          (macro :style "cod" :icon "symbol_misc" :face font-lock-keyword-face)
          (magic :style "cod" :icon "wand" :face font-lock-builtin-face)
          (method :style "cod" :icon "symbol_method" :face success)
          (function :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
          (module :style "cod" :icon "file_submodule" :face font-lock-preprocessor-face)
          (numeric :style "cod" :icon "symbol_numeric" :face font-lock-builtin-face)
          (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
          (param :style "cod" :icon "symbol_parameter" :face default)
          (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
          (reference :style "cod" :icon "references" :face font-lock-variable-name-face)
          (snippet :style "cod" :icon "symbol_snippet" :face font-lock-string-face)
          (string :style "cod" :icon "symbol_string" :face font-lock-string-face)
          (struct :style "cod" :icon "symbol_structure" :face font-lock-variable-name-face)
          (text :style "cod" :icon "text_size" :face font-lock-doc-face)
          (typeparameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
          (type-parameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
          (unit :style "cod" :icon "symbol_ruler" :face font-lock-constant-face)
          (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
          (variable :style "cod" :icon "symbol_variable" :face font-lock-variable-name-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face)))

  ;; Remember to add an entry for `t', the library uses that as default.
  ;; The Custom interface is also supported for tuning the variable above.

  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )


(use-package which-key
  :ensure t
  :config (setq which-key-show-early-on-C-h nil)
  :init (which-key-mode))

(use-package consult
  :ensure t
  :init
  (global-set-key (kbd "C-x b") #'consult-buffer))



(use-package helpful
  :ensure t
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  :config
  (define-key helpful-mode-map (kbd "n") #'laluxx/next-helpful-heading)
  (define-key helpful-mode-map (kbd "p") #'laluxx/previous-helpful-heading)

  :hook (helpful-mode . laluxx/helpful-setup))


(defun laluxx/helpful-setup ()
  "Enable Olivetti mode and set the preferred width."
  (olivetti-mode 1)
  (olivetti-set-width 84))

(defun laluxx/navigate-helpful-heading (direction)
  "Move point to the next or previous text with helpful-heading face.
DIRECTION can be 'next or 'previous. If no heading is found, keep point
at current position and show message."
  (interactive)
  (let ((orig-point (point))
        (found nil)
        (search-fn (if (eq direction 'next)
                       #'next-single-property-change
                     #'previous-single-property-change))
        (limit (if (eq direction 'next)
                   (point-max)
                 (point-min))))
    
    ;; Search for heading
    (while (and (not found)
                (if (eq direction 'next)
                    (not (eobp))
                  (not (bobp))))
      (goto-char (funcall search-fn (point) 'face nil limit))
      (when (eq (get-text-property (point) 'face) 'helpful-heading)
        (setq found t)
        ;; For previous direction, go to beginning of line
        (when (eq direction 'previous)
          (beginning-of-line))))
    
    ;; If no heading found, restore position and notify user
    (unless found
      (goto-char orig-point)

      (message "No more helpful headings %s" 
               (if (eq direction 'next) "after this point" "before this point"))
      )))

(defun laluxx/next-helpful-heading ()
  "Move to next helpful heading."
  (interactive)
  (laluxx/navigate-helpful-heading 'next)
  (recenter))

(defun laluxx/previous-helpful-heading ()
  "Move to previous helpful heading."
  (interactive)
  (laluxx/navigate-helpful-heading 'previous)
  (recenter))


(use-package lsp-mode
  :ensure t
  ;; :hook ((c-mode . lsp)
  ;;        (c++-mode . lsp))
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-idle-delay 0.1) ; clangd is fast
  (setq lsp-headerline-breadcrumb-enable nil)
  (global-set-key (kbd "C-c C-k") #'lsp-ui-doc-show) ;; TODO only when lsp mode
  )


(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-frame-parameters
        '((border-width . 0)
          ))
  )


(use-package flycheck
  :ensure t
  :config (global-set-key (kbd "C-h C-e") #'consult-flycheck))



;;; SCROLLING
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;; OPTIMIZATIONS
(setq make-backup-files nil)
(setq auto-save-default nil)

;; (setq idle-update-delay 1.0)
(setq-default cursor-in-non-selected-windows nil)
;; (setq highlight-nonselected-windows nil)

;; (setq fast-but-imprecise-scrolling t)
;; (setq redisplay-skip-fontification-on-input t)

(setq custom-safe-themes t)
(setq use-dialog-box nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq confirm-kill-processes nil)

(defun shut-up-autosave-a (fn &rest args)
  "If a file has autosaved data, `after-find-file' will pause for 1 second to
tell you about it. Very annoying. This prevents that."
  (cl-letf (((symbol-function 'sit-for) #'ignore))
    (apply fn args)))

(advice-add 'after-find-file :around #'shut-up-autosave-a)

;;; ELFEED

(use-package elfeed
  :ensure t
  :config
  (add-hook 'elfeed-search-mode-hook (lambda () (hl-line-mode -1)))
  (global-set-key (kbd "C-x e") 'elfeed)
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://planet.emacslife.com/atom.xml"
          "https://stallmansupport.org/feed.xml"
          )))

;;; ERC

;; TODO C-k on a erc buffer should go to `Libera.chat' buffer if it exist
(defun erc-insert-backtick-single-quote ()
  "Insert a backtick followed by a single quote at point."
  (interactive)
  (insert "`'")
  (backward-char))

(with-eval-after-load 'erc
  (define-key erc-mode-map (kbd "C-j") 'erc-insert-backtick-single-quote))

(global-set-key (kbd "C-c c") 'erc-switch-to-buffer) ;; TODO or log into erc and openg the buffer

;;; DIRED

;; Dired --stfu
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq delete-by-moving-to-trash t)
(setq dired-no-confirm t)
(setq dired-mark-confirm nil)

(setq auto-revert-interval 0.1)
(setq auto-revert-idle-time 0.1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Set up trash directory in .emacs.d
(let ((trash-dir (expand-file-name "trash" user-emacs-directory)))
  (unless (file-exists-p trash-dir)
    (make-directory trash-dir t))
  (setq delete-by-moving-to-trash t
        trash-directory trash-dir))

;; Advice the actual deletion mechanism
(advice-add 'dired-internal-do-deletions :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'yes-or-no-p) #'always)
                        ((symbol-function 'y-or-n-p) #'always))
                (apply orig-fun args))))




(require 'dired-aux)
(auto-compression-mode 1)
(add-to-list 'auto-mode-alist '("\\.tar\\.gz\\'" . tar-mode))
(add-to-list 'auto-mode-alist '("\\.tgz\\'" . tar-mode))
(add-to-list 'auto-mode-alist '("\\.zip\\'" . archive-mode))

(use-package diredfl
  :ensure t
  :config
  (diredfl-global-mode))

(use-package wdired
  :ensure t
  :config
  (define-key wdired-mode-map (kbd "RET") 'wdired-finish-edit)
  (define-key wdired-mode-map (kbd "C-g") 'wdired-finish-edit)
  )

(use-package nerd-icons-dired
  :ensure t
  :diminish
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

(defun laluxx/dired-jump-or-kill ()
  "Jump to Dired buffer in another window or kill the Dired buffer if already in one."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (progn
        (kill-this-buffer)
        (delete-window))
    (dired-jump-other-window)))

(global-set-key (kbd "C-x C-l") 'laluxx/dired-jump-or-kill)

(defun laluxx/dired-move-marks-inside-here ()
  "Move all marked files in dired to the directory at point."
  (interactive)
  (let* ((marks (dired-get-marked-files))
         (dir (dired-get-filename))
         (target (file-name-as-directory dir)))
    (if (not (file-directory-p dir))
        (error "Target is not a directory")
      (dolist (marked marks)
        (let ((new-name (expand-file-name 
                         (file-name-nondirectory marked) 
                         target)))
          (dired-rename-file marked new-name nil)))
      ;; Revert the buffer to show the new state
      (revert-buffer))))

(define-key dired-mode-map (kbd "C-c m") 'laluxx/dired-move-marks-inside-here)


(defun laluxx/dired-copy-file-content ()
  "Copy the contents of the file at point in Dired to the clipboard."
  (interactive)
  (let* ((filename (dired-get-file-for-visit))
         (buffer-existed (get-buffer filename))
         (buffer (find-file-noselect filename)))
    (with-current-buffer buffer
      (clipboard-kill-ring-save (point-min) (point-max)))
    (unless buffer-existed
      (kill-buffer buffer))
    (message "Copied %s" filename)))

(defun laluxx/dired-duplicate-line ()
  "Duplicate file/directory and prepare for immediate rename."
  (interactive)
  (let* ((current-file (dired-get-filename))
         (new-name (if (file-directory-p current-file)
                       (concat current-file "-new")
                     (concat (file-name-sans-extension current-file) 
                             "-new" 
                             (or (file-name-extension current-file t) "")))))
    ;; Create duplicate
    (if (file-directory-p current-file)
        (copy-directory current-file new-name nil t t)
      (copy-file current-file new-name nil t t))
    
    ;; Go to the new entry and prepare for rename
    (revert-buffer)
    (dired-goto-file new-name)
    (wdired-change-to-wdired-mode)
    (dired-move-to-filename)
    (kill-line)))

;;; DIRED-EXTRA
(require 'dired-x)
(setq dired-omit-files (concat dired-omit-files "\\|^\\.#\\|~$"))
(setq dired-omit-verbose nil)
(add-hook 'dired-mode-hook 'dired-omit-mode)

;; TODO [0/2] Slowly support more programs, and arguments
;; [ ]   TODO wget
;; [ ]   TODO curl
;; [3/3] DONE CARGO 
;;   [ ] v TODO init
;;   [ ]   TODO `run' it should use compilation mode
;;   [ ]   TODO `build' it should use compilation mode

(defun laluxx/shell-command-advice (orig-fun command &rest args)
  "Advice to handle info commands in shell-command."
  (let* ((info-regexp "^info\\s-+\\(.*\\)$")
         (is-info (string-match info-regexp command))
         (info-topic (and is-info (match-string 1 command))))
    
    (if is-info
        (progn
          (condition-case nil
              (if (= (length (window-list)) 1)
                  ;; For single window
                  (let ((original-window (selected-window)))
                    (info info-topic)  ; Try to load info first
                    (select-window (split-window-right))  ; Split after success
                    (switch-to-buffer "*info*")  ; Show info in new window
                    (select-window original-window))  ; Go back to original
                ;; For multiple windows
                (info info-topic))
            (error
             (message "Could not find info page for %s" info-topic)))
          (setq args (plist-put args :output-buffer nil)))
      
      ;; For non-info commands, call the original function
      (apply orig-fun command args))))

(advice-add 'shell-command :around #'laluxx/shell-command-advice)


(defun laluxx/dired-smart-shell-command-advice (orig-fun command &rest args)
  "Advice to handle git clone and cargo init commands in dired-smart-shell-command."
  (let* ((git-clone-regexp "^git clone \\(--depth [0-9]+ \\)?\\(.*\\)$")
         (cargo-init-regexp "^cargo init \\(.*\\)$")
         (is-git-clone (string-match git-clone-regexp command))
         (is-cargo-init (string-match cargo-init-regexp command))
         (repo-url (and is-git-clone (match-string 2 command)))
         (project-name (cond
                        (is-git-clone (file-name-base repo-url))
                        (is-cargo-init (match-string 1 command))))
         (target-dir (and project-name (expand-file-name project-name))))
    
    ;; Call the original function
    (apply orig-fun command args)
    
    ;; Handle navigation for both git clone and cargo init
    (when (and project-name
               (or is-git-clone is-cargo-init)
               (file-exists-p target-dir))
      (revert-buffer)
      (dired-goto-file target-dir))))

(advice-add 'dired-smart-shell-command :around #'laluxx/dired-smart-shell-command-advice)

;; FIXME Overflow
;; (defun laluxx/smart-command (command &optional output-buffer error-buffer)
;;   "Execute COMMAND as a shell command or as an Emacs function if it exists.
;; Only considers interactive functions. Works in both dired and regular contexts."
;;   (interactive (list (read-shell-command "Shell command or function: ")))
;;   (let* ((cmd-symbol (intern-soft command))
;;          (cmd-fn (and cmd-symbol (symbol-function cmd-symbol))))
;;     (cond
;;      ;; Check if it's a defined function and is interactive
;;      ((and cmd-fn 
;;            (commandp cmd-fn))
;;       (call-interactively cmd-fn))
;;      ;; In dired-mode, use dired-smart-shell-command
;;      ((derived-mode-p 'dired-mode)
;;       (dired-smart-shell-command command output-buffer error-buffer))
;;      ;; Otherwise, use regular shell-command
;;      (t
;;       (shell-command command output-buffer error-buffer)))))

;; ;; Advice both shell-command and dired-smart-shell-command
;; (advice-add 'shell-command :around
;;             (lambda (orig-fn command &rest args)
;;               (apply #'laluxx/smart-command command args)))

;; (advice-add 'dired-smart-shell-command :around
;;             (lambda (orig-fn command &rest args)
;;               (apply #'laluxx/smart-command command args)))


;;; DIRED-HACKS

(use-package dired-hacks
  :load-path "~/.config/emacs/lisp/dired-hacks"
  :config
  (require 'dired-subtree)
  (require 'dired-narrow)
  ;; (require 'dired-collapse) FIXME Conflicts with 'dired-subtree'
  )

(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

(defun laluxx/dired-next-heading ()
  "Move to the next heading in Dired mode."
  (interactive)
  (forward-paragraph)
  (next-line))

(defun laluxx/dired-previous-heading ()
  "Move to the previous heading in Dired mode."
  (interactive)
  (backward-paragraph)
  (backward-paragraph)
  (next-line)
  (when (= (line-number-at-pos) 2)
    (previous-line)))

(defun laluxx/dired-smart-tab ()
  "Toggle subtree if directory, toggle preview if image."
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (is-image (and file 
                        (string-match (image-file-name-regexp) file))))
    (if is-image
        (dired-image-preview-toggle)
      (dired-subtree-toggle))))

(defun laluxx/dired-kill-word ()
  "Wrapper for laluxx/kill-word that handles dired read-only state."
  (interactive)
  (wdired-change-to-wdired-mode)
  (laluxx/kill-word))

(defun laluxx/dired-smart-up ()
  "If there are marked files, move them to parent directory.
If no files are marked, just go up to parent directory."
  (interactive)
  (let ((marked-files (dired-get-marked-files nil nil nil t)))
    (if (eq (car marked-files) t)  ; No marks
        (dired-up-directory)
      (let* ((marks (dired-get-marked-files))
             (dir (directory-file-name default-directory))
             (target (file-name-directory dir)))
        (dolist (marked marks)
          (let ((new-name (expand-file-name 
                           (file-name-nondirectory marked) 
                           target)))
            (dired-rename-file marked new-name nil)))
        (revert-buffer)))))

(defun laluxx/dired-mode-setup ()
  "Custom keybindings and settings for `dired-modexor`."
  ;; (define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
  (define-key dired-mode-map (kbd "f") 'dired-show-file-type)
  (define-key dired-mode-map (kbd "w") 'laluxx/dired-set-wallpaper)
  (define-key dired-mode-map (kbd "C-s") 'dired-narrow)
  (define-key dired-mode-map (kbd "TAB") 'laluxx/dired-smart-tab)
  (define-key dired-mode-map (kbd "b") 'dired-up-directory)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "h") 'dired-up-directory)
  ;; (define-key dired-mode-map (kbd "h") 'laluxx/dired-smart-up)
  (define-key dired-mode-map (kbd "l") 'dired-find-file)
  ;; (define-key dired-mode-map (kbd "y") 'laluxx/dired-copy-file-content)
  (define-key dired-mode-map (kbd "i") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "s") 'dired-do-isearch)
  (define-key dired-mode-map (kbd "C-o") 'dired-maybe-insert-subdir)
  (define-key dired-mode-map (kbd "C-w") 'laluxx/dired-kill-word)
  (define-key dired-mode-map (kbd "M-n") 'laluxx/dired-next-heading)
  (define-key dired-mode-map (kbd "M-p") 'laluxx/dired-previous-heading)
  ;; (define-key dired-mode-map (kbd "C-j") 'laluxx/dired-next-heading)
  (define-key dired-mode-map (kbd "C-S-o") 'laluxx/dired-duplicate-line)
  (auto-revert-mode 1)
  (beginning-of-buffer)
  (dired-next-line 1)
  )

(add-hook 'dired-mode-hook 'laluxx/dired-mode-setup)

;;; INFO

(setq Info-use-header-line nil)

(with-eval-after-load 'info
  (define-key Info-mode-map (kbd "n") 'Info-forward-node)
  (define-key Info-mode-map (kbd "p") 'Info-backward-node)
  (define-key Info-mode-map (kbd "]") 'Info-next)
  (define-key Info-mode-map (kbd "[") 'Info-prev))

(defun laluxx/info-mode-setup ()
  "Set up my preferences for Info mode."
  (olivetti-mode 1)
  (olivetti-set-width 82)
  (text-scale-set 1))

(add-hook 'Info-mode-hook 'laluxx/info-mode-setup)

;;; IMAGE-MODE

(defun laluxx/delete-image ()
  "Delete the current image file and move to the next image using image-mode navigation."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (when current-file
      ;; Delete the current file
      (delete-file current-file)
      ;; Display deletion message with custom formatting
      (message "%s %s"
               (propertize "DELETED" 'face '(bold error))
               (propertize current-file 'face 'default))
      ;; Kill current buffer and move to next image
      (let ((buf (current-buffer)))
        (image-next-file)
        (kill-buffer buf)))))

(with-eval-after-load 'image-mode
  (define-key image-mode-map (kbd "D") #'laluxx/delete-image))


;;; UI

(setq kill-buffer-query-functions nil) ; Never prompt about processes
(setq echo-keystrokes-help nil) ; Never show help in minibuffer

(use-package theme-magic
  :ensure t)

;;; WINDOW DIVIDER
(defun laluxx/set-window-divider-colors ()
  (interactive)
  (if (display-graphic-p)
      ;; GUI mode - match background color
      (let ((bg-color (face-attribute 'default :background nil t)))
        (set-face-foreground 'window-divider bg-color)
        (set-face-background 'window-divider bg-color)
        (set-face-foreground 'window-divider-first-pixel bg-color)
        (set-face-background 'window-divider-first-pixel bg-color)
        (set-face-foreground 'window-divider-last-pixel bg-color)
        (set-face-background 'window-divider-last-pixel bg-color))
    ;; Terminal mode - use space character for divider
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\s))))

;; Initial setup
(setq window-divider-default-right-width 1)
(setq window-divider-default-bottom-width 0)

;; Set up colors for initial theme
(laluxx/set-window-divider-colors)

;; Update colors when theme changes
(with-eval-after-load 'consult
  (advice-add 'consult-theme-switcher-dark :after
              (lambda (&rest _)
                (laluxx/set-window-divider-colors))))

;; Enable window divider mode
(window-divider-mode 1)


;; TODO write `epoke' package
;; just like `dired-poke' (but based on `ilib')
;; like this /d will jump to this tag and recenter
;;       v            
;;; DISPLAY-BUFFER-ALIST [d]

(defun laluxx/select-body-function (window)
  (select-window window))

;; (defun laluxx/fit-compilation-window (buffer status)
;;   (with-current-buffer buffer
;;     (when (string-match "\\*compilation\\*" (buffer-name))
;;       (let ((window (get-buffer-window buffer t)))
;;         (when window
;;           (fit-window-to-buffer 
;;            window 
;;            (floor (* 0.5 (frame-height)))  ; max height: 50% of frame
;;            1  ; min height: 1 line
;;            nil nil t))))))

;; (add-hook 'compilation-finish-functions #'laluxx/fit-compilation-window)

;; (setq split-height-threshold nil) ;; Disable automatic vertical-splitting
;; (setq split-width-threshold 0)    ;; Force automatic horizontal-splitting



;;[ ] TODO C-e should jump at the end of the line without the comment in lisps
;;    then if called again at the end of line


(setq display-buffer-alist
      '(;; Magit modes - explicit matching for different magit buffers
        ("\\*magit: .*\\|magit-.*\\|\\*Magit.*\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-height . 0.4)
         (reusable-frames . visible))
        
        ("^magit: .*"  ; magit status
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 0)
         (window-height . 0.4)
         (reusable-frames . visible))
        
        ("\\*magit-commit\\*"  ; magit commit
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 1)
         (window-height . 0.4)
         (reusable-frames . visible))

        ("\\*magit-diff\\*"  ; magit diff
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 2)
         (window-height . 0.4)
         (reusable-frames . visible))

        ("\\*doom.*\\*"
         (display-buffer-same-window)
         (mode-line-format . none))

        ;; NOTE Integrate `list-packages' into `doom-dashboard'
        ("\\*Packages\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 3)
         (window-height . 0.56)
         (reusable-frames . visible))

        ;; Help and documentation
        ("\\*Help\\*\\|\\*Apropos\\*\\|\\*cider-doc\\*\\|\\*TeX Help\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.45)
         (reusable-frames . visible))

        ("\\*compilation\\*\\|\\*Compile-Log\\*\\|\\*rustic-compilation\\*"
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         ;; Parameters
         (window-height . 0.5); 1
         (dedicated . t)
         (body-function . laluxx/select-body-function)
         (window-parameters . ((mode-line-format . none))))

        ;; Custom mode
        ("\\*Custom.*\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 3)
         (window-height . 0.4)
         (body-function . laluxx/select-body-function)
         (window-parameters . ((mode-line-format . none)))
         )

        ;; Terminal and shell buffers
        ("\\*e?shell\\*\\|\\*terminal\\*\\|\\*eat\\*\\|\\*ansi-term\\*\\|\\*term\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 6)
         (window-height . 0.3)
         (reusable-frames . visible))

        (("\\*Messages\\*\\|\\*evil-registers\\*"
          (display-buffer-reuse-window display-buffer-in-side-window)
          (side . bottom)
          (slot . 1)  ; Changed to slot 1 (left position)
          (window-width . 0.5)  ; Set width to 50% of frame
          (window-height . 0.57)
          (reusable-frames . visible))

         ("\\*Warnings\\*\\|\\*Backtrace\\*\\|\\*cider-error\\*\\|\\*HS-Error\\*"
          (display-buffer-reuse-window display-buffer-in-side-window)
          (side . bottom)
          (slot . 2)  ; Changed to slot 2 (right position)
          (window-width . 0.5)  ; Set width to 50% of frame
          (window-height . 0.57)
          (reusable-frames . visible)))

        ;; Xref and completion
        ("\\*xref\\*\\|\\*Completions\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 9)
         (window-height . 0.3)
         (reusable-frames . visible))

        ;; Shell command output
        ("\\*Shell Command Output\\*\\|\\*Async Shell Command\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 10)
         (window-height . 0.3)
         (reusable-frames . visible))

        ;; Calculator
        ("^\\*Calc.*\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 11)
         (window-height . 0.3)
         (reusable-frames . visible))

        ;; r2 mode
        ("\\*r2:.*\\*"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (slot . 13)
         (window-height . 0.4)
         (reusable-frames . visible))))


(setq switch-to-buffer-obey-display-actions t)
;; (setq switch-to-buffer-in-dedicated-window 'pop)

;; (defun laluxx/hide-modelines ()
;;   "Hide the mode line in specific buffers and modes."
;;   (let ((buffer-name (buffer-name)))
;;     (when (or (member buffer-name '("*Warnings*"
;;                                     "*Packages*"
;;                                     "*Completions*"
;;                                     "*Compile-Log*"
;;                                     "*cider-doc*"
;;                                     "*cider-error*"
;;                                     "*Lisp Result*"
;;                                     "*rustic-compilation*"
;;                                     "*Embark Actions*"
;;                                     "*ansi-term*"
;;                                     "*doom*"
;;                                     "*eat*"
;;                                     "*term*"
;;                                     ;; "*ielm*"
;;                                     "*msh-debug*"
;;                                     "*xref*"
;;                                     "*shell*"
;;                                     "*eshell*"
;;                                     "*HS-Error*"
;;                                     "*Help*"
;;                                     "*Apropos*"
;;                                     "*Disabled Command*"
;;                                     "*Backtrace*"
;;                                     "*compilation*"
;;                                     "*Shell Command Output*"))
;;               (derived-mode-p 'magit-log-mode
;;                               'magit-revision-mode
;;                               'magit-status-mode
;;                               'leetcode--problem-detail-mode
;;                               'r2-mode
;;                               'msh-mode
;;                               'eshell-mode
;;                               'shell-mode
;;                               'help-mode))
;;       (setq mode-line-format nil))))

;; (add-hook 'after-change-major-mode-hook 'laluxx/hide-modelines)
;; (add-hook 'buffer-list-update-hook 'laluxx/hide-modelines)


;; (defun laluxx/org-icons ()
;;   "Beautify org mode keywords."
;;   (setq prettify-symbols-alist 
;;         '(("WAIT" . "")
;;           ;; ("TODO" . "")
;;           ("NOPE" . "")
;;           ("DONE" . "")
;;           ("[#A]" . "")
;;           ("[#B]" . "")
;;           ("[#C]" . "")
;;           ("[ ]" . "")
;;           ("[X]" . "")
;;           ("[-]" . "")
;;           ("#+BEGIN_SRC" . "")
;;           ("#+begin_src" . "")
;;           ("#+END_SRC" . "󰨿")
;;           (":PROPERTIES:" . "")
;;           (":END:" . "―")
;;           ("#+STARTUP:" . "")
;;           ("#+TITLE: " . "")
;;           ("#+RESULTS:" . "")
;;           ("#+NAME:" . "")
;;           ("#+ROAM_TAGS:" . "")
;;           ("#+FILETAGS:" . "")
;;           ("#+HTML_HEAD:" . "")
;;           ("#+SUBTITLE:" . "")
;;           ("#+AUTHOR:" . "")
;;           ("#+DESCRIPTION:" . "󰯂")
;;           (":Effort:" . "")
;;           ("SCHEDULED:" . "")
;;           ("DEADLINE:" . "")))
;;   (prettify-symbols-mode 1))

;; (add-hook 'org-mode-hook 'laluxx/org-icons)


(use-package ligature
  :ensure t
  :config
  ;; NOTE This config is specific for Jetbrains mono
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;"))
  (global-ligature-mode t))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq prettify-symbols-alist '(("lambda" . "λ"))) (prettify-symbols-mode 1)))


;;; TTY
;; Set terminal truncation indicator TODO for left too
(unless (display-graphic-p)
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?›))
  (xterm-mouse-mode 1)
  (use-package xclip
    :load-path "~/.config/emacs/lisp/xclip"
    :config (xclip-mode)))


(setq confirm-nonexistent-file-or-buffer nil)

(setq frame-title-format '("%b – Minimacs")
      icon-title-format frame-title-format)

;; (global-prettify-symbols-mode)

(use-package rainbow-mode
  :diminish
  :ensure t
  :diminish
  :hook org-mode prog-mode help-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package olivetti
  :ensure t
  :config
  (global-set-key (kbd "C-<") #'olivetti-expand)
  (global-set-key (kbd "C->") #'olivetti-shrink))

(use-package hl-todo
  :ensure t
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("FIX"      error bold)
          ("BUG"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DONE"       success bold)
          ("IMPORTANT"  success bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("AFTER" font-lock-constant-face bold)
          ("NEXT" font-lock-keyword-face bold)
          ("MAYBE" font-lock-keyword-face bold)
          ("SYNTAX" font-lock-keyword-face bold)
          ("LATER" font-lock-doc-face bold))))

(use-package rainbow-mode
  :ensure t
  :hook org-mode prog-mode help-mode)

;;; MAGIT

(use-package magit
  :ensure t
  :commands magit-status
  :config
  (global-set-key (kbd "C-h g") #'magit-status)

  )

(global-set-key (kbd "C-h C-c") #'magit-log-all)
(defun laluxx/magit-bury-and-log-all ()
  "Bury the current Magit buffer and open Magit log all."
  (interactive)
  (magit-mode-bury-buffer)
  (magit-log-all))

(with-eval-after-load 'magit
  (define-key magit-revision-mode-map (kbd "q") 'laluxx/magit-bury-and-log-all))


;;; EWW

(defun laluxx/eww-mode-setup ()
  "Setup settings for `eww-mode`."
  ;; (setq-local header-line-format nil) ; Hide the header line
  (olivetti-mode)
  (olivetti-set-width 118)
  (text-scale-set 1))

(add-hook 'eww-mode-hook #'laluxx/eww-mode-setup)

(defun laluxx/eww-recenter-hack ()
  "Position cursor at first real link and recenter to hide search boxes."
  (interactive)
  (unless (eq last-command 'eww-follow-link)
    (dotimes (_ 5) (shr-next-link))
    (previous-line)
    (recenter 0)))

(add-hook 'eww-after-render-hook #'laluxx/eww-recenter-hack)

(defun laluxx/wikipedia-recenter-hack ()
  "After loading a Wikipedia page, position at article start."
  (when (string-match "wikipedia.org" (plist-get eww-data :url))
    (eww-follow-link)
    (next-line)
    (recenter 0)))

(add-hook 'eww-after-render-hook #'laluxx/wikipedia-recenter-hack)

(global-set-key (kbd "C-h s") 'eww)
(global-set-key (kbd "C-h C-s") 'eww)


(defun laluxx/maybe-open-url-in-eww (original-fun &rest args)
  "Advice function to make buttons open Wikipedia URLs in eww, all other URLs in default browser."
  (let* ((url (car args))
         (wikipedia-domains "\\(wikipedia\\.org\\|wiktionary\\.org\\|wikimedia\\.org\\|mediawiki\\.org\\)"))
    (if (and (string-match-p "^\\(http\\|https\\)://" url)
             (string-match-p wikipedia-domains url))
        (eww url)
      (apply original-fun args))))

(advice-add 'browse-url :around #'laluxx/maybe-open-url-in-eww)


;;; AUTO INSERT
(setq auto-insert-alist nil) ; Clear existing auto-insert-alist to remove default templates
(auto-insert-mode 1) ; Enable auto-insert mode
(setq auto-insert-query nil) ;; Don't ask for confirmation

;; Define only the org template
(define-auto-insert
  "\\.org\\'"
  '("Org File Template"
    "#+TITLE: " (file-name-nondirectory (buffer-file-name)) "\n"
    "#+AUTHOR: " user-full-name "\n"
    "#+DATE: " (format-time-string "[%Y-%m-%d]") "\n"
    "#+OPTIONS: toc:2\n\n"
    ))

(defun laluxx/auto-insert-predicate ()
  "Return t if current buffer should get auto-inserted content."
  (and (string-match-p "\\.org\\'" (buffer-file-name))
       (= (buffer-size) 0)))  ; Only insert if buffer is empty

(setq auto-insert-predicate 'laluxx/auto-insert-predicate)

;;; LANGUAGES

(use-package lua-mode
  :ensure t)

;;; GUILE

(use-package geiser-guile
  :ensure t)

;;; JULIA

(use-package eat
  :ensure t)


(use-package julia-mode
  :ensure t)

(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode)
  :config ;; (add-hook 'julia-snail-mode-hook #'julia-snail)
  :bind (:map julia-snail-mode-map
              ("C-j" . julia-snail-send-dwim)
              ))

;; (use-package julia-mode
;;   :ensure t)

;; (use-package julia-snail
;;   :ensure t
;;   :config
;;   )


;;; WEB
(use-package emmet-mode
  :ensure t
  :config
  (setq emmet-preview-default nil)
  (define-key html-mode-map (kbd "M-n") 'emmet-next-edit-point)
  (define-key html-mode-map (kbd "M-p") 'emmet-prev-edit-point)
  :hook
  (html-mode . emmet-mode))

(setq css-fontify-colors nil)

;;; ELISP
(setq delete-pair-blink-delay 0)
(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-last-sexp)
(define-key input-decode-map [?\C-m] [C-m])
(define-key emacs-lisp-mode-map (kbd "<C-m>") 'mark-sexp)
(define-key emacs-lisp-mode-map (kbd "RET") 'newline)
(define-key emacs-lisp-mode-map (kbd "TAB") 'laluxx/tab-cycle)
(define-key emacs-lisp-mode-map (kbd "M-^") 'laluxx/elisp-delete-indentation)

;; [ ] TODO merge also sexps
(defun laluxx/elisp-delete-indentation (&optional arg)
  "Merge lines, intelligently handling comment lines."
  (interactive "*P")
  (save-excursion
    (beginning-of-line)
    (when (and (save-match-data
                 (save-excursion
                   (forward-line -1)
                   (looking-at "^[ \t]*;;")))
               (looking-at "^[ \t]*;;"))
      (delete-char 2)))
  (delete-indentation arg))

(use-package bicycle
  :ensure t
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([backtab] . bicycle-cycle-global)))

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;;; HASKELL
(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-process-show-debug-tips nil))

(defun laluxx/haskell-prettify-symbols ()
  "Prettify backslash to lambda in Haskell mode."
  (push '("\\" . "λ") prettify-symbols-alist)
  (prettify-symbols-mode))

(add-hook 'haskell-mode-hook 'laluxx/haskell-prettify-symbols)


(defun laluxx/haskell-load-on-save ()
  "When saving, load the current Haskell file interactively."
  (add-hook 'after-save-hook
            (lambda ()
              (call-interactively 'haskell-process-load-file))
            nil 'local))

(add-hook 'haskell-mode-hook 'laluxx/haskell-load-on-save)


(defun laluxx/haskell-smart-newline ()
  "Insert newline, indent, and if applicable, add the function name on the new line in Haskell."
  (interactive)
  (let ((function-name nil)
        (current-line (thing-at-point 'line t)))
    ;; Check for the function name in the previous lines
    (save-excursion
      (if (re-search-backward "^\\([a-zA-Z0-9_']+\\)\\s-+::" nil t)
          (setq function-name (match-string 1))))

    ;; Insert the newline and indent
    (haskell-indentation-newline-and-indent)

    ;; Determine the current line context
    (when (and function-name
               (not (string-blank-p current-line)) ;; Ignore empty lines
               (not (string-match-p "^\\s-*l\\s-*=" current-line))) ;; Ignore lines defining 'l'
      (insert function-name " "))))



(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-j") 'laluxx/haskell-smart-newline)
  (with-eval-after-load 'haskell-indentation
    (define-key haskell-indentation-mode-map (kbd "C-j") 'laluxx/haskell-smart-newline)))

(defun setup-haskell-interactive-keys ()
  "Set up custom keybindings for the Haskell interactive mode."
  (local-set-key (kbd "<up>") 'haskell-interactive-mode-history-previous)
  (local-set-key (kbd "<down>") 'haskell-interactive-mode-history-next)
  (local-set-key (kbd "C-l") 'haskell-interactive-mode-clear))

(add-hook 'haskell-interactive-mode-hook 'setup-haskell-interactive-keys)

;; LOGO
;; (defvar laluxx/haskell-logo-frame nil
;;   "Store the child frame showing Haskell logo.")

;; (defun laluxx/get-haskell-window-position ()
;;   "Get the position of the current Haskell interactive window."
;;   (let* ((window (selected-window))
;;          (edges (window-pixel-edges))
;;          (left (nth 0 edges))
;;          (top (nth 1 edges)))
;;     (cons left top)))

;; (defun laluxx/create-haskell-logo-frame ()
;;   "Create a floating child frame displaying the Haskell logo."
;;   (interactive)
;;   ;; Close existing frame if it exists
;;   (when (and laluxx/haskell-logo-frame (frame-live-p laluxx/haskell-logo-frame))
;;     (delete-frame laluxx/haskell-logo-frame))
  
;;   (let* ((image-path (expand-file-name "multisession/images/haskell.png" user-emacs-directory))
;;          (image (create-image image-path))
;;          ;; Get exact pixel dimensions
;;          (image-pixels (image-size image t))
;;          ;; Convert to exact frame size (character width/height)
;;          (frame-width (/ (car image-pixels) (frame-char-width)))
;;          (frame-height (/ (cdr image-pixels) (frame-char-height)))
;;          ;; Get window position
;;          (window-pos (laluxx/get-haskell-window-position))
;;          (pos-x (car window-pos))         ; Left edge of window
;;          (pos-y (cdr window-pos)))        ; Top edge of window

;;     ;; Create floating frame with exact dimensions
;;     (setq laluxx/haskell-logo-frame
;;           (make-frame
;;            `((name . "Haskell Logo")
;;              (parent-frame . ,(selected-frame))
;;              (minibuffer . nil)
;;              (width . ,(ceiling frame-width))
;;              (height . ,(ceiling frame-height))
;;              (left . ,pos-x)
;;              (top . ,pos-y)
;;              (no-accept-focus . t)
;;              (no-focus-on-map . t)
;;              (internal-border-width . 1)
;;              (internal-border-color . ,(face-background 'default))
;;              (vertical-scroll-bars . nil)
;;              (horizontal-scroll-bars . nil)
;;              (menu-bar-lines . 0)
;;              (tool-bar-lines . 0)
;;              (tab-bar-lines . 0)
;;              (undecorated . t)
;;              (visibility . t)
;;              (cursor-type . nil)
;;              (background-color . ,(face-background 'default))
;;              (auto-hide-function . nil)
;;              (override-redirect . t)
;;              (z-group . above)
;;              (alpha . 90))))

;;     ;; Create buffer with image and exact fit
;;     (with-selected-frame laluxx/haskell-logo-frame
;;       (switch-to-buffer " *haskell-logo*")
;;       (setq mode-line-format nil
;;             header-line-format nil
;;             truncate-lines t)              ; Prevent line wrapping
;;       (insert-image image)
;;       (read-only-mode 1))))

;; (defun laluxx/remove-haskell-logo-frame ()
;;   "Remove the Haskell logo frame if it exists."
;;   (when (and laluxx/haskell-logo-frame (frame-live-p laluxx/haskell-logo-frame))
;;     (delete-frame laluxx/haskell-logo-frame)
;;     (setq laluxx/haskell-logo-frame nil)))

;; ;; Function to update frame position when window changes
;; (defun laluxx/update-haskell-logo-position (&rest _)
;;   "Update the position of the Haskell logo frame when window configuration changes."
;;   (when (and laluxx/haskell-logo-frame 
;;              (frame-live-p laluxx/haskell-logo-frame)
;;              (eq major-mode 'haskell-interactive-mode))
;;     (let* ((window-pos (laluxx/get-haskell-window-position))
;;            (pos-x (car window-pos))
;;            (pos-y (cdr window-pos)))
;;       (modify-frame-parameters laluxx/haskell-logo-frame
;;                                `((left . ,pos-x)
;;                                  (top . ,pos-y))))))

;; ;; Add hooks to create/remove/update the logo frame
;; (add-hook 'haskell-interactive-mode-hook #'laluxx/create-haskell-logo-frame)
;; (add-hook 'kill-buffer-hook
;;           (lambda ()
;;             (when (eq major-mode 'haskell-interactive-mode)
;;               (laluxx/remove-haskell-logo-frame))))
;; (add-hook 'window-configuration-change-hook #'laluxx/update-haskell-logo-position)


;;; RUST
(use-package rust-mode
  :ensure t
  :defer t
  :mode ("\\.rs\\'" . rust-mode)
  :hook
  (rust-mode . (lambda ()
                 (local-set-key (kbd "C-c C-m") 'lsp-rust-analyzer-expand-macro))))

;;; C
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)



(use-package embark
  :ensure t)

(use-package embark-consult
  :ensure t)


;;;; IELM
(require 'ielm)
(setq ielm-header "")
(setq ielm-prompt " 󰄾 ")

(add-hook 'ielm-mode-hook (lambda ()
                            (define-key ielm-map (kbd "C-l")
                                        (lambda ()
                                          (interactive)
                                          (recenter-top-bottom 0)))))

(defun laluxx/ielm-smart-space ()
  "Smart space function for IELM.
If cursor is before an opening parenthesis and there's a word immediately
to the left without a space, insert a space, then wrap the word with parentheses."
  (interactive)
  (if (and (eq (char-after) ?\()
           (looking-back "\\w+" 1))
      (let ((word-start (save-excursion
                          (backward-word)
                          (point))))
        (insert " ")
        (save-excursion
          (goto-char word-start)
          (insert "(")
          (forward-list)
          (insert ")")))
    (insert " ")))

(define-key ielm-map (kbd "SPC") 'laluxx/ielm-smart-space)
;; (define-key emacs-lisp-mode-map (kbd "SPC") 'laluxx/ielm-smart-space)


(defun laluxx/toggle-ielm ()
  "Toggle ielm buffer."
  (interactive)
  (toggle-buffer "*ielm*" #'ielm))

(global-set-key (kbd "C-c C-i") 'laluxx/toggle-ielm)


;; WOMAN
;; TODO make me pretty 
(defun laluxx/woman-setup ()
  "Setup for `woman-mode'"
  (olivetti-mode)
  (olivetti-set-width 72)
  (text-scale-set 1))

(add-hook 'woman-mode-hook #'laluxx/woman-setup)
(global-set-key (kbd "C-h w") 'woman)



;; ISEARCH

(defun laluxx/isearch-with-region (&rest _)
  "When isearch is invoked and region is active, use region content as search string."
  (when (use-region-p)
    (let ((region (buffer-substring-no-properties (region-beginning) (region-end))))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))

(advice-add 'isearch-forward :after #'laluxx/isearch-with-region)
(advice-add 'isearch-backward :after #'laluxx/isearch-with-region)


;;; FUNCTIONS

(defun laluxx/find-nested-use-package ()
  "Find use-package forms nested inside other use-package forms."
  (interactive)
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "(use-package" nil t)
        (let ((pos (match-beginning 0)))
          (save-excursion
            (goto-char pos)
            (when (ignore-errors  ; only match if inside another use-package
                    (backward-up-list)
                    (looking-at "(use-package"))
              (goto-char pos)     ; go back to push the inner occurrence
              (push (list (current-buffer)
                          (line-number-at-pos)
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                    matches))))))
    (if matches
        (with-current-buffer (get-buffer-create "*Nested use-package*")
          (let ((inhibit-read-only t))
            (compilation-mode)
            (erase-buffer)
            (dolist (match (nreverse matches))
              (insert (format "%s:%d:%s\n" 
                              (buffer-name (nth 0 match))
                              (nth 1 match)
                              (nth 2 match))))
            (display-buffer (current-buffer))))
      (message "No nested use-package forms found"))))


;; (defun j-maybe-evil ()
;;   (interactive)
;;   (if (and (not (buffer-modified-p))
;;            (not (minibufferp))
;;            (not buffer-read-only))
;;       (progn 
;;         (evil-mode 1)
;;         (next-line))
;;     (insert "j")))

;; (global-set-key (kbd "j") #'j-maybe-evil)


;;[ ] TODO It just doesn't work
(defun maximize-all-windows ()
  "Maximize all visible windows based on the longest visible line in each, then expand to perfect alignment."
  (interactive)
  (let* ((windows (window-list))
         (frame-width (frame-width))
         (frame-height (frame-height))
         (window-data (mapcar (lambda (window)
                                (with-current-buffer (window-buffer window)
                                  (let ((start (window-start window))
                                        (end (window-end window t))
                                        (max-line-length 0))
                                    (save-excursion
                                      (goto-char start)
                                      (while (and (< (point) end) (not (eobp)))
                                        (end-of-line)
                                        (setq max-line-length (max max-line-length (current-column)))
                                        (forward-line 1)))
                                    (list window max-line-length))))
                              windows))
         (total-width (apply '+ (mapcar 'cadr window-data)))
         (width-factor (/ (float frame-width) total-width)))
    
    ;; First pass: resize based on content
    (dolist (data window-data)
      (let* ((window (car data))
             (max-line-length (cadr data))
             (new-width (floor (* max-line-length width-factor))))
        (adjust-window-trailing-edge window (- new-width (window-width window)) t)))
    
    ;; Second pass: expand further if possible
    (dolist (window windows)
      (let ((max-width (- frame-width (window-pixel-left window) 1)))
        (adjust-window-trailing-edge window (- max-width (window-pixel-width window)) t)))
    
    ;; Final pass: perfect alignment
    (dolist (window windows)
      (let* ((edges (window-edges window t t t))
             (right-edge (nth 2 edges))
             (next-left-edge (or (nth 0 (window-edges (next-window window) t t t)) frame-width))
             (extra-space (- next-left-edge right-edge 1)))
        (when (> extra-space 0)
          (adjust-window-trailing-edge window extra-space t))))
    
    (balance-windows-area)))


;;[ ] TODO `C-j' shoudl eval the function under point
;;[ ] TODO connect with ascii art `if' to the `else' list in `elisp-mode'
;;[ ] TODO if the point is here "|" if i prss a keybind insert the keybind as a string


(defun laluxx/mark-words ()
  "Mark word at point and set its background to the foreground color of the word."
  (interactive)
  (let* ((word-pos (point))
         (face (get-text-property word-pos 'face))
         (fg (face-foreground face)))
    (when fg
      (set-face-background 'region fg))
    (backward-word)
    (mark-word)))

(global-set-key (kbd "C-S-w") 'laluxx/mark-word)

(defvar big-mode-state nil)

(defun laluxx/toggle-big-mode ()
  (interactive)
  (if big-mode-state
      (progn
        (set-face-attribute 'default nil
                            :family "JetBrains Mono Nerd Font"
                            :weight 'regular
                            :height 130)
        (setq big-mode-state nil))
    (progn
      (set-face-attribute 'default nil
                          :family "JetBrains Mono Nerd Font"
                          :weight 'regular
                          :height 160)
      (setq big-mode-state t))))

(global-set-key (kbd "C-c b") 'laluxx/toggle-big-mode)




(defun laluxx/ensure-org-buffer-in-right-window ()
  "When opening an org file with two windows in vertical split, display it in the right window."
  (when (and (eq major-mode 'org-mode)            ; Check if it's org-mode
             (= (length (window-list)) 2)         ; Exactly 2 windows
             (window-in-direction 'right)         ; Has window to the right
             (not (window-in-direction 'up))      ; No window above
             (not (window-in-direction 'down)))   ; No window below
    (let ((org-buffer (current-buffer)))
      (other-window 1)                           ; Move to right window
      (switch-to-buffer org-buffer))))           ; Display org buffer there

(add-hook 'org-mode-hook #' laluxx/ensure-org-buffer-in-right-window)

;;;DWM
;; Make Emacs the best suckless editor
;; TODO make make a package
;; TODO Apply patch in region

(defun compile-dwm-config ()
  "Compile DWM config after saving config.def.h"
  (when (string= (buffer-name) "config.def.h")
    (let ((default-directory (file-name-directory (buffer-file-name))))
      (compile
       (concat "echo " (shell-quote-argument (read-passwd "Password: "))
               " | sudo -S cp config.def.h config.h && echo $? > /tmp/cp_status && "
               "cat /tmp/cp_status | grep '^0$' > /dev/null && "
               "sudo -S make install")))))

(add-hook 'after-save-hook #'compile-dwm-config)


;;; REGION

;; TODO Make this a package called `ewra'
;; TODO it doesn't work well with functions with infinite arity
;; like `lambda' `interactive'...
(defun laluxx/wrap ()
  "Smart wrapping of Lisp forms using Emacs' built-in form understanding."
  (interactive)
  (if (eq last-command 'laluxx/wrap)
      (undo)
    (condition-case nil
        (delete-pair)
      (error
       (save-excursion
         (let* ((bounds (bounds-of-thing-at-point 'symbol))
                (sym (when bounds 
                       (intern-soft (buffer-substring-no-properties 
                                     (car bounds) (cdr bounds)))))
                (min-args (when (and sym (functionp sym))
                            (car (func-arity sym)))))
           (if (and min-args                        ; It's a function
                    (not (nth 3 (syntax-ppss))))    ; Not in a string
               (condition-case nil
                   (let ((start (car bounds)))
                     (goto-char (cdr bounds))       ; End of function name
                     (dotimes (_ min-args)          ; Try to find all required args
                       (forward-sexp))
                     (insert ")")
                     (goto-char start)
                     (insert "("))
                 (error                             ; Missing args or invalid syntax
                  (goto-char (car bounds))
                  (insert "(")
                  (goto-char (cdr bounds))
                  (insert ")")))
             ;; Not a function or in a string - wrap word
             (skip-syntax-backward "w")
             (insert "(")
             (skip-syntax-forward "w")
             (insert ")"))))))))

(global-set-key (kbd "C-M-d") 'laluxx/wrap)


;; TODO Make sure the end is visible ? (as an option)
;; TODO pulse the added region ? (as an option)
(defun laluxx/append-region-to-other-window (start end)
  "Append the current region to the buffer in the other window.
Ensures exactly one empty line before the appended text if there wasn't one.
The region is specified by START and END positions."
  (interactive "r")
  (let ((text (buffer-substring start end))
        (other-buffer (window-buffer (other-window-for-scrolling))))
    (save-excursion
      (with-current-buffer other-buffer
        (goto-char (point-max))
        ;; Go back two chars if possible to check for double newline
        (let ((needs-newline (not (and (>= (- (point-max) (point-min)) 2)
                                       (= (char-before) ?\n)
                                       (= (char-before (1- (point))) ?\n)))))
          ;; If we don't already have an empty line, add one
          (when needs-newline
            (unless (= (char-before) ?\n)
              (insert "\n"))
            (insert "\n"))
          (insert text))))))

(global-set-key (kbd "C-c a") 'laluxx/append-region-to-other-window)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(defun laluxx/copy-window ()
  "Display the current window geometry and create a clipboard-ready alist entry."
  (interactive)
  (let* ((window (selected-window))
         (edges (window-edges window))
         (left (nth 0 edges))
         (top (nth 1 edges))
         (right (nth 2 edges))
         (bottom (nth 3 edges))
         (width (- right left))
         (height (- bottom top))
         (message-log-max nil)
         (alist-entry (format "(\"\\\\*NEW-BUFFER-NAME\\\\*\"
 (display-buffer-reuse-window display-buffer-in-side-window)
 (side . %s)
 (slot . %d)
 (window-%s . %.2f)
 (reusable-frames . visible))"
                              (if (> width height) 'right 'bottom)
                              (length display-buffer-alist)
                              (if (> width height) "width" "height")
                              (/ (float (if (> width height) width height))
                                 (if (> width height)
                                     (frame-width)
                                   (frame-height))))))
    (message
     (concat
      (propertize "Window Geometry" 'face 'font-lock-keyword-face)
      "\n"
      (propertize "Position: " 'face 'font-lock-function-name-face)
      (propertize (format "(%d, %d)" left top) 'face 'font-lock-constant-face)
      "  "
      (propertize "Size: " 'face 'font-lock-function-name-face)
      (propertize (format "%dx%d" width height) 'face 'success)
      "\n"
      (propertize "Left: " 'face 'font-lock-variable-name-face)
      (propertize (number-to-string left) 'face 'font-lock-constant-face)
      "  "
      (propertize "Top: " 'face 'font-lock-variable-name-face)
      (propertize (number-to-string top) 'face 'font-lock-constant-face)
      "  "
      (propertize "Right: " 'face 'font-lock-variable-name-face)
      (propertize (number-to-string right) 'face 'font-lock-constant-face)
      "  "
      (propertize "Bottom: " 'face 'font-lock-variable-name-face)
      (propertize (number-to-string bottom) 'face 'font-lock-constant-face)
      "\n\n"
      (propertize "Clipboard-ready alist entry:" 'face 'font-lock-keyword-face)
      "\n"
      (propertize alist-entry 'face 'font-lock-comment-face)))
    (kill-new alist-entry)))

(global-set-key (kbd "C-c w") 'laluxx/copy-window)


;;[ ] TODO Check also "~/.emacs.d/"
(defun laluxx/find-init ()
  "Open the Emacs init file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(global-set-key (kbd "C-c f") 'laluxx/find-init)

(defun laluxx/dashboard-dired ()
  (interactive)
  (laluxx/dired-jump-or-kill)
  (toggle-modeline)
  (enlarge-window 2)
  )

;; TODO it shoudl behave better
(defun laluxx/tab-cycle ()
  "Custom TAB behavior:
   - If on a comment heading (;;;), call `bicycle-cycle'
   - Otherwise, perform normal indentation"
  (interactive)
  (let* ((line-start (save-excursion
                       (beginning-of-line)
                       (point)))
         (line-content (buffer-substring-no-properties
                        line-start
                        (line-end-position)))
         (is-comment-heading (string-match-p "^[ \t]*;;;\\s-*[A-Z]" line-content))
         (proper-indent-pos (save-excursion
                              (beginning-of-line)
                              (skip-chars-forward " \t")
                              (point)))
         (current-pos (point)))
    (if is-comment-heading
        (if (= proper-indent-pos current-pos)
            ;; If we're at proper indentation, cycle the heading
            (bicycle-cycle)
          ;; If not properly indented, first indent then prepare for next tab
          (indent-for-tab-command)
          (message "Press TAB again to cycle the heading"))
      ;; Not a comment heading, do normal indentation
      (indent-for-tab-command))))


;;; PERSONAL
(defun laluxx/delete-blank-lines ()
  "Enhanced version of delete-blank-lines with smart indentation.
In programming modes:
- First attempts to indent the line properly
- If indentation fails or isn't supported, removes leading whitespace
For blank lines:
- Removes the line if it's only whitespace
- Collapses multiple blank lines like delete-blank-lines"
  (interactive)
  (let ((start (point)))
    ;; First handle the current line
    (beginning-of-line)
    (let* ((line-start (point))
           (orig-col (current-column)))
      (end-of-line)
      (let* ((line-end (point))
             (line-content (buffer-substring-no-properties line-start line-end)))
        (if (string-match-p "^[ \t]*$" line-content)
            ;; Line is only whitespace - delete it entirely
            (delete-region line-start (1+ line-end))
          ;; Line has content - try indent first, then clean if needed
          (beginning-of-line)
          (let ((indent-success nil))
            ;; Try to indent in programming modes
            (when (derived-mode-p 'prog-mode)
              (condition-case nil
                  (progn
                    ;; Try indent command specific to the mode
                    (indent-according-to-mode)
                    (setq indent-success t))
                (error nil)))
            
            ;; If indentation didn't work or wasn't available, clean whitespace
            (unless indent-success
              (delete-region line-start line-end)
              (insert (string-trim line-content)))))))
    
    ;; Now handle multiple blank lines
    (goto-char start)
    (let ((prev-blank (save-excursion 
                        (forward-line -1)
                        (looking-at "^[ \t]*$")))
          (next-blank (save-excursion 
                        (forward-line 1)
                        (looking-at "^[ \t]*$"))))
      (when (and prev-blank next-blank)
        (delete-blank-lines)))))

(global-set-key (kbd "C-x C-o") 'laluxx/delete-blank-lines)

;; TODO Better string support
;; kill the entire string directly when going from left <-- right
;; make sure that it doesn't stop on strings when going from left <-- right

(defun laluxx/kill-word ()
  "Kill the entire word under the cursor, regardless of point position within the word.
If at the end of a line (even with trailing characters), kill the word to the left.
If inside a string, kill from point to after the opening quote.
Otherwise, if no word is under the cursor or at word boundary, move to the next word and kill that word."
  (interactive)
  (let ((ppss (syntax-ppss)))
    (if (nth 3 ppss)  ; Inside a string
        (let ((string-start (1+ (nth 8 ppss))))  ; Move past the opening quote
          (if (> (point) string-start)
              (kill-region string-start (point))
            (message "Already at string beginning")))
      ;; Not in a string - original word killing logic
      (let ((bounds (bounds-of-thing-at-point 'word))
            (next-word-same-line
             (save-excursion
               (let ((current-line (line-number-at-pos)))
                 (forward-word)
                 (and (= current-line (line-number-at-pos))
                      (bounds-of-thing-at-point 'word)))))
            kill-start kill-end)
        (cond
         ;; No word under cursor and no next word on same line - kill word to the left
         ((and (not bounds) (not next-word-same-line))
          (save-excursion
            (backward-word)
            (setq bounds (bounds-of-thing-at-point 'word))
            (when bounds
              (setq kill-start (car bounds)
                    kill-end (cdr bounds)))))
         
         ;; Word under cursor but point is after word end
         ((and bounds 
               (> (point) (car bounds))
               (>= (point) (cdr bounds)))
          (setq bounds nil))  ; treat as if no word under cursor
         
         ;; Word under cursor
         (bounds
          (setq kill-start (car bounds)
                kill-end (cdr bounds)))
         
         ;; No word under cursor - try next word
         (t
          (forward-word)
          (setq bounds (bounds-of-thing-at-point 'word))
          (when bounds
            (setq kill-start (car bounds)
                  kill-end (cdr bounds)))))
        
        (when kill-start
          (let ((orig-start kill-start))
            (kill-region kill-start kill-end)
            (goto-char orig-start)))
        
        (unless kill-start
          (message "No word found"))))))

(defun laluxx/kill-word-or-kill-region ()
  "Kill region if active, otherwise kill whole word.
Uses custom 'kill-whole-word' when no region is active."
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-region)
    (laluxx/kill-word)))

(global-set-key (kbd "C-w") 'laluxx/kill-word-or-kill-region)




(defun laluxx/yank-eval ()
  "Yank text and evaluate it as Emacs Lisp code."
  (interactive)
  (yank)
  (eval-last-sexp nil))

(global-set-key (kbd "C-M-y") 'laluxx/yank-eval)

(defun laluxx/diff-buffer-with-file (&optional arg)
  "Compare buffer to its file, else run `vc-diff'.
With \\[universal-argument] also enable highlighting of word-wise
changes, local to the current buffer."
  (interactive "P")
  (let ((buf nil))     ; this method will "fail" if multi diff buffers
    (if (buffer-modified-p)
        (progn
          (diff-buffer-with-file (current-buffer))
          (setq buf "*Diff*"))
      (vc-diff)
      (setq buf "*vc-diff*"))
    (when arg
      (with-current-buffer (get-buffer buf)
        (setq-local diff-refine 'font-lock)))))



;;; FORMAT


(defun laluxx/format-assignment-operators-in-region ()
  "Format the selected region to align the assignment operators."
  (interactive)
  (let* ((start (region-beginning))
         (end (region-end))
         (region-text (buffer-substring start end))
         formatted-text max-length)
    ;; Split the region into lines
    (setq lines (split-string region-text "\n"))
    ;; Calculate the maximum length of the variable names
    (setq max-length (apply 'max (mapcar (lambda (line)
                                           (length (car (split-string line "="))))
                                         lines)))
    ;; Construct the formatted text
    (setq formatted-text 
          (mapconcat (lambda (line)
                       (if (string-match "\\(.*\\)=\\(.*\\)" line)
                           (let ((var-name (match-string 1 line))
                                 (rest (match-string 2 line)))
                             (format "%s=%s" 
                                     (concat var-name (make-string (- max-length (length var-name)) ? ))
                                     rest))
                         line))
                     lines "\n"))
    ;; Replace the region with the formatted text
    (delete-region start end)
    (goto-char start)
    (insert formatted-text)))

(defun laluxx/format-parameters-in-region ()
  "Format the selected region to align keys, functions, descriptions, and categories while preserving existing indentation."
  (interactive)
  (let* ((start (region-beginning))
         (end (region-end))
         (region-text (buffer-substring start end))
         formatted-text lines key-length func-length desc-length)
    ;; Split the region into lines
    (setq lines (split-string region-text "\n"))
    ;; Initialize maximum lengths
    (setq key-length 0 func-length 0 desc-length 0)
    ;; Calculate the maximum lengths for each column
    (dolist (line lines)
      (when (string-match "^[ \t]*{\\\"\\([^\\\"]+\\)\\\", \\([^,]+\\), \\\"\\([^\\\"]+\\)\\\", \\([^}]+\\)}" line)
        (setq key-length (max key-length (length (match-string 1 line))))
        (setq func-length (max func-length (length (match-string 2 line))))
        (setq desc-length (max desc-length (length (match-string 3 line))))))
    ;; Construct the formatted text
    (setq formatted-text 
          (mapconcat (lambda (line)
                       (if (string-match "^\\([ \t]*\\){\\\"\\([^\\\"]+\\)\\\", \\([^,]+\\), \\\"\\([^\\\"]+\\)\\\", \\([^}]+\\)}" line)
                           (let ((indent (match-string 1 line))
                                 (key (match-string 2 line))
                                 (func (match-string 3 line))
                                 (desc (match-string 4 line))
                                 (cat (match-string 5 line)))
                             (format "%s{\"%s\",%s %s,%s \"%s\",%s %s},"
                                     indent
                                     key
                                     (make-string (- key-length (length key)) ? )
                                     func
                                     (make-string (- func-length (length func)) ? )
                                     desc
                                     (make-string (- desc-length (length desc)) ? )
                                     cat))
                         line))  ; Preserve lines that don't match
                     lines "\n"))
    ;; Replace the region with the formatted text
    (delete-region start end)
    (goto-char start)
    (insert formatted-text)))

;; TODO Make it smarter, call 'laluxx/format-assignment-operators-in-region' only if there is an '=' the same for parameters
(defun laluxx/format-region ()
  "Format region"
  (interactive)
  (laluxx/format-assignment-operators-in-region)
  (laluxx/format-parameters-in-region))


(defun align-columns-interactive (begin end)
  "Align columns in the selected region interactively."
  (interactive "r")
  (let* ((lines (split-string (buffer-substring begin end) "\n"))
         (fields (mapcar (lambda (line) (split-string line nil t)) lines))
         (num-columns (apply #'max (mapcar #'length fields)))
         (max-lengths (make-vector num-columns 0))
         (alignments (make-vector num-columns 'right))
         (formatted-lines '())
         (current-column 0))
    
    ;; Find the maximum length for each column
    (dolist (line fields)
      (cl-loop for field in line
               for i from 0
               do (setf (aref max-lengths i)
                        (max (aref max-lengths i) (length field)))))
    
    ;; Interactive alignment choice
    (while (< current-column num-columns)
      (message "Column %d/%d (h:left, l:right, !:right-align remaining): " 
               (1+ current-column) num-columns)
      (let ((key (read-char-exclusive)))
        (cond
         ((char-equal key ?h)
          (setf (aref alignments current-column) 'left)
          (setq current-column (1+ current-column)))
         ((char-equal key ?l)
          (setf (aref alignments current-column) 'right)
          (setq current-column (1+ current-column)))
         ((char-equal key ?!)
          (setq current-column num-columns))
         (t (message "Invalid input. Try again.")
            (sit-for 1)))))
    
    ;; Format each line
    (dolist (line fields)
      (let ((formatted-fields '()))
        (cl-loop for field in line
                 for i from 0
                 for align = (aref alignments i)
                 do (push (if (eq align 'left)
                              (format (format "%%-%ds" (aref max-lengths i)) field)
                            (format (format "%%%ds" (aref max-lengths i)) field))
                          formatted-fields))
        (push (string-trim-right (mapconcat #'identity (nreverse formatted-fields) " ")) formatted-lines)))
    
    ;; Replace the region with formatted text
    (delete-region begin end)
    (insert (mapconcat #'identity (nreverse formatted-lines) "\n"))))
(defun laluxx/copy-project ()
  "Concatenate the content of all .c and .h files in the current directory and copy to the clipboard."
  (interactive)
  (let ((files (directory-files "." t "\\.[ch]$"))
        (content ""))
    (dolist (file files)
      (setq content (concat content (when (file-readable-p file)
                                      (with-temp-buffer
                                        (insert-file-contents file)
                                        (buffer-string)))
                            "\n\n"))) ; Add extra newlines between files for readability
    (unless (string= content "")
      (kill-new content)
      (message "Copied content of .c and .h files to clipboard."))))



;;; FAKE LSP
;; TODO laluxx/header-jump
(defun goto-implementation ()
  (interactive)
  ;; Extract the current line.
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    ;; Extract the function name from the current line.
    (if (string-match "\\b\\([_a-zA-Z][_a-zA-Z0-9]*\\) *(" line)
        (let* ((function-name (match-string 1 line))
               ;; Replace the extension of the current file from .h to .c
               (source-file (concat (file-name-sans-extension (buffer-file-name)) ".c")))
          (if (file-exists-p source-file)
              (progn
                ;; Open the .c file
                (find-file source-file)
                ;; Go to the beginning of the buffer to start the search
                (goto-char (point-min))
                ;; Search for the function implementation.
                (unless (search-forward (concat function-name "(") nil t)
                  (message "Function implementation not found for %s" function-name)))
            (message "Source file does not exist: %s" source-file)))
      (message "Could not extract function name from the current line."))))



;; TODO
(defun laluxx/unique-base-names (files)
  "Extract unique base names from a list of file names."
  (let ((names (mapcar #'file-name-sans-extension files)))
    (seq-uniq names)))

(defun laluxx/find-module ()
  "Find modules in the current directory, close all other windows, and open selected .c and .h files side by side."
  (interactive)
  (let* ((files (directory-files default-directory nil "\\.\\(c\\|h\\)$"))
         (base-names (laluxx/unique-base-names files))
         (ivy-action (lambda (base-name)
                       (delete-other-windows)  ; Close all other windows
                       (find-file (concat base-name ".c")) ; Open .c file in the current window
                       (split-window-right)    ; Split window to the right
                       (other-window 1)        ; Move to the new window
                       (find-file (concat base-name ".h"))  ; Open .h file
                       (other-window -1))))    ; Return focus to the .c file
    (ivy-read "Select a module: " base-names
              :action ivy-action)))


;;; GIT
(defun laluxx/git-diff ()
  "Show all changes since the last commit in a new buffer."
  (interactive)
  (let* ((buffer-name "*changes-since-last-commit*")
         (default-directory (vc-git-root default-directory))
         (changes (shell-command-to-string "git diff HEAD")))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert changes)
      (diff-mode)
      (goto-char (point-min)))
    (switch-to-buffer buffer-name)))


;;; FACES


;; Extract colors from you current theme:
;; 
;; (default-bg "")
;; (default-fg "") <- Transform this
;; (region-bg  "")
;; (highlight  "")
;;
;; (default-bg "#1d1f21") NOTE The -bg and -fg convention
;; (default-fg "#c5c8c6") <- Into this
;; (region-bg  "#333537") 
;; (highlight  "#0d0d0d")
(defun laluxx/generate-face-colors ()
  "Replace face definitions in the region with face names and their foreground or background colors."
  (interactive)
  (let* ((region-begin (region-beginning))
         (region-end (region-end))
         (region-content (buffer-substring-no-properties region-begin region-end))
         (face-lines (split-string region-content "\n" t "[ \t\n\r]+"))
         (max-length (apply #'max 
                            (mapcar (lambda (line)
                                      (when (string-match "(\\([^ \t\n]+\\)" line)
                                        (length (match-string 1 line))))
                                    face-lines)))
         (new-content "")
         (packages-to-try '(org erc)))
    
    ;; Try to load packages only if they're not already loaded
    (dolist (package packages-to-try)
      (unless (featurep package)
        (condition-case nil
            (require package)
          (error (message "Couldn't load package %s" package)))))
    
    (dolist (line face-lines)
      (if (string-match "(\\([^ \t\n]+\\)\\s-+\"\\([^\"]*\\)\")" line)
          (let* ((face-name (match-string 1 line))
                 (current-color (match-string 2 line))
                 (padding (- max-length (length face-name)))
                 (base-face-name (replace-regexp-in-string "-[fb]g$" "" face-name))
                 (face-symbol (intern base-face-name))
                 (fg-or-bg (if (string-match "-bg$" face-name) 'background 'foreground))
                 (color-func (if (eq fg-or-bg 'background) 'face-background 'face-foreground))
                 (color (cond
                         ((facep face-symbol)
                          (funcall color-func face-symbol nil t))
                         ((find-face-definition face-symbol)
                          (let ((face (find-face-definition face-symbol)))
                            (cdr (assq fg-or-bg (cadr face)))))
                         (t nil))))
            (setq new-content 
                  (concat new-content 
                          (format "       (%s%s \"%s\")\n" 
                                  face-name
                                  (make-string padding ? )
                                  (or color current-color)))))
        (setq new-content (concat new-content line "\n"))))
    
    (delete-region region-begin region-end)
    (goto-char region-begin)
    (insert new-content)
    (indent-region region-begin (point))))


;; (outline-1        "#8AC6F2")
;; (outline-2        "#E18Cbb")   <- Transform this
;; (outline-3        "#BF93C3")
;; (outline-4        "#a7d4f5")
;; (outline-5        "#e8a8cb")
;; (outline-6        "#c4e2f8")
;;
;; `(outline-1 ((t (:foreground ,outline-1))))
;; `(outline-2 ((t (:foreground ,outline-2))))
;; `(outline-3 ((t (:foreground ,outline-3))))  <- Into this
;; `(outline-4 ((t (:foreground ,outline-4))))
;; `(outline-5 ((t (:foreground ,outline-5))))
;; `(outline-6 ((t (:foreground ,outline-6))))
(defun generate-face-definitions ()
  "Generate face definitions from ansi-color variables in region.
Each variable name becomes both the face name and its foreground color variable."
  (interactive)
  (let* ((region-content (buffer-substring-no-properties (region-beginning) (region-end)))
         (color-lines (split-string region-content "\n" t "[ \t\n\r]+"))
         (start (point))
         ;; Find the longest name to determine padding
         (max-length (apply #'max 
                            (mapcar (lambda (line)
                                      (when (string-match "(\\([^ \t\n]+\\)" line)
                                        (length (match-string 1 line))))
                                    color-lines))))
    (insert "\n")
    (dolist (line color-lines)
      (when (string-match "(\\([^ \t\n]+\\)" line)
        (let* ((color-name (match-string 1 line))
               (padding (- max-length (length color-name))))
          (insert (format "   `(%s%s ((t (:foreground ,%s))))\n" 
                          color-name
                          (make-string padding ? )
                          color-name)))))
    (indent-region start (point))))


(defun list-matching-commands (prefix)
  "Generate a buffer with all commands that match the given PREFIX."
  (interactive "sEnter command prefix: ")
  (let ((matching-commands '())
        (buffer-name "*Matching Commands*"))
    
    ;; Collect all matching commands
    (mapatoms
     (lambda (symbol)
       (when (and (commandp symbol)
                  (string-prefix-p prefix (symbol-name symbol)))
         (push symbol matching-commands))))
    
    ;; Sort the commands alphabetically
    (setq matching-commands (sort matching-commands #'string-lessp))
    
    ;; Create or switch to the output buffer
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))  ; Allow modification of read-only buffer
        (erase-buffer)
        (goto-char (point-min))
        
        ;; Insert header
        (insert (format "Commands matching prefix '%s':\n\n" prefix))
        
        ;; Insert each matching command with its documentation
        (dolist (cmd matching-commands)
          (insert (format "Command: %s\n" cmd))
          (let ((doc (documentation cmd)))
            (if doc
                (insert (format "  Documentation: %s\n" 
                                (replace-regexp-in-string "\n" "\n               " doc)))
              (insert "  Documentation: Not available\n")))
          (insert "\n"))
        
        ;; Set buffer to read-only and enable view-mode
        (read-only-mode 1)
        (view-mode 1)))
    
    ;; Switch to the buffer
    (switch-to-buffer buffer-name)
    
    ;; Return the number of matching commands
    (length matching-commands)))

(defun laluxx/duplicate-region-replace ()
  "Duplicate the current region and automatically replace all occurrences in the duplicated content."
  (interactive)
  (if (not (region-active-p))
      (message "No active region")
    (let* ((start (region-beginning))
           (end (region-end))
           (original-text (buffer-substring-no-properties start end))
           ;; Use query-replace-read-args which properly sets up the replacement
           (args (query-replace-read-args "Text to replace: " t))
           (from-string (nth 0 args))
           (to-string (nth 1 args))
           ;; Calculate where the duplicated region will be
           (dup-start (1+ end))
           (dup-end (+ dup-start (length original-text))))
      ;; Insert duplicated text after the region
      (goto-char end)
      (insert "\n" original-text)
      ;; Replace all occurrences in the duplicated region without prompting
      (save-excursion
        (save-restriction
          ;; Narrow to just the duplicated region to prevent affecting rest of buffer
          (narrow-to-region dup-start dup-end)
          (goto-char (point-min))
          (while (search-forward from-string nil t)
            (replace-match to-string t t)))))))

;; TODO keep it in one line
(defun laluxx/eval-region (start end)
  "Evaluate region with enhanced feedback about compilation and timing."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (bytes (string-bytes region-text))
         (chars (length region-text))
         (lines (count-lines start end))
         (time-before (current-time))
         (mem-before (car (memory-info)))
         (error-buffer (get-buffer "*Warnings*"))
         (initial-warning-size (when error-buffer
                                 (with-current-buffer error-buffer
                                   (buffer-size))))
         (result (eval (read region-text)))
         (elapsed (float-time (time-subtract (current-time) time-before)))
         (mem-after (car (memory-info)))
         (mem-diff (- mem-after mem-before))
         (error-buffer (get-buffer "*Warnings*"))
         (final-warning-size (when error-buffer
                               (with-current-buffer error-buffer
                                 (buffer-size))))
         (new-warnings (if (and initial-warning-size final-warning-size)
                           (> final-warning-size initial-warning-size)
                         nil))
         (native-comp-available (and (fboundp 'native-comp-available-p)
                                     (native-comp-available-p)))
         (was-native (and native-comp-available
                          (functionp result)
                          (subr-native-elisp-p result))))
    
    (message
     "%s %s\n%s %s  %s %s\n%s %s  %s %s  %s %s"
     (propertize "EVALUATED:" 'face 'font-lock-keyword-face)
     (let ((result-str (format "%S" result)))
       (if (> (length result-str) 40)
           (concat (substring result-str 0 37) "...")
         result-str))
     
     (propertize "Time:" 'face 'font-lock-builtin-face)
     (propertize (format "%.6f sec" elapsed) 'face 'font-lock-constant-face)
     
     (propertize "Size:" 'face 'font-lock-builtin-face)
     (propertize (format "[%d lines, %d chars, %s]"
                         lines chars (laluxx/byte-size-human-readable bytes))
                 'face 'font-lock-constant-face)
     
     (propertize "Native:" 'face 'font-lock-builtin-face)
     (propertize (if was-native "yes" "no") 
                 'face (if was-native 'font-lock-string-face 'font-lock-comment-face))
     
     (propertize "Warnings:" 'face 'font-lock-builtin-face)
     (propertize (cond (new-warnings "yes")
                       ((eq new-warnings nil) "no")
                       (t "unknown"))
                 'face (if new-warnings 'font-lock-warning-face 'font-lock-string-face))
     
     (propertize "Memory:" 'face 'font-lock-builtin-face)
     (propertize (format "%+s" (laluxx/byte-size-human-readable mem-diff))
                 'face 'font-lock-constant-face))))

(defun laluxx/byte-size-human-readable (bytes)
  "Convert bytes to human readable format."
  (cond ((< bytes 1024) (format "%d B" bytes))
        ((< bytes (* 1024 1024)) (format "%.2f KB" (/ bytes 1024.0)))
        ((< bytes (* 1024 1024 1024)) (format "%.2f MB" (/ bytes (* 1024.0 1024))))
        (t (format "%.2f GB" (/ bytes (* 1024.0 1024 1024))))))


(defun toggle-buffer (buffer-name open-func)
  "Toggle buffer with BUFFER-NAME.
If buffer doesn't exist, call OPEN-FUNC to create it.
If buffer exists but isn't current, switch to it.
If buffer is current, bury it."
  (interactive)
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (if (equal (current-buffer) buffer)
            (bury-buffer)
          (switch-to-buffer buffer))
      (funcall open-func))))

(defun laluxx/adjust-hex-color (hex-string delta)
  "Adjust each RGB component of HEX-STRING by DELTA."
  (let* ((rgb-str (substring hex-string 1))  ; Remove # prefix
         (rgb-val (string-to-number rgb-str 16))
         (r (lsh rgb-val -16))
         (g (logand (lsh rgb-val -8) #xFF))
         (b (logand rgb-val #xFF))
         (new-r (min 255 (max 0 (+ r delta))))
         (new-g (min 255 (max 0 (+ g delta))))
         (new-b (min 255 (max 0 (+ b delta)))))
    (format "#%02x%02x%02x" new-r new-g new-b)))

(defun laluxx/adjust-background-opacity (delta)
  "Adjust the background opacity by DELTA."
  (let* ((current-alpha (or (frame-parameter nil 'alpha-background) 100))
         (new-alpha (max 0 (min 100 (+ current-alpha delta)))))
    (set-frame-parameter nil 'alpha-background new-alpha)
    (message "Background opacity set to %d%%" new-alpha)))

(defun laluxx/smart-increment-at-point (&optional n)
  "Increment number, adjust hex color at point, or increase background opacity by N (default 1)."
  (interactive "p")
  (or n (setq n 1))
  (save-excursion
    (skip-chars-backward "0123456789abcdefABCDEF#")
    (cond
     ;; Hex color case
     ((looking-at "#[0-9a-fA-F]\\{6\\}")
      (let* ((hex-color (match-string 0))
             (new-color (laluxx/adjust-hex-color hex-color n)))
        (replace-match new-color)))
     ;; Number case
     ((looking-at "[0-9]+")
      (replace-match
       (number-to-string (+ (string-to-number (match-string 0)) n))))
     ;; Adjust background opacity if no number or hex color found
     (t (laluxx/adjust-background-opacity n)))))

(defun laluxx/smart-decrement-at-point (&optional n)
  "Decrement number, adjust hex color at point, or decrease background opacity by N (default 1)."
  (interactive "p")
  (laluxx/smart-increment-at-point (- (or n 1))))

;; Key bindings
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'laluxx/smart-increment-at-point)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'laluxx/smart-decrement-at-point)
(global-set-key (kbd "C-x <up>") 'laluxx/smart-increment-at-point)
(global-set-key (kbd "C-x <down>") 'laluxx/smart-decrement-at-point)

(defun laluxx/mwim--in-string-p ()
  "Return string bounds if point is inside a string, nil otherwise.
Returns (START . END) where START and END are positions inside the
string, excluding the quote characters."
  (let ((state (syntax-ppss)))
    (when (nth 3 state)  ; nth 3 is non-nil when in string
      (let ((start (nth 8 state))  ; start of string including quote
            (end (save-excursion
                   (parse-partial-sexp (point) (point-max)
                                       nil nil state 'syntax-table)
                   (point))))  ; end of string including quote
        (cons (1+ start)      ; exclude opening quote
              (1- end))))))   ; exclude closing quote


(defun laluxx/mwim-beginning-of-line ()
  "Move to beginning of line or string intelligently.
If inside a string:
- First call moves to beginning of string content
- Second call moves to beginning of line
If not in string, moves to beginning of line."
  (interactive)
  (let ((string-bounds (laluxx/mwim--in-string-p))
        (orig-point (point)))
    (if (not string-bounds)
        (beginning-of-line)
      (if (= (point) (car string-bounds))
          (beginning-of-line)
        (goto-char (car string-bounds))))))

(defun laluxx/mwim-end-of-line ()
  "Move to end of line or string intelligently.
If inside a string:
- First call moves to end of string content
- Second call moves to end of line
If not in string, moves to end of line."
  (interactive)
  (let ((string-bounds (laluxx/mwim--in-string-p))
        (orig-point (point)))
    (if (not string-bounds)
        (end-of-line)
      (if (= (point) (cdr string-bounds))
          (end-of-line)
        (goto-char (cdr string-bounds))))))

;; TODO only in prog-mode
(global-set-key (kbd "C-a") #'laluxx/mwim-beginning-of-line)
(global-set-key (kbd "C-e") #'laluxx/mwim-end-of-line)



(defun laluxx/copy-last-message ()
  "Copy the last message from the *Messages* buffer."
  (interactive)
  (save-excursion
    (let ((msg nil))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (forward-line -1)
        (setq msg (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
      (if msg
          (progn
            (kill-new msg)
            (message "Copied: %s" msg))
        (message "No message found")))))

(global-set-key (kbd "C-h C-m") 'laluxx/copy-last-message)





(defun laluxx/get-effective-color (face color-type)
  "Get effective color for FACE based on COLOR-TYPE ('foreground or 'background).
Recursively checks inherited faces until a color is found or there are no more faces to check."
  (let ((color (if (eq color-type 'foreground)
                   (face-foreground face nil t)
                 (face-background face nil t))))
    (cond
     ;; If we found a color, return it
     (color color)
     ;; If face is 'default and still no color, return nil
     ((eq face 'default) nil)
     ;; Check inherited faces
     (t
      (let ((inherited (face-attribute face :inherit nil t)))
        (cond
         ((null inherited) (laluxx/get-effective-color 'default color-type))
         ((symbolp inherited) (laluxx/get-effective-color inherited color-type))
         ((listp inherited)
          (cl-loop for iface in inherited
                   for icolor = (laluxx/get-effective-color iface color-type)
                   when icolor return icolor
                   finally return (laluxx/get-effective-color 'default color-type)))))))))


(defun laluxx/get-face-color (face-name type)
  "Get the color of FACE-NAME for TYPE (foreground or background)."
  (let ((face (intern-soft face-name)))
    (when (facep face)
      (if (eq type 'foreground)
          (face-foreground face)
        (face-background face)))))

(defun laluxx/copy-color (type)
  "Copy the color of TYPE (foreground or background) under point or of face name if detected."
  (let* ((text-at-point (thing-at-point 'symbol))
         (face-color (when text-at-point
                       (laluxx/get-face-color text-at-point type))))
    (if face-color
        (progn
          (kill-new face-color)
          (message "Copied %s of face %s: %s"
                   (if (eq type 'foreground) "foreground" "background")
                   text-at-point
                   (propertize (format "%s" face-color)
                               'face `(,(if (eq type 'foreground) :foreground :background) ,face-color)
                               'help-echo face-color)))
      (let* ((faces (let ((face (get-text-property (point) 'face)))
                      (cond ((null face) (list 'default))
                            ((listp face) face)
                            (t (list face)))))
             (color (cl-loop for face in faces
                             for col = (laluxx/get-effective-color face type)
                             when col return col
                             finally return (if (eq type 'foreground)
                                                (face-foreground 'default)
                                              (face-background 'default)))))
        (if color
            (progn
              (kill-new color)
              (message "Copied %s: %s"
                       (if (eq type 'foreground) "foreground" "background")
                       (propertize (format "%s" color)
                                   'face `(,(if (eq type 'foreground) :foreground :background) ,color)
                                   'help-echo color)))
          (message "No %s color found" (if (eq type 'foreground) "foreground" "background")))))))

(defun laluxx/copy-foreground-color ()
  "Copy the foreground color of the face name under point or character under point."
  (interactive)
  (laluxx/copy-color 'foreground))

(defun laluxx/copy-background-color ()
  "Copy the background color of the face name under point or character under point."
  (interactive)
  (laluxx/copy-color 'background))

(global-set-key (kbd "C-h C-h") 'laluxx/copy-foreground-color)
(global-set-key (kbd "C-h h") 'laluxx/copy-background-color)
(global-set-key (kbd "C-h C-M-h") 'find-face-definition)


(defun animate-message (text &optional steps delay)
  "Animate TEXT by moving it to the right by adding spaces.
STEPS controls how many spaces to add (default: 20)
DELAY controls animation speed in seconds (default: 0.1)"
  (interactive "sText to animate: ")
  (let* ((steps (or steps 89)) ; TODO calculate the steps
         (delay (or delay 0.025))
         (message-log-max nil))
    (dotimes (i steps)
      (let ((spaces (make-string i ?\s)))
        (message "%s%s" spaces text)
        (sit-for delay)))))


(defun laluxx/kill-strings-in-region (start end)
  "Kill texts inside double quotes in the specified region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\"\\([^\"]*\\)\"" end t)
      (replace-match "\"\"" nil nil))))

(defun laluxx/kill-ring-save-line ()
  "Copy the current line to the kill ring without deleting it."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-beginning-position 2))
  (message "Line copied to kill ring"))

(global-set-key (kbd "C-S-k") 'laluxx/kill-ring-save-line)


;; TODO Color the output message for better readabilty
(defun laluxx/convert (input)
  "Convert INPUT (in decimal, hexadecimal with 0x, octal with 0o, binary with 0b prefix, or ASCII)
to decimal, hexadecimal, octal, binary formats, and ASCII characters. If a region is active, use its contents as INPUT."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Enter a number (decimal, hexadecimal with 0x, octal with 0o, binary with 0b prefix, or ASCII): "))))
  (let* ((decimal
          (cond
           ;; Binary input
           ((string-prefix-p "0b" input)
            (string-to-number (substring input 2) 2))
           ;; Hexadecimal input
           ((string-prefix-p "0x" input)
            (string-to-number (substring input 2) 16))
           ;; Octal input
           ((string-prefix-p "0o" input)
            (string-to-number (substring input 2) 8))
           ;; Decimal input
           ((string-match-p "\\`[0-9]+\\'" input)
            (string-to-number input))
           ;; ASCII input
           (t
            (let ((ascii-value (string-to-char input)))
              (if (characterp ascii-value)
                  (progn
                    (message "Input is ASCII: %s" input)
                    ascii-value)
                nil)))))
         (binary (format "%s" decimal))
         (hexadecimal (format "0x%X" decimal))
         (octal (format "0o%o" decimal))
         (ascii-char (if (and (characterp decimal) (< decimal 128))
                         (format "'%c'" (string-to-char (string decimal)))
                       "N/A")))
    (message "Input: %s ➜ Decimal: %d,  Hexadecimal: %s,  Octal: %s,  Binary: 0b%s,  ASCII: %s"
             input decimal hexadecimal octal binary ascii-char)))



(global-set-key (kbd "C-c o") 'laluxx/convert)


(defun laluxx/find-init ()
  "Open the Emacs init file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

(global-set-key (kbd "C-c C-f") 'laluxx/find-init)


(defun laluxx/copy-c-declaration ()
  "Copy the current C function declaration to the kill ring, suitable for header files."
  (interactive)
  (save-excursion
    (c-beginning-of-defun)
    (let (declaration start end return-type function-name parameters)
      (beginning-of-line)
      (setq start (point))
      ;; Find the position just before the function body begins
      (re-search-forward "{")
      (backward-char)
      (setq end (point))
      ;; Extract the function declaration
      (setq declaration (buffer-substring-no-properties start end))
      ;; Prepare the declaration for copying and display
      (setq declaration (replace-regexp-in-string "[[:space:]\n]+" " " declaration))
      (setq declaration (string-trim declaration))
      (setq declaration (concat declaration ";"))

      ;; Adjusted regex to capture any C identifier as the return type and function name
      (if (string-match "\\([a-zA-Z_][a-zA-Z0-9_]*[[:space:]*]+\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\)[[:space:]]*(\\(.*\\))" declaration)
          (setq return-type (match-string 1 declaration)
                function-name (match-string 2 declaration)
                parameters (match-string 3 declaration)))

      ;; Manually propertize the parsed declaration
      (setq declaration
            (concat (propertize return-type 'face 'font-lock-type-face)  ;; Return type (custom types too)
                    (propertize function-name 'face 'font-lock-function-name-face)  ;; Function name
                    "("
                    (propertize parameters 'face 'font-lock-variable-name-face)  ;; Parameters
                    ");"))

      ;; Copy to kill ring
      (kill-new declaration)
      ;; Display in the minibuffer with properties
      (message "%s %s"
               (propertize "KILLED:" 'face 'error)
               declaration))))

(global-set-key (kbd "C-c k") 'laluxx/copy-c-declaration)

(defun laluxx/copy-project ()
  "Concatenate the content of all .c and .h files in the current directory and copy to the clipboard."
  (interactive)
  (let ((files (directory-files "." t "\\.[ch]$"))
        (content ""))
    (dolist (file files)
      (setq content (concat content (when (file-readable-p file)
                                      (with-temp-buffer
                                        (insert-file-contents file)
                                        (buffer-string)))
                            "\n\n"))) ; Add extra newlines between files for readability
    (unless (string= content "")
      (kill-new content)
      (message "Copied content of .c and .h files to clipboard."))))

(defun laluxx/cycle-line-numbers ()
  "Cycle between line number modes: absolute, relative, none."
  (interactive)
  (cond
   ((eq display-line-numbers t)
    (setq display-line-numbers 'relative))

   ((eq display-line-numbers 'relative)
    (setq display-line-numbers nil))

   (t (setq display-line-numbers t))))


(defun laluxx/random-numbers ()
  "Replace all numbers in the current buffer or selected region with random numbers from 1 to 100, maintaining the format (integer or float)."
  (interactive)
  (save-excursion
    (let (replacements
          (start (if mark-active (region-beginning) (point-min)))
          (end (if mark-active (region-end) (point-max))))
      (goto-char start)
      ;; Collect all numbers and their positions within the specified range
      (while (and (< (point) end)
                  (re-search-forward "\\b\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\b" end t))
        (let* ((match-start (match-beginning 0))
               (match-end (match-end 0))
               (original (match-string 0))
               (is-float (string-match "\\." original))
               (random-number (1+ (random 100)))
               (replacement (if is-float
                                (format "%.1f" random-number)
                              (number-to-string random-number))))
          (push (list match-start match-end replacement) replacements)))
      ;; Replace all collected numbers
      (dolist (rep replacements)
        (goto-char (car rep))
        (delete-region (car rep) (cadr rep))
        (insert (caddr rep))))))

(defun laluxx/stringify-region (start end)
  "Put a double quote at the start and end of each line in the selection."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (beginning-of-line)
      (insert "\"")
      (end-of-line)
      (insert "\"")
      (forward-line 1)
      (setq end (+ end 2)))))

(defun laluxx/smart-hungry-delete-backward ()
  "Delete contiguous stream of whitespace backward, or a single character if no whitespace is found. Leaves a newline if whitespaces spans multiple lines."
  (interactive)
  (let ((start (point)))
    (skip-chars-backward " \t\n")
    (if (> (count-lines (point) start) 1)
        (progn
          ;; If spanning multiple lines, delete to just before the first newline encountered.
          (re-search-forward "\n" start t)
          (delete-region (point) start))
      (if (= (point) start)
          ;; If no whitespace is found, delete the previous character.
          (delete-char -1)
        ;; Otherwise, just delete the whitespace.
        (delete-region (point) start)))))

(global-set-key (kbd "C-c C-<backspace>") 'laluxx/smart-hungry-delete-backward)

(defun laluxx/find-header ()
  "Toggle between a C source file and its corresponding header file."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (extension (file-name-extension current-file))
         (base-name (file-name-sans-extension current-file))
         target-file)
    ;; Determine the target file based on the extension of the current file
    (setq target-file
          (cond ((string= extension "c") (concat base-name ".h"))
                ((string= extension "h") (concat base-name ".c"))
                (t (error "Not a C or Header file: %s" current-file))))
    ;; Check if the target file exists and open it
    (if (file-exists-p target-file)
        (find-file target-file)
      (message "File does not exist: %s" target-file))))

(global-set-key (kbd "C-x C-h") 'laluxx/find-header)


;; TODO Make this a package
;; ability to use eww instead of xdg-open
;; and chose the search engine using a varibale

(defun laluxx/google-this (query)
  "Search QUERY using default browser.
If region is active, search the selected text.
Otherwise, prompt for search query."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Google: "))))
  (shell-command (concat "xdg-open 'https://www.google.com/search?q="
                         (url-hexify-string query) "'")))


(defun laluxx/kill-whole-word ()
  "Kill the entire word under the cursor, regardless of point position within the word.
If no word is under the cursor, move to the next word and kill that word."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (progn
        (forward-word)
        (setq bounds (bounds-of-thing-at-point 'word))
        (if bounds
            (kill-region (car bounds) (cdr bounds))
          (message "No word found"))))))

(defun find-file-in-home ()
  "Find a file starting from the home directory."
  (interactive)
  (let ((default-directory (expand-file-name "~")))
    (call-interactively 'find-file)))

(global-set-key (kbd "C-x ~") 'find-file-in-home)

(defun laluxx/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun laluxx/copy-buffer ()
  "Copy the entire buffer to the clipboard."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (point-min) (point-max))))

(defun toggle-modeline ()
  "Toggle the visibility of the modeline in the current buffer."
  (interactive)
  (setq mode-line-format
        (if mode-line-format
            nil
          (default-value 'mode-line-format)))
  (force-mode-line-update))

;; TODO check first on the lisp folder then elpa
(defun laluxx/find-package-source-code ()
  "NOTE Work only with 'elpa' opens a .el file corresponding to the extended 'word' under the cursor in the ~/.config/emacs/elpa/ directory in a new window."
  (interactive)
  (save-excursion
    (let* ((start (progn (skip-chars-backward "^ \t\n") (point)))
	   (end (progn (skip-chars-forward "^ \t\n") (point)))
	   (package-name (buffer-substring-no-properties start end))
	   (elpa-dir "~/.config/emacs/elpa/")
	   (directories (directory-files elpa-dir t "\\`[^.].*")) ; ignore hidden dirs
	   matching-dir file-path found)

      ;; Find the first directory that starts with the package name and has a version
      (dolist (dir directories found)
	(when (string-match-p (format "\\`%s-.*" (regexp-quote package-name)) (file-name-nondirectory dir))
	  (setq matching-dir dir)
	  (setq found t)))

      (when matching-dir
	;; Assuming the main .el file has the same name as the package
	(setq file-path (concat matching-dir "/" package-name ".el"))

	;; Check if the .el file exists or fallback to any .el file
	(unless (file-exists-p file-path)
	  (let ((el-files (directory-files matching-dir t "\\.el\\'")))
	    (when el-files
	      (setq file-path (car el-files)))))

	(if (file-exists-p file-path)
	    (find-file-other-window file-path)  ; Open the file in a new window if it exists
	  (message "Elisp file does not exist: %s" file-path)))
      (unless found
	(message "No directory starts with: %s" package-name)))))

;; GARBAGE COLLECTOR

(defun laluxx/gc-with-message ()
  "Print formatted messages before and after garbage collection with timing information."
  (interactive)
  (message "Garbage collecting...")
  (garbage-collect)
  (message "Garbage collecting... %s" 
           (propertize "DONE" 'face 'font-lock-string-face)))

(defun laluxx/gc-advice (orig-fun &rest args)
  "Advice function to add timing information to garbage collection."
  (let ((start-time (current-time)))
    (message "Garbage collecting...")
    (apply orig-fun args)
    (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
           (formatted-time (format "%.2f seconds" elapsed)))
      (message "Garbage collecting... %s (%s)"
               (propertize "DONE" 'face 'font-lock-string-face)
               (propertize formatted-time 'face 'font-lock-comment-face)))))

(advice-add 'garbage-collect :around #'laluxx/gc-advice)
(global-set-key (kbd "C-h C-g") #'laluxx/gc-with-message)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-workspace-name nil)
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(child-frame-border ((t (:background "#171717"))))
 '(dired-subtree-depth-1-face ((t (:background unspecified))))
 '(dired-subtree-depth-2-face ((t (:background unspecified))))
 '(dired-subtree-depth-3-face ((t (:background unspecified))))
 '(dired-subtree-depth-4-face ((t (:background unspecified))))
 '(dired-subtree-depth-5-face ((t (:background unspecified))))
 '(diredfl-file-suffix ((t (:weight bold))))
 '(diredfl-number ((t (:weight bold))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-type-face ((t (:weight bold))))
 '(fringe ((t (:foreground "#7b7b7b"))))
 '(lsp-ui-doc-background ((t (:background "#171717"))))
 '(markdown-code-face ((t (:extend t :background "#1c1c1c"))))
 '(org-document-title ((t (:height 1.6))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1))))
 '(woman-bold ((t (:inherit font-lock-variable-name-face :height 1.0 :weight bold)))))

;;; LISP

(use-package doom-dashboard
  :load-path "~/.config/emacs/lisp/doom-dashboard"
  :hook (after-init . +doom-dashboard-init-h))

(use-package ewm
  :load-path "~/.config/emacs/lisp/ewm"
  :config (global-ewm-mode))

(use-package page-break-lines
  :load-path "~/.config/emacs/lisp/page-break-lines"
  :config (global-page-break-lines-mode))

(use-package dired-two-step
  :load-path "~/.config/emacs/lisp/dired-two-step")

;; (use-package dired-ffmpeg
;;   :load-path "~/.config/emacs/lisp/dired-ffmpeg"
;;   :config (dired-ffmpeg-setup))


(use-package dired-ffmpeg
  :load-path "~/.config/emacs/lisp/dired-ffmpeg"
  :custom
  (dired-ffmpeg-media-extensions 
   '("svg" "png" "jpg" "jpeg" "gif" "webp" 
     "mp4" "mkv" "avi" "mov" "flv" "wmv"))
  (dired-ffmpeg-presets
   '(("png" . ((:command "-vcodec png -compression_level 9")
               (:description "High-quality PNG with maximum compression")
               (:icon "🖼")))
     ("jpg" . ((:command "-qscale:v 2 -sampling-factor 4:2:0")
               (:description "High-quality JPEG with 4:2:0 chroma subsampling")
               (:icon "📸")))
     ("webp" . ((:command "-quality 90 -preset picture -lossless 0")
                (:description "WebP with excellent quality-size ratio")
                (:icon "🌐")))
     ("gif" . ((:command "-loop 0 -filter_complex [0:v] split [a][b];[a] palettegen [p];[b][p] paletteuse")
               (:description "Optimized GIF with generated palette")
               (:icon "🎞")))))
  :init
  ;; Initialize all internal variables with default values
  (defvar dired-ffmpeg--current-file nil)
  (defvar dired-ffmpeg--process nil)
  (defvar dired-ffmpeg--quality 90)
  (defvar dired-ffmpeg--scale "1920:-1")
  (defvar dired-ffmpeg--setup-done nil)
  :config
  (dired-ffmpeg-mode 1))



(use-package consult-themes
  :load-path "~/.config/emacs/lisp/consult-themes"
  :bind (("C-h t" . consult-dark-themes)
         ("C-c C-t" . consult-dark-themes)
         :map global-map ("C-c t" . consult-themes-prefix-map))
  :init
  ;; Define a keymap
  (define-prefix-command 'consult-themes-prefix-map)
  (define-key consult-themes-prefix-map (kbd "d") #'consult-dark-themes)
  (define-key consult-themes-prefix-map (kbd "l") #'consult-light-themes)
  (define-key consult-themes-prefix-map (kbd "u") #'consult-ugly-themes)
  )


(use-package objdump
  :load-path "~/.config/emacs/lisp/emacs-objdump-mode")

(use-package key-echo-mode
  :load-path "~/.config/emacs/lisp/key-echo-mode")

(use-package gif-screencast
  :load-path "~/.config/emacs/lisp/emacs-gif-screencast")

(use-package msh
  :load-path "~/.config/emacs/lisp/msh")

(use-package activate-emacs
  :load-path "~/.config/emacs/lisp/activate-emacs")

(use-package ikey
  :load-path "~/.config/emacs/lisp/ikey"
  :config (global-set-key (kbd "C-h c") 'ikey-describe-key))

(use-package matematica-mode
  :load-path "~/.config/emacs/lisp/matematica-mode")

(use-package org-block-icons
  :load-path "~/.config/emacs/lisp/org-block-icons")

;;; JADE

(use-package jade-mode
  :load-path "~/.config/emacs/lisp/jade-mode")

(defun laluxx/jade-save-hook ()
  "Run the compiler on the current file when saving a .jade file."
  (when (and (buffer-file-name)          ; Ensure there's a file associated with the buffer
             (string-match "\\.jade\\'" (buffer-file-name)))  ; Check if the file has a .jade extension
    (shell-command (concat "compiler " (shell-quote-argument (buffer-file-name))))))  ; Run compiler on the file

(add-hook 'after-save-hook 'laluxx/jade-save-hook)

;; (defun laluxx/guile-save-hook ()
;;   "Compile the current .scm file when saving it"
;;   (when (and (buffer-file-name)
;;              (string-match "\\.scm\\'" (buffer-file-name)))  ; Changed to .scm extension
;;     (let* ((input-file (buffer-file-name))
;;            (output-file (concat (file-name-sans-extension input-file) ".go")))  ; Output to .go
;;       (shell-command
;;        (format "guild compile %s -o %s"   ; Using guild compile for Guile
;;                (shell-quote-argument input-file)
;;                (shell-quote-argument output-file))))))

;; (add-hook 'after-save-hook 'laluxx/guile-save-hook)


(defun laluxx/qbe-save-hook ()
  "Run the QBE compiler on the current file when saving a .ssa file,
outputting assembly to a corresponding .s file."
  (when (and (buffer-file-name)
             (string-match "\\.ssa\\'" (buffer-file-name)))
    (let* ((input-file (buffer-file-name))
           (output-file (concat (file-name-sans-extension input-file) ".s")))
      (shell-command
       (format "qbe %s > %s"
               (shell-quote-argument input-file)
               (shell-quote-argument output-file))))))

(add-hook 'after-save-hook 'laluxx/qbe-save-hook)

(defun laluxx/as-save-hook ()
  "Assemble the current file when saving a .s file,
outputting executable to a corresponding .out file."
  (when (and (buffer-file-name)
             (string-match "\\.s\\'" (buffer-file-name)))
    (let* ((input-file (buffer-file-name))
           (output-file (concat (file-name-sans-extension input-file) ".out")))
      (shell-command
       (format "as %s -o %s && chmod +x %s"
               (shell-quote-argument input-file)
               (shell-quote-argument output-file)
               (shell-quote-argument output-file))))))

(add-hook 'after-save-hook 'laluxx/as-save-hook)



;; Transpiler
;; (defun laluxx/jade-save-hook ()
;;   "Run './jade' command if the current buffer is visiting a .jade file."
;;   (when (and (buffer-file-name)          ; Ensure there's a file associated with the buffer
;;              (string-match "\\.jade\\'" (buffer-file-name)))  ; Check if the file has a .jade extension
;;     (shell-command (concat "./jade " (shell-quote-argument (buffer-file-name))))))  ; Run the jade command on the file

;; (add-hook 'after-save-hook 'laluxx/jade-save-hook)


(use-package dired-git-info
  :load-path "~/.config/emacs/lisp/dired-git-info"
  )

;; TODO Make it faster
;; TODO Support more languages
(use-package hide-comments-mode
  :load-path "~/.config/emacs/lisp/hide-comments-mode")

(use-package consult-flycheck
  :load-path "~/.config/emacs/lisp/consult-flycheck")

;; (use-package unicode-tables
;;   :load-path "~/.config/emacs/lisp/unicode-tables")

;; (use-package kitty-graphics
;;   :load-path "~/.config/emacs/lisp/kiyty-graphics")

;; TODO option in `dired-incremental-narrow' to stop
;; filtering if there are no more directories

(use-package dired-poke
  :load-path "~/.config/emacs/lisp/dired-poke"
  :config
  (setq dired-poke-alist
        `((?D . ,(expand-file-name "~/Desktop"))
          (?d . ,(expand-file-name "~/Downloads"))
          (?v . ,(expand-file-name "~/Videos"))
          (?p . ,(expand-file-name "~/Pictures"))
          (?m . ,(expand-file-name "~/Music"))
          (?c . ,(expand-file-name "~/Documents"))
          (?h . ,(expand-file-name "~/"))
          (?e . ,(expand-file-name "~/.config/emacs"))
          (?o . ,(expand-file-name "~/org"))
          )))

(use-package ginit
  :load-path "~/.config/emacs/lisp/ginit")

(use-package org-comments
  :load-path "~/.config/emacs/lisp/org-comments"
  :config (define-key prog-mode-map (kbd "C-c C-c") #'org-comments-dwim))

(use-package qbe-mode
  :load-path "~/.config/emacs/lisp/qbe-mode")

(use-package playwright
  :load-path "~/.config/emacs/lisp/playwright")

(use-package ctrl-quit
  :load-path "~/.config/emacs/lisp/ctrl-quit")

;; (use-package press-shift
;;   :load-path "~/.config/emacs/lisp/press-shift")

(use-package unmapped-keys
  :load-path "~/.config/emacs/lisp/unmapped-keys")

(use-package crystal-point
  :load-path "~/.config/emacs/lisp/crystal-point"
  :hook (after-init . crystal-point-enable))

(use-package hydra
  :ensure t)
;; v Depend 
(use-package org-block-icons
  :load-path "~/.config/emacs/lisp/org-block-icons"
  :hook (org-mode . org-block-icons-mode))

(use-package dired-image-preview
  :load-path "~/.config/emacs/lisp/dired-image-preview"
  :custom
  (dired-image-preview-scale 1.0)                    ; Can only make Smaller
  (dired-image-preview-delay 0.0)                    ; Faster preview in auto mode
  (dired-image-preview-spacing 2)                    ; More space around previews
  (dired-image-preview-auto-remove t)                ; Remove other previews
  (dired-image-preview-max-width 800)                ; Limit preview width
  (dired-image-preview-max-height 600)               ; Limit preview height
  (dired-image-preview-auto-mode t)                  ; Enable auto preview
  (dired-image-preview-excluded-extensions '("ico")) ; Skip ico files
  )


