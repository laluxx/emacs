(defvar pulse-cursor nil
  "Determine if the cursor should pulse.")


(add-to-list 'load-path "~/.config/emacs/scripts/")
(require 'elpaca-setup)    ;; The Elpaca Package Manager
;; (require 'app-launchers)   ;; Use emacs as a run launcher like dmenu (experimental)
(if pulse-cursor
(require 'pulsing-cursor)) ;; Pulse a cursor

;;this stuff should happen early TODO
;;; Reasonable defaults for interactive sessions

;;; Runtime optimizations
;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; PERF: Disable bidirectional text scanning for a modest performance boost.
;;   I've set this to `nil' in the past, but the `bidi-display-reordering's docs
;;   say that is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; PERF: Disabling BPA makes redisplay faster, but might produce incorrect
;;   reordering of bidirectional text with embedded parentheses (and other
;;   bracket characters whose 'paired-bracket' Unicode property is non-nil).
(setq bidi-inhibit-bpa t)  ; Emacs 27+ only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
;; (setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)


;; PGTK builds only: this timeout adds latency to frame operations, like
;; `make-frame-invisible', which are frequently called without a guard because
;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This could further increased where needed (like LSP TODO).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
;; (setq redisplay-skip-fontification-on-input t)

;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; A simple frame title
(setq frame-title-format '("%b – imaks")
      icon-title-format frame-title-format)

;; Try to keep the cursor out of the read-only portions of the minibuffer.
;; (setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(defun create-missing-directories ()
  "Automatically create missing directories when creating new files."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-remote-p buffer-file-name))
               (not (file-directory-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                                 parent-directory)))
      (make-directory parent-directory 'parents)
      t)))

(add-hook 'find-file-not-found-functions 'create-missing-directories)




;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (expand-file-name "backup/" "~/.config/emacs/")))
      tramp-backup-directory-alist backup-directory-alist)

(defun shut-up-autosave-a (fn &rest args)
  "If a file has autosaved data, `after-find-file' will pause for 1 second to
tell you about it. Very annoying. This prevents that."
  (cl-letf (((symbol-function 'sit-for) #'ignore))
    (apply fn args)))

(advice-add 'after-find-file :around #'shut-up-autosave-a)

;;
;;; Formatting

;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like go-mode).
(setq-default indent-tabs-mode nil
              tab-width 4)

;; An archaic default in the age of widescreen 4k displays? I disagree (same here).
;; We stillfrequently split our terminals and editor frames, or have them side-by-side,
;; using up more of that newly available horizontal real-estate.
(setq-default fill-column 80)

;; This was a widespread practice in the days of typewriters. I actually prefer
;; it when writing prose with monospace fonts, but it is obsolete otherwise.
(setq sentence-end-double-space nil)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant (and no big deal if not).
(setq require-final-newline t)

;; Default to soft line-wrapping in text modes. It is more sensibile for text
;; modes, even if hard wrapping is more performant.
(add-hook 'text-mode-hook #'visual-line-mode)

;;
;;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop' or `helm-show-kill-ring'.
(setq kill-do-not-save-duplicates t)

;;
;;; Extra file extensions to support

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))


(global-auto-revert-mode t) ;; Automatically show changes if the file has changed

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package all-the-icons-ibuffer
  :after all-the-icons
  :hook (ibuffer-mode . (lambda () (all-the-icons-ibuffer-mode 1))))

;; (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package diminish)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

;; (use-package peep-dired
;;   :after dired
;;   :hook (evil-normalize-keymaps . peep-dired-hook)
;;   :config
;;     (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
;;     (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;;     (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
;;     (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
;; )

(with-eval-after-load 'evil
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file))

(add-hook 'dired-mode-hook (lambda () (display-line-numbers-mode 0)))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo)
  (evil-mode))

;; Set the cursor to a solid block for both normal and insert modes
(setq evil-normal-state-cursor '(box))
(setq evil-insert-state-cursor '(box))




(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))


;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(keyboard-translate ?\C-i ?\H-i)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-8") 'swiper-isearch-thing-at-point)
  (define-key evil-normal-state-map (kbd "g r") 'deadgrep)
  (define-key evil-normal-state-map (kbd "DEL") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "H-i") 'laluxx/iedit-insert)
  ;; (define-key evil-normal-state-map (kbd "C-i") 'laluxx/iedit-insert)
)

(with-eval-after-load 'evil
  (define-key evil-insert-state-map [escape] 'laluxx/escape-and-quit-iedit)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-l") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-v") 'yank)
  (define-key evil-insert-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-c") 'kill-ring-save)
  (define-key evil-insert-state-map (kbd "C-x") 'kill-region)
  (define-key evil-insert-state-map (kbd "C-z") 'undo)
  (define-key evil-insert-state-map (kbd "C-y") 'undo-redo))

(with-eval-after-load 'evil
  (define-key evil-visual-state-map (kbd "|") 'shell-command-on-region))

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(use-package magit)

;; thanks DOOM
(defun +ivy-format-function-line-or-arrow (cands)
  "Transform CANDS into a string for minibuffer.
Uses an arrow in terminal and standard formatting in a GUI."
  (if (display-graphic-p)
      (ivy-format-function-line cands)  ; GUI Emacs
    (ivy--format-function-generic
     (lambda (str)
       (ivy--add-face (concat " " str "\n") 'ivy-current-match))  ; Selected candidate
     (lambda (str)
       (concat "  " str "\n"))  ; Other candidates
     cands
     "")))



(use-package ivy
  :ensure t
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (enable-recursive-minibuffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1) ;; TODO don't use ivy on keychords like (C-x C-f)
  (setq ivy-format-functions-alist '((t . +ivy-format-function-line-or-arrow)))

  (setq ivy-sort-functions-alist
        '((t . ivy--prefix-sort-recentf))) ;; prioritize recent items
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "<up>") 'ivy-previous-line-or-history)
  (define-key ivy-minibuffer-map (kbd "<down>") 'ivy-next-line-or-history))



(use-package counsel
  :ensure t
  ;; :after ivy
  :config 
  (counsel-mode 1)

  ;; Integrate `helpful` with `counsel`
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)


  (define-key counsel-mode-map [remap find-file] nil)
  (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package ivy-rich
  :ensure t
  ;; :after ivy
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))




(defun laluxx/ivy-rich-header-icon (_candidate)
  "Force the icon to always represent a .h file."
  (all-the-icons-icon-for-file "dummy.h"))

(defun laluxx/setup-ivy-rich-header-icon ()
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'laluxx/find-header
                   '(:columns
                     ((laluxx/ivy-rich-header-icon :width 2)
                      (identity (:width 0.3 :face default)))))))

(with-eval-after-load 'ivy-rich
  (laluxx/setup-ivy-rich-header-icon))

(defun laluxx/find-header ()
  "Search for headers and open in a new window."
  (interactive)
  (let* ((cmd "rg --files /usr/include --follow --hidden -g \"*.h\"")
         (headers (split-string (shell-command-to-string cmd) "\n" t)))
    (ivy-read "Choose header: " headers
              :action (lambda (x) (find-file-other-window x))
              :caller 'laluxx/find-header)))

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

(use-package projectile
  :config
  (projectile-mode 1))

(delete-selection-mode 1)
(electric-indent-mode -1)
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
;; The following prevents <> from auto-pairing when electric-pair-mode is on.
;; Otherwise, org-tempo is broken when you try to <s TAB...
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(global-display-line-numbers-mode 1)
;; (global-visual-line-mode t)  ;; Enable visual lines
(setq-default truncate-lines t) ;; Enable truncated lines
;; (menu-bar-mode -1)           ;; Disable the menu bar 
;; (scroll-bar-mode -1)         ;; Disable the scroll bar
;; (tool-bar-mode -1)           ;; Disable the tool bar
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.
(setq completing-read-function 'ivy-completing-read)
(setq use-dialog-box nil)
(setq use-short-answers t)
(global-set-key [escape] 'keyboard-escape-quit)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

  (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))

  ;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
  ;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
  ;; eshell-aliases-file -- sets an aliases file for the eshell.

  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm-toggle
  :after vterm
  :config
  ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
  ;; kills the program as it would in most standard terminal programs.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.4))))

(use-package sudo-edit)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)   ; Replace `describe-function`
   ("C-h v" . helpful-variable)   ; Replace `describe-variable`
   ("C-h k" . helpful-key)        ; Replace `describe-key`
   ("C-h C" . helpful-command)    ; Additional command for describing commands
   ("C-h F" . helpful-function))  ; Additional command for describing functions
  :custom
  (helpful-max-buffers 10 "Limit the number of helpful buffers to avoid clutter")
  :config
  (setq helpful-switch-buffer-function 'pop-to-buffer)
  ;; Hook for customizing helpful-mode
  :hook (helpful-mode . (lambda ()
                          ;; Disable line numbers
                          (display-line-numbers-mode -1)
                          ;; Enable olivetti-mode
                          (olivetti-mode 1)
                          ;; Set olivetti width
                          (olivetti-set-width 70))))

(use-package iedit
  :ensure t)

(use-package olivetti
  :config
  ;; text width
  (setq olivetti-body-width 80)
)

(add-to-list 'display-buffer-alist
             '("\\*WoMan.*\\*" . (display-buffer-pop-up-window)))

(add-hook 'woman-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (olivetti-mode 1)))

(use-package deadgrep
  :ensure t
  :config
  (setq deadgrep--search-type 'regexp)  ;; Default search type to regular expressions

  (custom-set-faces
   '(deadgrep-filename-face ((t (:inherit org-level-1))))
   '(deadgrep-match-face ((t (:inherit font-lock-constant-face)))))

(add-hook 'deadgrep-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)
            ;; Set keybindings when evil-mode is active
            (when (bound-and-true-p evil-mode)
              (evil-define-key 'normal deadgrep-mode-map (kbd "j") 'deadgrep-forward)
              (evil-define-key 'normal deadgrep-mode-map (kbd "k") 'deadgrep-backward)
              (evil-define-key 'normal deadgrep-mode-map (kbd "C-j") 'deadgrep-forward-match)
              (evil-define-key 'normal deadgrep-mode-map (kbd "C-k") 'deadgrep-backward-match)
              (evil-define-key 'normal deadgrep-mode-map (kbd "n") 'deadgrep-forward-filename)
              (evil-define-key 'normal deadgrep-mode-map (kbd "N") 'deadgrep-backward-filename))))
)

(use-package vterm
:config
(setq shell-file-name "/bin/sh"
      vterm-max-scrollback 5000)
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))
)

;; Set a very high garbage collection threshold to reduce frequency of garbage collection
(setq gc-cons-threshold (* 500 1024 1024))  ; 500MB

;; Adjust gc-cons-percentage to a higher value
(setq gc-cons-percentage 0.7)

;; Optional: Use gcmh package for more dynamic management
(use-package gcmh
  :ensure t
  :config
  (setq gcmh-high-cons-threshold (* 500 1024 1024))  ; 500MB
  (gcmh-mode 1))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1)
)

(use-package vundo
  :if (> emacs-major-version 27)
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)

  ;; Customize vundo-mode
  (add-hook 'vundo-mode-hook
            (lambda ()
              (display-line-numbers-mode -1))))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil)
  ;; Additional undo-tree configurations can go here
)

(use-package amx
  :ensure t
  :after ivy
  :custom
  (amx-backend 'auto)
  (amx-save-file "~/.config/emacs/amx-items")
  (amx-history-length 50)
  (amx-show-key-bindings nil)
  :config
  (amx-mode 1))

(define-key global-map [remap dired] 'counsel-dired)
(global-set-key [remap describe-variable] 'counsel-describe-variable)
(global-set-key [remap describe-function] 'counsel-describe-function)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     t 
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; (setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs
;;     projectile hydra flycheck company avy dap-mode))

;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))

;; ;; Helm configuration removed

;; ;; (which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   ;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-cpptools)
;;   (yas-global-mode))

(use-package lsp-mode
  :hook ((c-mode . lsp)
         (c++-mode . lsp))
  :config
  (setq lsp-idle-delay 0.1)  ;; clangd is fast

  ;; Disable the LSP headerline (breadcrumb)
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;; (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "~/.config/emacs/images/dtmacs-logo.png")
  (setq dashboard-startup-banner "~/xos/emacs/dashboard/xos-logo.png") ;; logo
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook)

  ;; disable solaire-mode
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (solaire-mode -1)))
  
  ;; Ensure dashboard is in evil normal mode
  (add-hook 'dashboard-mode-hook 'evil-normal-state)

  (evil-define-key 'normal dashboard-mode-map (kbd "j") 'widget-forward)
  (evil-define-key 'normal dashboard-mode-map (kbd "k") 'widget-backward)
  (evil-define-key 'normal dashboard-mode-map (kbd "h") 'widget-backward)
  (evil-define-key 'normal dashboard-mode-map (kbd "l") 'dashboard-return))

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

(defun laluxx/hide-which-key-mode-line-and-line-numbers (&rest _)
  "Hide the mode line and line numbers in the `which-key' popup window."
  (let* ((buf (get-buffer which-key-buffer-name))
         (window (and buf (get-buffer-window buf))))
    (when window
      (with-selected-window window
        (setq mode-line-format nil)
        (display-line-numbers-mode -1)))))

(advice-add 'which-key--show-popup :after 'laluxx/hide-which-key-mode-line-and-line-numbers)

(use-package edwina
  :ensure f
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (define-key edwina-mode-map (kbd "M-q") 'delete-window))


;; (defun edwina-toggle-mode-based-on-window-count ()
;;   "Toggle edwina-mode based on window count.
;; Enable if there are two or more windows, disable if there's only one.
;; However, don't toggle if which-key is currently displayed."
;;   (let ((which-key-buffer (get-buffer which-key-buffer-name)))
;;     (if (and which-key-buffer (get-buffer-window which-key-buffer))
;;         nil ;; Do nothing if which-key is displayed
;;       (if (= 1 (count-windows))
;;           (when edwina-mode (edwina-mode -1))
;;         (unless edwina-mode (edwina-mode 1))))))

(defun edwina-toggle-mode-based-on-window-count ()
  "Toggle edwina-mode based on window count.
Enable if there are two or more windows, disable if there's only one.
However, don't toggle if which-key is currently displayed."
  (when (and (featurep 'edwina)  ;; Check if edwina is loaded
             (boundp 'which-key-buffer-name))  ;; Check if which-key-buffer-name is defined
    (let ((which-key-buffer (get-buffer " *which-key*")))  ;; Access the which-key buffer directly
      (if (and which-key-buffer (get-buffer-window which-key-buffer))
          nil ;; Do nothing if which-key is displayed
        (if (= 1 (count-windows))
            (when edwina-mode (edwina-mode -1))
          (unless edwina-mode (edwina-mode 1)))))))

(add-hook 'window-configuration-change-hook 'edwina-toggle-mode-based-on-window-count)

(use-package toc-org ;; Table of contents
    :commands toc-org-enable
    ;; :init (add-hook 'org-mode-hook 'toc-org-enable)
)

;; Remove "Ind" from showing in the modeline.
(eval-after-load 'org-indent '(diminish 'org-indent-mode))
(setq org-confirm-babel-evaluate nil) ;; Dont Bother  


(add-hook 'org-mode-hook
          (lambda ()
            (toc-org-enable)  ;; Enable Table of Contents
            (display-line-numbers-mode -1)  ;; Disable line numbers
            (evil-define-key 'normal org-mode-map (kbd "RET") 'org-ctrl-c-ctrl-c)))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   ;; other languages if needed
   ))

(require 'org-tempo)

(defun my-org-cycle-or-move-right ()
  (interactive)
  (if (and (bolp) (org-at-heading-p))
      (org-cycle)
    (evil-forward-char 1)))

(defun my-org-close-or-move-left ()
  (interactive)
  (if (and (bolp) (org-at-heading-p))
      (outline-hide-subtree)
    (evil-backward-char 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (evil-define-key 'normal org-mode-map
              (kbd "l") 'my-org-cycle-or-move-right)
            (evil-define-key 'normal org-mode-map
              (kbd "h") 'my-org-close-or-move-left)))

;; (use-package tree-sitter-langs
;;   :ensure t)


;; (use-package tree-sitter
;;   :config
;;   (require 'tree-sitter-langs)
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

(use-package ruby-electric
  :ensure t
  :hook (ruby-mode . ruby-electric-mode))

(use-package robe
  :ensure t
  :defer t
  :hook (ruby-mode . robe-mode)
  :init
  ;; Optional: Keybindings for robe mode
  (eval-after-load 'robe
    '(progn
       (define-key robe-mode-map (kbd "C-c C-d") 'robe-doc)
       (define-key robe-mode-map (kbd "C-c C-j") 'robe-jump)
       (define-key robe-mode-map (kbd "C-c C-r") 'robe-rails-refresh)))
  :config
  ;; Start robe server automatically
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
)

(blink-cursor-mode -1)
(defvar laluxx/original-cursor-color nil
  "Variable to store the original cursor color.")

(defun laluxx/update-cursor-colors ()
  "Update cursor colors based on the current theme."
  (setq laluxx/original-cursor-color (face-background 'cursor)) ; Save the original cursor color
  (put 'cursor 'laluxx/evil-emacs-color (face-foreground 'warning)))

(defun laluxx/emacs-mode-cursor-color ()
  "Set cursor color for Emacs mode."
  (unless laluxx/original-cursor-color
    (setq laluxx/original-cursor-color (face-background 'cursor))) ; Save the original color if not already saved
  (evil-set-cursor-color (get 'cursor 'laluxx/evil-emacs-color)))

(defun laluxx/reset-cursor-color ()
  "Restore the original cursor color."
  (when laluxx/original-cursor-color
    (evil-set-cursor-color laluxx/original-cursor-color)))

;; Hooks for entering and exiting Emacs mode
(add-hook 'evil-emacs-state-entry-hook 'laluxx/emacs-mode-cursor-color)
(add-hook 'evil-emacs-state-exit-hook 'laluxx/reset-cursor-color)

;; Hook to update cursor colors after theme load
(add-hook 'after-load-theme-hook 'laluxx/update-cursor-colors)

;; Update cursor colors immediately in case a theme is already loaded
(laluxx/update-cursor-colors)






(defvar normal-mode-cursor-color nil "cursor color for normal mode.")

(defun save-default-cursor-color ()
  (setq normal-mode-cursor-color (face-background 'cursor)))

(defun set-evil-insert-cursor-color ()
  (let ((color (face-foreground 'font-lock-constant-face)))
    (setq evil-insert-state-cursor `(box ,color))))

;; Function to restore the cursor color for normal mode
(defun restore-normal-mode-cursor-color ()
  (when normal-mode-cursor-color
    (setq evil-normal-state-cursor `(box ,normal-mode-cursor-color))
    ;; HACK Force a refresh of the cursor in case it's not updated correctly the first time
    (unless (equal (face-background 'cursor) normal-mode-cursor-color)
      (set-cursor-color normal-mode-cursor-color))))


(add-hook 'evil-normal-state-entry-hook 'restore-normal-mode-cursor-color)
(add-hook 'emacs-startup-hook (lambda ()
                                (save-default-cursor-color)
                                (set-evil-insert-cursor-color)))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don"t accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 23) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(setq double-buffering t)

(use-package ef-themes
  :ensure t)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Sets the default theme to load!!! 
  ;; (load-theme 'doom-one t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;(require 'theme-magic)
;;(theme-magic-export-theme-mode)

(use-package theme-magic
  :ensure t)
  ;;:config
  ;; Enable theme-magic for supported terminals
  ;; (theme-magic-export-theme-mode t))

(use-package ewal
  :ensure t
  :init
  (setq ewal-use-built-in-always-p nil
        ewal-use-built-in-on-failure-p t
        ewal-built-in-palette "sexy-material"))

;; ** Ewal-Doom-Theme Configuration
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

;; (add-to-list 'default-frame-alist '(alpha-background . 85)) ; For hardcoded alpha

(defun set-theme-transparency (&rest args)
  "Set the frame transparency based on the theme loaded."
  (let* ((transparent-themes '(ewal-doom-one ewal-doom-vibrant))
         (current-theme (car args)) ; the first argument to `load-theme` is the theme name
         (alpha-value (if (member current-theme transparent-themes) 85 100)))
    ;; Remove old setting
    (setq default-frame-alist (assq-delete-all 'alpha-background default-frame-alist))
    ;; Apply new setting
    (add-to-list 'default-frame-alist `(alpha-background . ,alpha-value))
    ;; Update current frames
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'alpha-background alpha-value))))

(advice-add 'load-theme :after 'set-theme-transparency)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets left bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package general
  :config
  (general-evil-setup)
  
  ;; set up 'SPC' as the global leader key
  (general-create-definer laluxx/leader-keys
    :states '(normal ;; insert
		     visual emacs)
    :keymaps 'override
    :prefix "SPC") ;; set leader
    ;; :global-prefix "M-SPC") ;; access leader in insert mode

  (laluxx/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(counsel-find-file :wk "Find file")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

  (laluxx/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(counsel-switch-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(laluxx/kill-current-buffer-and-window :wk "Kill buffer and window")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  (laluxx/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d J" '(laluxx/dired-split-jump :wk "Dired split jump ")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d i" '(laluxx/diff-buffer-with-file :wk "Diff buffer with file")
    "d p" '(peep-dired :wk "Peep-dired"))

  (laluxx/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (laluxx/leader-keys
    "f" '(:ignore t :wk "Files")    
    "f c" '((lambda () (interactive)
              (find-file "~/.config/emacs/config.org")) 
            :wk "Open emacs config.org")
    "f e" '((lambda () (interactive)
              (dired "~/.config/emacs/")) 
            :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    ;; "f i" '((lambda () (interactive)
    ;;           (find-file "~/.config/emacs/init.el")) 
    ;;         :wk "Open emacs init.el")
    "f i" '(swiper :wk "Swiper")
    "f j" '(laluxx/file-jump :wk "Jump to a file")
    ;; "f l" '(counsel-locate :wk "Locate a file")
    "f l" '(find-library :wk "Locate a file")
    "f r" '(counsel-recentf :wk "File recent")
    "f R" '(laluxx/counsel-recentf-split :wk "File recent split")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f f" '(counsel-find-file :wk "Find file")
    "f F" '(laluxx/file-jump :wk "Find file split")
    "f h" '(laluxx/find-header :wk "Find header")
    "f t" '(laluxx/find-TODOs :wk "Find TODOs")
    "f n" '(laluxx/find-NOTES :wk "Find NOTES")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (laluxx/leader-keys
    "q" '(:ignore t :wk "Quit")
    "q r" '(restart-emacs :wk "Restart emacs"))

  (laluxx/leader-keys
    "i" '(:ignore t :wk "Insert")
    "i i" '(all-the-icons-insert-faicon :wk "Insert FontAwesome")
    "i f" '(all-the-icons-insert-fileicon :wk "Insert file icon") ;
    "i g" '(all-the-icons-insert-octicon :wk "Insert github icon"))

  (laluxx/leader-keys
    "k" '(:ignore t :wk "Kill")
    "k p" '(kill-process :wk "Kill process"))

  (laluxx/leader-keys
    "g" '(:ignore t :wk "Git")    
    "g /" '(magit-displatch :wk "Magit dispatch")
    "g ." '(magit-file-displatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create") 
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g f" '(:ignore t :wk "Find") 
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))

  (laluxx/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function") 
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h p" '(helpful-at-point :wk "Helpful at point")
    "h k" '(helpful-key :wk "Describe key")
    "h K" '(counsel-descbinds :wk "Describe keybinds")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    ;; "h s" '(helm-lsp-workspace-symbol :wk "Symbol")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
                (load-file "~/.config/emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config")
    "h t" '(laluxx/load-dark-theme :wk "Load theme")
    "h T" '(laluxx/wal-set :wk "Wal set")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(woman :wk "Woman")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (laluxx/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (laluxx/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (laluxx/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (laluxx/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o e" '(elfeed :wk "Elfeed RSS")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings 
  ;; set for us, so no need to specify each individually.
  (laluxx/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

  (laluxx/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s m" '(man :wk "Man pages")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s i" '(counsel-imenu :wk "Counsel imenu")
    "s u" '(vundo :wk "Visual undo")
    "s w" '(woman :wk "Similar to man but doesn't require man"))

  (laluxx/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t m" '(laluxx/toggle-modeline :wk "Toggle modeline")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t o" '(org-mode :wk "Toggle org mode") ;; TODO Toggle opacity
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(toggle-truncate-lines :wk "Toggle truncated lines")
    "t h" '(laluxx/toggle-hl-line-mode :wk "Toggle hl-line-mode")
    "v" '(vterm-toggle :wk "Toggle vterm"))

  (laluxx/leader-keys
    "w" '(:ignore t :wk "Windows")
    "w i" '(where-is :wk "Where is ?")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))
  )

(defvar my-modeline-mode-line-format
  '(:eval (let* ((evil-state (cond ((evil-normal-state-p) '("NORMAL" . "green"))
                                   ((evil-insert-state-p) '("INSERT" . "blue"))
                                   ((evil-visual-state-p) '("VISUAL" . "orange"))
                                   ((evil-replace-state-p) '("REPLACE" . "red"))
                                   ((evil-emacs-state-p) '("EMACS" . "purple"))
                                   (t '("UNKNOWN" . "grey"))))
            (evil-state-name (car evil-state))
            (evil-state-color (cdr evil-state)))
            (concat (propertize (format " %s " evil-state-name) 'face `(:background ,evil-state-color))
                    " %b [%m] Line %l")))
  "Custom modeline format with EVIL mode indicator.")

(defun my-modeline-refresh ()
  "Refresh the modeline in all buffers."
  (setq-default mode-line-format (if my-modeline-mode my-modeline-mode-line-format))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (force-mode-line-update))))

(define-minor-mode my-modeline-mode
  "A minor mode for a custom modeline."
  :init-value nil
  :global true
  :lighter " My-Modeline"
  :group 'my-custom-modeline
  (my-modeline-refresh))

(provide 'my-modeline-mode)

(defun laluxx/kill-current-buffer-and-window ()
  "Kill the current buffer and close the window if there are other windows in the frame."
  (interactive)
  (let ((current-window (selected-window))
        (buffer-to-kill (current-buffer)))
    ;; Kill the current buffer
    (kill-buffer buffer-to-kill)
    ;; Close the window if there are other windows available
    (unless (one-window-p)
      (delete-window current-window))))

(defun laluxx/toggle-modeline ()
  "Toggle the modeline on and off."
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (default-value 'mode-line-format)
          nil))
  (force-mode-line-update)
  ;; If you're using a window system, refresh the frame as well
  (when (display-graphic-p)
    (redraw-frame (selected-frame))))

(defun laluxx/find-TODOs ()
  "Search for TODOs using deadgrep."
  (interactive)
  (deadgrep "TODO"))

(defun laluxx/find-NOTES ()
  "Search for NOTE using deadgrep."
  (interactive)
  (deadgrep "NOTE"))

(defun laluxx/iedit-insert()
  "Activate iedit-mode, switch to insert mode and go to the end of the current word."
  (interactive)
  (iedit-mode)
  (evil-insert-state)
  (forward-word))

(defun laluxx/escape-and-quit-iedit ()
  "Switch to normal state, then quit iedit-mode if active."
  (interactive)
  (evil-force-normal-state)
  (when (bound-and-true-p iedit-mode)
    (iedit--quit)))

;; TODO
(defun laluxx/counsel-fonts ()
  "Display a list of fonts, with each font name styled in its own font."
  (interactive)
  (let ((fonts (font-family-list))
        (formatted-fonts '()))
    (dolist (font fonts)
      (push (propertize font 'face `(:family ,font)) formatted-fonts))
    (ivy-read "Choose a font: " (nreverse formatted-fonts)
              :action (lambda (selection)
                        (message "You selected: %s" selection)))))






(defun laluxx/counsel-recentf-split ()
  "Open a recent file in a new window to the right."
  (interactive)
  (ivy-read "Recentf: " recentf-list
            :action (lambda (f)
                      (with-selected-window (split-window-right)
                        (find-file f)))
            :caller 'counsel-recentf))

(defun laluxx/dired-split-jump ()
  "Split the window vertically and open dired in the new window."
  (interactive)
  (split-window-right)       ;; Split the window vertically
  (other-window 1)           ;; Move to the new window
  (dired nil))               ;; Open dired

(defun laluxx/toggle-hl-line-mode ()
  "Toggle highlighting of the current line."
  (interactive)
  (if hl-line-mode
      (hl-line-mode -1)
    (hl-line-mode 1)))

(defun laluxx/cycle-line-numbers ()
  "Cycle between line number modes: absolute, relative, none."
  (interactive)
  (cond
   ;; If line numbers are currently displayed
   ((eq display-line-numbers t)
    (setq display-line-numbers 'relative)
    (message "Relative line numbers enabled"))

   ;; If relative line numbers are currently displayed
   ((eq display-line-numbers 'relative)
    (setq display-line-numbers nil)
    (message "Line numbers disabled"))

   ;; If no line numbers are currently displayed
   (t
    (setq display-line-numbers t)
    (message "Absolute line numbers enabled"))))

(defun laluxx/mark-word (&optional arg allow-extend)
  "Mark the whole word at point. 
This function is a modified version of the built-in `mark-word'."
  (interactive "P\np")
  (let ((x "forward-word"))
    (cond ((and allow-extend
               (or (and (eq last-command this-command) (mark t))
                   (region-active-p)))
          (setq arg (if arg (prefix-numeric-value arg)
                      (if (< (mark) (point)) -1 1)))
          (set-mark
           (save-excursion
             (goto-char (mark))
             (funcall (intern x) arg)
             (point))))
         (t
          (let ((bounds (bounds-of-thing-at-point 'word)))
            (unless (consp bounds)
              (user-error "No word at point"))
            (if (>= (prefix-numeric-value arg) 0)
                (goto-char (car bounds))
              (goto-char (cdr bounds)))
            (push-mark
             (save-excursion
               (funcall (intern x) (prefix-numeric-value arg))
               (point)))
            (activate-mark))))))

(defun laluxx/mark-word-backward (&optional arg allow-extend)
  "Mark the whole word backward from point. 
This function is a modified version of `laluxx/mark-word' but moves backward."
  (interactive "P\np")
  (laluxx/mark-word (- (or arg 1)) allow-extend))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global "w" 'laluxx/mark-word)
  (evil-define-key 'normal 'global "W" 'laluxx/mark-word-backward))

(defvar laluxx/window-configuration nil
  "Current window configuration.
Intended for use by `laluxx/window-monocle'.")

(define-minor-mode laluxx/window-single-toggle
  "Toggle between multiple windows and single window.
This is the equivalent of maximizing a window. Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  :lighter " [M]"
  :global nil
  (if (one-window-p)
      (when laluxx/window-configuration
        (set-window-configuration laluxx/window-configuration))
    (setq laluxx/window-configuration (current-window-configuration))
    (delete-other-windows)))

(add-hook 'after-init-hook (lambda () (global-set-key (kbd "M-SPC") 'laluxx/window-single-toggle)))

(defun laluxx/org-move-to-begin-src ()
  "Move cursor to the line below #+begin_src."
  (interactive)
  (let ((original-pos (point)))
    (search-backward "#+begin_src")
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

(defun laluxx/setup-org-evil-bindings ()
  (evil-define-key 'visual org-mode-map (kbd "C-k") 'laluxx/org-move-to-begin-src)
  (evil-define-key 'visual org-mode-map (kbd "C-j") 'laluxx/org-move-to-end-src)
  (evil-define-key 'visual-line org-mode-map (kbd "C-k") 'laluxx/org-move-to-begin-src)
  (evil-define-key 'visual-line org-mode-map (kbd "C-j") 'laluxx/org-move-to-end-src)
  (evil-define-key 'block org-mode-map (kbd "C-k") 'laluxx/org-move-to-begin-src)
  (evil-define-key 'block org-mode-map (kbd "C-j") 'laluxx/org-move-to-end-src))

(add-hook 'org-mode-hook 'laluxx/setup-org-evil-bindings)

(defvar laluxx/light-themes
  '(doom-one-light doom-tomorrow-day doom-flatwhite doom-homage-white doom-plain whiteboard tsdh-light tango modus-operandi
		   leuven adwaita dichromacy  doom-bluloco-light doom-acario-light doom-ayu-light doom-feather-light doom-gruvbox-light
		   doom-nord-light doom-oksolar-light doom-opera-light doom-solarized-light doom-earl-grey ef-cyprus ef-day ef-deuteranopia-light
		   ef-duo-light ef-elea-light ef-frost ef-kassio ef-light ef-maris-light ef-melissa-light ef-spring ef-summer ef-trio-light ef-tritanopia-light)
  "List of light themes.")

(defvar laluxx/ugly-themes
  '(wombat wheatgrass tsdh-dark tango-dark misterioso leuven-dark
	   deeper-blue doom-acario-dark doom-homage-black doom-ir-black doom-meltbus doom-oksolar-dark)
  "List of themes that are considered ugly.")

(defvar laluxx/current-theme nil
  "The current theme used in the Emacs session.")

(defun laluxx/save-current-theme ()
  "Save the current theme name and its background color as a string to a file."
  (when laluxx/current-theme
    (let ((background-color (face-attribute 'default :background)))
      (with-temp-file "~/.config/emacs/static-variables"
        (insert (format "%s\n%s" (symbol-name laluxx/current-theme) background-color))))))



(defun laluxx/load-theme-generic (theme-list prompt)
  "Load a theme from THEME-LIST, with preview. Revert to original theme if canceled."
  (let ((original-theme (car custom-enabled-themes))
        selected-theme)
    (ivy-read prompt (mapcar 'symbol-name theme-list)
              :preselect (symbol-name original-theme)
              :update-fn (lambda ()
                           (let ((current-selection (intern (ivy-state-current ivy-last))))
                             (when (and current-selection
                                        (not (equal current-selection original-theme)))
                               (mapc #'disable-theme custom-enabled-themes)
                               (load-theme current-selection t)
			                   (if pulse-cursor
				   (update-pulsing-cursor-color))))) ;; Preview theme
              :action (lambda (theme)
                        (setq selected-theme (intern theme))
                        (when selected-theme
                          (setq laluxx/current-theme selected-theme) ;; Update current theme
                          (mapc #'disable-theme custom-enabled-themes)
                          (load-theme selected-theme t)
                          (laluxx/save-current-theme))
			            (if pulse-cursor
                            (update-pulsing-cursor-color))) ;; Save current theme
              :unwind (lambda ()
                        (unless selected-theme
                          (mapc #'disable-theme custom-enabled-themes)
                          (when original-theme
                            (load-theme original-theme t)
                            (laluxx/update-cursor-colors) ;; Update cursor colors
                            (setq laluxx/current-theme original-theme)
                            (laluxx/save-current-theme)
			                (if pulse-cursor
                                (update-pulsing-cursor-color)))))))) ;; Revert to original theme



(defun laluxx/load-saved-theme ()
  "Load the saved theme from the file."
  (when (file-exists-p "~/.config/emacs/static-variables")
    (with-temp-buffer
      (insert-file-contents "~/.config/emacs/static-variables")
      (goto-char (point-min)) ;; Move to the beginning of the buffer
      (let ((theme-name (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (setq laluxx/current-theme (intern theme-name))
        (load-theme laluxx/current-theme t)
        (laluxx/update-cursor-colors) ;; Update cursor colors
	    (if pulse-cursor
            (update-pulsing-cursor-color))))))



(with-eval-after-load 'doom-themes
  (with-eval-after-load 'ewal
    (laluxx/load-saved-theme)))



(defun laluxx/load-dark-theme ()
  "Load a dark theme, excluding light and ugly themes."
  (interactive)
  (laluxx/load-theme-generic (seq-difference (custom-available-themes) (append laluxx/light-themes laluxx/ugly-themes))
                             "Load dark theme: "))

(defun laluxx/load-light-theme ()
  "Load a light theme."
  (interactive)
  (laluxx/load-theme-generic laluxx/light-themes "Load light theme: "))

(defun laluxx/load-ugly-theme ()
  "Load an ugly theme."
  (interactive)
  (laluxx/load-theme-generic laluxx/ugly-themes "Load ugly theme: "))



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

;; (defun laluxx/find-header ()
;;   (interactive)
;;   (let* ((cmd "rg --files /usr/include --follow --hidden -g \"*.h\"")
;;          (headers (split-string (shell-command-to-string cmd) "\n" t)))
;;     (ivy-read "Choose header: " headers
;;               :action (lambda (x) (find-file-other-window x)))))

(defun laluxx/file-jump ()
  "Open a file in a new split. Vertical split if one window, horizontal otherwise."
  (interactive)
  (let ((current-buffer (current-buffer))
        (selected-file (counsel-find-file))
        (split-fn (if (= (length (window-list)) 1) 'split-window-right 'split-window-below)))
    (when selected-file
      (funcall split-fn)             ; Split based on number of windows
      (switch-to-buffer current-buffer) ; Switch back to the original buffer
      (other-window 1)              ; Move to the newly created window
      (find-file selected-file))))

(defun laluxx/wal-set ()
  (interactive)
  (let* ((default-directory "~/xos/wallpapers/static")
         (image-files (directory-files-recursively default-directory "\\.\\(png\\|jpg\\|jpeg\\|webp\\)$"))
         (wm-name (string-trim (shell-command-to-string "wmctrl -m | awk 'NR==1 {print $2}'"))))
    (ivy-read "Choose wallpaper: "
              image-files
              :action (lambda (wallpaper)
                        (when (and (not (string-empty-p wallpaper))
                                   (file-exists-p wallpaper))
                          (let ((abs-wallpaper (expand-file-name wallpaper)))
                            (shell-command-to-string (concat "wal -i " abs-wallpaper)))
                          (shell-command-to-string "dashboard-wal-gen")
                          (cond
                           ((string-equal wm-name "LG3D") (shell-command-to-string "xmonad --restart"))
                           ((string-equal wm-name "dwm") (shell-command-to-string "xdotool key super+F5"))))))))

(defun laluxx/wal-set-animated ()
  "Set an animated wallpaper and configure theme based on it."
  (interactive)
  (let* ((default-directory "~/xos/wallpapers/animated")
         (theme-directory "~/xos/theme")
         (pywal-scripts-directory "~/xos/pywal-scripts")
         (video-files (directory-files-recursively default-directory "\\.\\(mp4\\|mkv\\|webm\\|avi\\)$"))
         (current-buffer-name (buffer-name)))
    (ivy-read "Choose animated wallpaper: "
              video-files
              :action (lambda (video)
                        (when (and (not (string-empty-p video))
                                   (file-exists-p video))
                          (let* ((abs-video (expand-file-name video))
                                 (first-frame-image (concat abs-video "-frame1.png")))
                            ;; Extract the first frame of the video
                            (shell-command (concat "ffmpeg -i " abs-video " -vframes 1 " first-frame-image))
                            ;; Use the extracted frame with `wal` to generate and set the theme (but not set as wallpaper)
                            (shell-command (concat "wal -n -i " first-frame-image))
                            ;; Rest of the commands similar to your wal-set function
                            (with-temp-file (concat theme-directory "/.wallpaper")
                              (insert first-frame-image))
                            (shell-command "theme pywal --no-random")
                            (dolist (script '("xmonad-dark-wal.py" "nvim-wal.py" "nvim-wal-dark.py"))
                              (shell-command (concat "python3 " pywal-scripts-directory "/" script)))
                            (shell-command "xmonad --restart")
                            (shell-command "dashboard-wal-gen")
                            (when (equal current-buffer-name "*dashboard*")
                              (run-at-time "0.5 sec" nil 'open-dashboard))
                            (run-at-time "1 sec" nil 'spaceline-compile)
                            ;; Finally, set the video as the animated wallpaper
                            (start-process "set-animated-wallpaper" nil "xwinwrap" "-o" "1.0" "-nf" "-ovr" "--"
                                           "mpv" "-wid" "WID" "--loop-file=inf" "--video-unscaled"
                                           "--no-terminal" "--no-audio" "--no-input-default-bindings"
                                           "--no-border" "--no-window-dragging" "--no-osc" "--no-osd-bar" abs-video)))))))



;; ORIGINAL Xmobar below
;; (defun laluxx/wal-set-animated ()
;;   (interactive)
;;   (let* ((default-directory "~/xos/wallpapers/animated")
;;          (theme-directory "~/xos/theme")
;;          (pywal-scripts-directory "~/xos/pywal-scripts")
;;          (video-files (directory-files-recursively default-directory "\\.\\(mp4\\|mkv\\|webm\\|avi\\)$"))
;;          (current-buffer-name (buffer-name)))
;;     (ivy-read "Choose animated wallpaper: "
;;               video-files
;;               :action (lambda (video)
;;                         (when (and (not (string-empty-p video))
;;                                    (file-exists-p video))
;;                           (let* ((abs-video (expand-file-name video))
;;                                  (first-frame-image (concat abs-video "-frame1.png")))
;;                             ;; Extract the first frame of the video
;;                             (shell-command-to-string (concat "ffmpeg -i " abs-video " -vframes 1 " first-frame-image))
;;                             ;; Use the extracted frame with `wal` to generate and set the theme (but not set as wallpaper)
;;                             (shell-command-to-string (concat "wal -n -i " first-frame-image))
;;                             ;; Rest of the commands similar to your wal-set function
;;                             (with-temp-file (concat theme-directory "/.wallpaper")
;;                               (insert first-frame-image))
;;                             (shell-command-to-string "theme pywal --no-random")
;;                             (dolist (script '("xmonad-dark-wal.py" "nvim-wal.py" "nvim-wal-dark.py"))
;;                               (shell-command-to-string (concat "python3 " pywal-scripts-directory "/" script)))
;;                             (shell-command-to-string "xmonad --restart")
;;                             (shell-command-to-string "dashboard-wal-gen")
;;                             (when (equal current-buffer-name "*dashboard*")
;;                               (run-at-time "0.5 sec" nil 'open-dashboard))
;;                             (run-at-time "1 sec" nil 'spaceline-compile)
;;                             ;; Finally, set the video as the animated wallpaper
;;                             (start-process "set-animated-wallpaper" nil "xwinwrap" "-o" "1.0" "-nf" "-ovr" "--"
;;                                            "mpv" "-wid" "WID" "--loop-file=inf" "--video-unscaled"
;;                                            "--no-terminal" "--no-audio" "--no-input-default-bindings"
;;                                            "--no-border" "--no-window-dragging" "--no-osc" "--no-osd-bar" abs-video)))))))

(defun laluxx/wal-set-favourite ()
  (interactive)
  (let* ((default-directory "~/xos/wallpapers/favourites")
         (theme-directory "~/xos/theme")
         (pywal-scripts-directory "~/xos/pywal-scripts")
         (image-files (directory-files-recursively default-directory "\\.\\(png\\|jpg\\|jpeg\\|webp\\)$")))
    (ivy-read "Favourite wallpapers: "
              image-files
              :action (lambda (wallpaper)
                        (when (and (not (string-empty-p wallpaper))
                                   (file-exists-p wallpaper))
                          (let ((abs-wallpaper (expand-file-name wallpaper)))
                            (shell-command-to-string (concat "wal -i " abs-wallpaper))
                            (with-temp-file (concat theme-directory "/.wallpaper")
                              (insert abs-wallpaper))
                            (shell-command-to-string "theme pywal --no-random")
                            (dolist (script '("xmonad-dark-wal.py" "nvim-wal.py" "nvim-wal-dark.py"))
                              (shell-command-to-string (concat "python3 " pywal-scripts-directory "/" script)))
                            (shell-command-to-string "xmonad --restart")
                            ;; (shell-command "papirus-wal")
                            ;; (shell-command-to-string "oomox-gtk-gen")
                            (shell-command-to-string "dashboard-wal-gen")
                            ;; (laluxx/load-org-wal-colors)
                            (run-at-time "1 sec" nil 'spaceline-compile)))))))  ; Delay spaceline-compile

(defun laluxx/wal-set-solid ()
  (interactive)
  (let* ((default-directory "~/xos/wallpapers/static")
         (theme-directory "~/xos/theme")
         (pywal-scripts-directory "~/xos/pywal-scripts")
         (solid-wallpapers-directory "~/xos/wallpapers/solid")
         (image-files (directory-files-recursively default-directory "\\.\\(png\\|jpg\\|jpeg\\|webp\\)$")))
    (ivy-read "Wallpapers to turn solid: "
              image-files
              :action (lambda (wallpaper)
                        (when (and (not (string-empty-p wallpaper))
                                   (file-exists-p wallpaper))
                          (let* ((abs-wallpaper (expand-file-name wallpaper))
                                 (base-wallpaper-name (file-name-base wallpaper))  ;; Get the filename without extension
                                 (solid-wallpaper (concat solid-wallpapers-directory "/" base-wallpaper-name "-SOLID.png")))
                            (if (file-exists-p solid-wallpaper)
                                (shell-command-to-string (concat "wal -i " solid-wallpaper))
                              (progn
                                (shell-command-to-string (concat "wal -n -i " abs-wallpaper)) ;; Use wal -n to generate colors without setting wallpaper
                                (shell-command-to-string (concat "wal-set-solid " base-wallpaper-name))))
                            (with-temp-file (concat theme-directory "/.wallpaper")
                              (insert abs-wallpaper))
                            (shell-command-to-string "theme pywal --no-random")
                            (dolist (script '("xmonad-dark-wal.py" "nvim-wal.py" "nvim-wal-dark.py"))
                              (shell-command-to-string (concat "python3 " pywal-scripts-directory "/" script)))
                            (shell-command-to-string "xmonad --restart")
                            (run-at-time "1 sec" nil 'spaceline-compile)))))))  ; Delay spaceline-compile

(defun laluxx/set-wallpaper ()
  (interactive)
  (let* ((default-directory "~/xos/wallpapers/static")
         (image-files (directory-files-recursively default-directory "\\.\\(png\\|jpg\\|jpeg\\|webp\\)$")))
    (ivy-read "Choose wallpaper: "
              image-files
              :action (lambda (wallpaper)
                        (when (and (not (string-empty-p wallpaper))
                                   (file-exists-p wallpaper))
                          (let ((abs-wallpaper (expand-file-name wallpaper)))
                            (shell-command-to-string (concat "feh --bg-scale " abs-wallpaper))))))))

;; work original
(defvar laluxx/last-animated-wallpaper nil "Path to the last set animated wallpaper.")

(defun laluxx/set-animated-wallpaper ()
  (interactive)
  (let* ((default-directory "~/xos/wallpapers/animated")
         (video-files (directory-files-recursively default-directory "\\.\\(mp4\\|mkv\\|webm\\|avi\\)$")))
    (ivy-read "Choose animated wallpaper: "
              video-files
              :action (lambda (video)
                        (when (and (not (string-empty-p video))
                                   (file-exists-p video))
                          (let ((abs-video (expand-file-name video)))
                            ;; Kill mpv if the new wallpaper is different from the last one
                            (when (not (equal abs-video laluxx/last-animated-wallpaper))
                              (shell-command "pkill mpv"))
                            (setq laluxx/last-animated-wallpaper abs-video)
                            (start-process "set-animated-wallpaper" nil "xwinwrap" "-o" "1.0" "-nf" "-ovr" "--"
                                           "mpv" "-wid" "WID" "--loop-file=inf" "--video-unscaled"
                                           "--no-terminal" "--no-audio" "--no-input-default-bindings"
                                           "--no-border" "--no-window-dragging" "--no-osc" "--no-osd-bar" abs-video)))))))

(run-with-idle-timer
 1 nil
 (lambda ()
   (file-notify-add-watch
    "~/.cache/wal/colors"
    '(change)
    (lambda (event)
      (mapc #'disable-theme custom-enabled-themes) ;; Disable all currently enabled themes
      (load-theme 'ewal-doom-one t)
      (enable-theme 'ewal-doom-one)
      (if pulse-cursor
      (update-pulsing-cursor-color))
      (setq laluxx/current-theme 'ewal-doom-one) ;; Set the current theme variable
      (laluxx/save-current-theme)))))
