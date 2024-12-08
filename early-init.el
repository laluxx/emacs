(setq gc-cons-threshold most-positive-fixnum)

;; emacs --shut-the-fuck-up
(defun display-startup-echo-area-message ()
  (message ""))
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(add-to-list 'default-frame-alist '(background-color . "#18181B")) ;; kaolin-dark

(set-face-attribute 'default nil
                    :family "JetBrains Mono Nerd Font"
                    :weight 'medium
                    ;; :height 110
                    :height 130
                    )

(setq frame-resize-pixelwise t)
(electric-pair-mode)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

