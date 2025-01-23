;; kitty-graphics.el --- Display images in terminal Emacs using Kitty graphics protocol -*- lexical-binding: t -*-

;; Copyright (C) 2024 Laluxx
;; Author: Laluxx
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia
;; URL: https://github.com/laluxx/kitty-graphics

;;; Commentary:
;; This package enables displaying images in terminal Emacs when running
;; a terminal emulator that support Kitty graphics protocol.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup kitty-graphics nil
  "Display images in terminal Emacs using Kitty graphics protocol."
  :group 'multimedia
  :prefix "kitty-graphics-")

(defvar kitty-graphics--image-counter 0
  "Counter for generating unique image IDs.")

(defvar kitty-graphics--active-images (make-hash-table :test 'equal)
  "Hash table storing active image information.")

(defun kitty-graphics--encode-base64 (data)
  "Encode DATA as base64 string."
  (base64-encode-string data t))

(defun kitty-graphics--read-file-bytes (filename)
  "Read FILENAME as binary data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename)
    (buffer-string)))

(defun kitty-graphics--send-command (control-data payload)
  "Send Kitty graphics command with CONTROL-DATA and PAYLOAD."
  (let ((command (format "\033_G%s;%s\033\\" 
                         control-data 
                         (if payload 
                             (kitty-graphics--encode-base64 payload)
                           ""))))
    (send-string-to-terminal command)))

(defun kitty-graphics-show-image (filename &optional width height x-offset y-offset)
  "Display image from FILENAME at current point.
Optional WIDTH and HEIGHT specify size in cells.
Optional X-OFFSET and Y-OFFSET specify placement within the cell."
  (interactive "fImage file: ")
  (when (and (display-graphic-p)
             (not (getenv "KITTY_WINDOW_ID")))
    (error "This command only works in the Kitty terminal emulator"))
  
  (let* ((image-id (cl-incf kitty-graphics--image-counter))
         (image-data (kitty-graphics--read-file-bytes filename))
         (control-data
          (format "a=T,f=100,i=%d%s%s%s%s"
                  image-id
                  (if width (format ",c=%d" width) "")
                  (if height (format ",r=%d" height) "")
                  (if x-offset (format ",X=%d" x-offset) "")
                  (if y-offset (format ",Y=%d" y-offset) ""))))
    
    ;; Send image data
    (kitty-graphics--send-command control-data image-data)
    
    ;; Store image information
    (puthash image-id 
             (list :filename filename
                   :width width
                   :height height
                   :x-offset x-offset
                   :y-offset y-offset)
             kitty-graphics--active-images)
    
    ;; Return image ID for potential future reference
    image-id))

(defun kitty-graphics-clear-image (image-id)
  "Delete image with IMAGE-ID from display."
  (interactive "nImage ID to clear: ")
  (when (gethash image-id kitty-graphics--active-images)
    (kitty-graphics--send-command 
     (format "a=d,d=i,i=%d" image-id)
     nil)
    (remhash image-id kitty-graphics--active-images)))

(defun kitty-graphics-clear-all ()
  "Clear all displayed images."
  (interactive)
  (kitty-graphics--send-command "a=d" nil)
  (clrhash kitty-graphics--active-images))

;;;###autoload
(define-minor-mode kitty-graphics-mode
  "Minor mode for displaying images using Kitty graphics protocol."
  :lighter " KittyImg"
  :global t
  (if kitty-graphics-mode
      (unless (getenv "KITTY_WINDOW_ID")
        (setq kitty-graphics-mode nil)
        (error "Kitty graphics mode requires running in Kitty terminal emulator"))
    (kitty-graphics-clear-all)))

(provide 'kitty-graphics)

;;; kitty-graphics.el ends here
