;;; dired-auto-readme.el --- Auto-display README file in Dired buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; Display README files in Dired buffers when such are present in a manner
;; similar to public forges such as Codeberg/Github/Gitlab.  README files are
;; displayed by default as plain text buffers, but if you have additional
;; packages such as markdown-mode or org-view-mode installed, you can enable
;; those for prettier previews.

;;; Code:

(require 'dired)
(require 'org-fold)
(require 'markdown-mode)
(require 'text-property-search)
(require 'image-mode)


;;; User Options

(defgroup dired-auto-readme nil
  "Automatically display 'readme' files when present in a Dired buffer."
  :group 'files
  :prefix "dired-auto-readme")


(defcustom dired-auto-readme-files '("readme\\.md"
                                     "readme\\.org"
                                     "readme\\.rst"
                                     "readme\\.markdown"
                                     "readme"
                                     "manifest"
                                     "demo\\.gif"
                                     ".*\\.png"
                                     ".*\\.jpg"
                                     ".*\\.jpeg"
                                     ".*\\.svg")
  "A list of regular expressions used to tell which file to use."
  :type '(list string)
  :group 'dired-auto-readme)


(defun dired-auto-readme--image-type (file)
  "Determine the image type for FILE based on its extension."
  (when (string-match "\\.\\([^.]+\\)\\'" file)
    (let ((ext (match-string 1 file)))
      (pcase (downcase ext)
        ("png" 'png)
        ("jpg" 'jpeg)
        ("jpeg" 'jpeg)
        ("gif" 'gif)
        ("svg" 'svg)))))

(defun dired-auto-readme--process-org-links (content)
  "Process org-mode image links in CONTENT and return modified content."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\([^]]+?\\)\\]\\[\\([^]]+?\\)\\]\\]" nil t)
      (let* ((link (match-string 1))
             (description (match-string 2)))
        (when (and description
                   (string-match "^\\(?:file:\\|https?:\\)" description)
                   (string-match "\\.\\(?:svg\\|png\\|jpe?g\\|gif\\)\\'" description))
          (let* ((image-url (if (string-prefix-p "file:" description)
                                (substring description 5)
                              description))
                 (image (condition-case nil
                            (create-image image-url nil nil :scale 1.0)
                          (error nil))))
            (when image
              (delete-region (match-beginning 0) (match-end 0))
              (insert "\n")
              (insert-image image)
              (insert "\n"))))))
    (buffer-string)))

(defcustom dired-auto-readme-alist
  '((org-mode . dired-auto-readme-fake-org))
  "List of modes and custom hooks to call when a README buffer is read-in.
The hook is called after the text has been inserted in Dired buffer."
  :type 'alist)

;;; Implementation


(defun dired-auto-readme-fake-org ()
  "Hack to display descriptive links in Dired buffer."
  (org-fold-initialize "..."))

(defun dired-auto-readme--point ()
  "Return point of readme-file insertion or end of dired-buffer."
  (if-let ((dar (or (text-property-search-backward 'bis)
                    (text-property-search-forward 'bis))))
      (prop-match-beginning dar) (point-max)))

(defun dired-auto-readme--fontify-region (_ _ _)
  "Fontify Dired portion of the buffer."
  (font-lock-default-fontify-region 1 (dired-auto-readme--point) nil))

(defun dired-auto-readme--find-file ()
  "Return first file-name in current buffer matching readme patterns."
  (catch 'file
    (save-excursion
      (goto-char (point-min))
      (while (dired-next-line 1)
        (let ((file (dired-get-filename nil t))
              (case-fold-search t))
          (when (and file
                     (file-exists-p file)
                     (not (file-directory-p file)))
            (let ((basename (file-name-nondirectory file)))
              (dolist (pattern dired-auto-readme-files)
                (when (string-match-p (concat "^" pattern "$") basename)
                  (throw 'file file))))))))))

(defun dired-auto-readme--insert (&optional _)
  "Insert content of Readme/Demo file in current Dired buffer.
Handles both text files and GIFs appropriately."
  (save-excursion
    (when-let* ((file (dired-auto-readme--find-file)))
      (with-silent-modifications
        (setq-local font-lock-fontify-region-function
                    #'dired-auto-readme--fontify-region)
        (let* ((enable-local-variables nil)
               (data (dired-auto-readme--read-in file))
               (action (assoc (cdr data) dired-auto-readme-alist)))
          (goto-char (point-max))
          (if (eq (cdr data) 'image-mode)
              (progn
                (insert (car (car data)))
                ;; Start animation after insertion
                (let* ((image (cdr (car data)))
                       (frames (image-multi-frame-p image)))
                  (when (and (listp frames) (numberp (cdr frames)))
                    (image-animate image 0 t))))
            (progn
              (insert (car data))
              (when action
                (funcall (cdr action))))))))))

(defun dired-auto-readme--remove (&optional _)
  "Remove content of a Readme file from the current Dired buffer."
  (with-silent-modifications
    (save-excursion
      ;; invisibility spec is left with some garbage; fix for another day
      (kill-local-variable 'font-lock-fontify-region-function)
      (kill-local-variable 'auto-image-file-mode)
      (kill-local-variable 'image-animate-loop)
      (when-let ((dar (dired-auto-readme--point)))
        (delete-region dar (point-max))))))

(defun dired-auto-readme--enable ()
  "Insert README file in the current buffer."
  (add-hook 'dired-after-readin-hook #'dired-auto-readme--insert nil t)
  (add-hook 'dired-before-readin-hook #'dired-auto-readme--remove nil t)
  (advice-add 'wdired-change-to-dired-mode :after #'dired-auto-readme--insert)
  (advice-add 'wdired-change-to-wdired-mode :before #'dired-auto-readme--remove)
  (advice-add 'dired-create-directory :after #'dired-auto-readme--insert)
  (advice-add 'dired-create-directory :before #'dired-auto-readme--remove)
  (advice-add 'dired-create-empty-file :after #'dired-auto-readme--insert)
  (advice-add 'dired-create-empty-file :before #'dired-auto-readme--remove)
  (when (eq major-mode 'dired-mode)
    (with-silent-modifications
      (dired-auto-readme--insert)
      (dired-revert t t))))

(defun dired-auto-readme--disable ()
  "Remove README file from the current Dired buffer."
  (remove-hook 'dired-after-readin-hook #'dired-auto-readme--insert t)
  (remove-hook 'dired-before-readin-hook #'dired-auto-readme--remove t)
  (remove-hook 'post-command-hook #'dired-auto-readme--update-image t)
  (advice-remove 'dired-create-directory #'dired-auto-readme--insert)
  (advice-remove 'dired-create-directory #'dired-auto-readme--remove)
  (advice-remove 'dired-create-empty-file #'dired-auto-readme--insert)
  (advice-remove 'dired-create-empty-file #'dired-auto-readme--remove)
  (advice-remove 'wdired-change-to-dired-mode #'dired-auto-readme--insert)
  (advice-remove 'wdired-change-to-wdired-mode #'dired-auto-readme--remove)
  (and (eq major-mode 'dired-mode)
    (with-silent-modifications
      (dired-auto-readme--remove)
      (dired-revert t t))))

(defun dired-auto-readme--read-in (file)
  "Read and format FILE content for display in dired buffer.
For images (GIF, PNG, JPG, JPEG, SVG), display them directly.
For text files, format with appropriate modes and indentation."
  (with-temp-buffer
    (let ((buffer-file-name file))
      (insert "\n")  ; Space from dired listing
      (if (and (display-images-p)
               (dired-auto-readme--image-type file))
          (progn
            ;; For images: display them appropriately
            (let* ((image-path (expand-file-name file))
                   (image-type (dired-auto-readme--image-type file))
                   (image (condition-case nil
                              (create-image image-path image-type nil :scale 1.0)
                            (error nil))))
              (when image
                ;; Center the image with padding
                (insert (make-string 2 ?\s))  ; Left padding
                (insert-image image)
                (insert "\n\n")
                ;; For GIFs, handle animation
                (when (eq image-type 'gif)
                  (let ((frames (image-multi-frame-p image)))
                    (when (and (listp frames) (numberp (cdr frames)))
                      (image-animate image 0 t))))
                ;; Return the image object along with the buffer content
                (cons (cons (buffer-string) image) 'image-mode))))
        ;; For text files: handle org-mode links
        (insert-file-contents file)
        (set-auto-mode)
        (when (eq major-mode 'markdown-mode)
          (gfm-view-mode))
        ;; Process org links if in org-mode
        (when (eq major-mode 'org-mode)
          (let ((content (buffer-string)))
            (erase-buffer)
            (insert (dired-auto-readme--process-org-links content))))
        (font-lock-mode)
        (font-lock-ensure)
        (goto-char 1)
        (put-text-property
         1 2 'bis (if (listp buffer-invisibility-spec)
                      (copy-sequence buffer-invisibility-spec)
                    't))
        ;; Align text with dired-mode indentation
        (while (not (eobp))
          (insert "  ")
          (forward-line))
        (cons (buffer-string) major-mode)))))


;;; User commands
;;;###autoload
(define-minor-mode dired-auto-readme-mode
  "Dired minor mode to enable README file preview in current directory."
  :global nil :lighter " README"
  (if dired-auto-readme-mode
      (dired-auto-readme--enable)
    (dired-auto-readme--disable)))

(provide 'dired-auto-readme)
;;; dired-auto-readme.el ends here
