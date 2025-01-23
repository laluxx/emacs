;;; playwright.el --- Browser automation with Playwright -*- lexical-binding: t -*-

;; Author: Laluxx
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, browser, automation
;; URL: https://github.com/laluxx/playwright

;;; Commentary:
;; This package provides integration between Emacs and browser automation
;; using Playwright.  It allows sending commands from the minibuffer and
;; receiving browser output.


;;; Code:

(require 'json)

(defgroup playwright-emacs nil
  "Browser automation with Playwright."
  :group 'tools
  :prefix "playwright-")

(defcustom playwright-server-port 3000
  "Port number for the Playwright server."
  :type 'integer
  :group 'playwright-emacs)

(defvar playwright--process nil
  "Process handle for the Playwright server.")

(defvar playwright--output-buffer "*playwright-output*"
  "Buffer name for Playwright output.")

(defun playwright-start-server ()
  "Start the Playwright server process."
  (interactive)
  (unless (process-live-p playwright--process)
    (let ((default-directory (file-name-directory (locate-library "playwright"))))
      (setq playwright--process
            (start-process "playwright-server" "*playwright-server*"
                           "node" "server.js"))
      (message "Playwright server started"))))

(defun playwright-stop-server ()
  "Stop the Playwright server process."
  (interactive)
  (when (process-live-p playwright--process)
    (delete-process playwright--process)
    (setq playwright--process nil)
    (message "Playwright server stopped")))

(defun playwright-send-command (command)
  "Send COMMAND to the browser through Playwright."
  (interactive "sEnter browser command: ")
  (unless (process-live-p playwright--process)
    (playwright-start-server))
  (let ((url (format "http://localhost:%d/execute" playwright-server-port))
        (json-object (json-encode `((command . ,command)))))
    (url-retrieve url
                  (lambda (status)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (let ((response (json-read)))
                      (with-current-buffer (get-buffer-create playwright--output-buffer)
                        (goto-char (point-max))
                        (insert "\n" (cdr (assoc 'result response))))
                      (display-buffer playwright--output-buffer))))))

(defun playwright-clear-output ()
  "Clear the Playwright output buffer."
  (interactive)
  (with-current-buffer (get-buffer-create playwright--output-buffer)
    (erase-buffer)))

(provide 'playwright)
;;; playwright.el ends here
