(require 'url)

(defgroup tokyopromenade nil
  "Utilities for tokyopromenade based site"
  :group 'convenience)

(defcustom tokyopromenade-host ""
  "host name of tokyopromenade based site"
  :type 'string
  :group 'tokyopromenade)

(defcustom tokyopromenade-port 80
  "port number of tokyopromenade based site"
  :type 'integer
  :group 'tokyopromenade)

(defcustom tokyopromenade-path "/promenade.cgi"
  "path to promenade.cgi"
  :type 'string
  :group 'tokyopromenade)

(defcustom tokyopromenade-user "admin"
  "user name of tokyopromenade"
  :type 'string
  :group 'tokyopromenade)

(defcustom tokyopromenade-pass "nimda"
  "password for tokyopromenade-user"
  :type 'string
  :group 'tokyopromenade)

(defcustom tokyopromenade-article-template 'tokyopromenade-default-article-template
  "default template of new article"
  :type 'function
  :group 'tokyopromenade)

(defun tokyopromenade-default-article-template ()
  (mapconcat (lambda (_) _) (list
			     "#! "
			     (concat "#c " (tokyopromenade-get-current-time))
			     (concat "#m " (tokyopromenade-get-current-time))
			     (concat "#o " tokyopromenade-user)
			     "#t "
			     "") "\n"))

(defun tokyopromenade-get-current-time ()
  (concat (format-time-string "%Y-%m-%dT%T")
	  (format "+%02d:00" (/ (car (current-time-zone)) 3600))))

(defun tokyopromenade-uri ()
  (concat "http://" tokyopromenade-host
	  (if (/= tokyopromenade-port 80)
	      (number-to-string tokyopromenade-port) "")
	  tokyopromenade-path))

(defun tokyopromenade-login-token ()
  (concat "user=" tokyopromenade-user "&pass=" tokyopromenade-pass "&act=logincheck"))

(defun tokyopromenade-default-handler (status)
  (kill-buffer (current-buffer)))

(defun tokyopromenade ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*tokyopromenade*"))
  (tokyopromenade-mode)
  (insert (funcall tokyopromenade-article-template))
  (goto-char 4))

(defun tokyopromenade-login ()
  (interactive)
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (tokyopromenade-login-token)))
    (kill-buffer (url-retrieve-synchronously (tokyopromenade-uri)))
    (if (tokyopromenade-check-login)
	(message "tokyopromenade: login succeeded!"))))

(defun tokyopromenade-logout ()
  (interactive)
  (let ((url-request-data "act=logout"))
    (url-retrieve (tokyopromenade-uri) 'tokyopromenade-default-handler)
    (unless (tokyopromenade-check-login)
      (message "tokyopromenade: logout!"))))

(defun tokyopromenade-check-login ()
  (let ((cookies (assoc tokyopromenade-host url-cookie-storage)))
    (if cookies
	(let ((cookies (cdr cookies))
	      (foundp nil))
	  (while cookies
	    (setq cookie (pop cookies))
	    (if (and (string= (elt cookie 1) "auth")
		     (> (length (elt cookie 2)) 0))
		(setq foundp t)))
	  foundp))))

(defvar tokyopromenade-post-success-regexp
  "The article.*(\\([0-9]+\\)) have been stored")

(defun tokyopromenade-post-article (body)
  (interactive (list (buffer-string)))
  (unless (tokyopromenade-check-login) (tokyopromenade-login))
  (let ((url-request-method "POST")
	(url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data (concat "wiki=" (url-hexify-string body) "&act=update")))
    (url-retrieve (tokyopromenade-uri)
		  (lambda (status)
		    (if (re-search-forward tokyopromenade-post-success-regexp nil t)
			(message (concat "Post done. id=" (match-string 1)))
		      (message (buffer-string)))
		    (kill-buffer (current-buffer))))
    (kill-buffer (current-buffer))))

(defvar tokyopromenade-mode-map ()
  "Keymap used in Tokyo Promenade mode.")

(when (not tokyopromenade-mode-map)
  (setq tokyopromenade-mode-map (make-sparse-keymap))
  (define-key tokyopromenade-mode-map "\C-c\C-c" #'tokyopromenade-post-article))

(defvar tokyopromenade-font-lock-keywords
  '()
  "tokyopromenade keywords")

(define-derived-mode tokyopromenade-mode text-mode "Tokyo Promenade"
  "Major mode for editing article of tokyopromenade"
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults)
       '(tokyopromenade-font-lock-keywords nil t))
  (use-local-map tokyopromenade-mode-map)
  (run-hooks 'tokyopromenade-mode-hook))

(provide 'tokyopromenade)
