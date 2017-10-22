(require 'mu4e)

(provide 'my-mu4e)

(setq mu4e-maildir        "~/docs/mail"
      mu4e-sent-folder    "/[Gmail].Drafts"
      mu4e-drafts-folder  "/[Gmail].Sent Mail"
      mu4e-trash-folder   "/[Gmail].Trash")

(setq mu4e-bookmarks '(("maildir:/INBOX AND flag:unread" "Inbox" ?i)))

(setq user-mail-address       "janssen.dhj@gmail.com"
      user-full-name          "David H.J. Janssen"
      mu4e-compose-signature  "\nDavid H.J. Janssen")

(setq message-send-mail-function    'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
      smtpmail-auth-credentials     '(("smtp.gmail.com" 587 "janssen.dhj@gmail.com" nil))
      smtpmail-default-smtp-server  "smtp.gmail.com"
      smtpmail-smtp-server          "smtp.gmail.com"
      smtpmail-smtp-service         587
      smtpmail-debug-info           t
      starttls-use-gnutls           t)

(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300
      mu4e-sent-messages-behavior 'delete
      message-kill-buffer-on-exit t)

(setq org-mu4e-link-query-in-headers-mode nil)

(setq mu4e-maildirs-extension-maildir-format "  %n: %u"
      mu4e-maildirs-extension-custom-list '("/INBOX"))

(defun my/mu4e-maildirs-formatter (item)
  "Pretty-print a maildir"
  (let* ((unread (or (plist-get item :unread) 0))
         (faced  (cond
                  ((> unread 0) 'mu4e-maildirs-extension-maildir-hl-face)
                  (t            'mu4e-maildirs-extension-maildir-face))))
    (format "\t%s : %s"
            (plist-get item :name)
            (propertize (number-to-string unread) 'face faced))))
(setq mu4e-maildirs-extension-propertize-func 'my/mu4e-maildirs-formatter)

(setq mu4e-headers-date-format "%Y-%m-%d %H:%M:%S"
      mu4e-headers-fields '((:date . 20)
                            (:flags . 5)
                            (:mailing-list . 10)
                            (:from-or-to . 25)
                            (:subject . nil)))

(define-key mu4e-view-mode-map
  [remap mu4e-headers-mark-for-refile] 'mu4e-view-mark-as-read)
