;;; ~/opt/dotfiles/dotfiles/.config/doom/+email.el -*- lexical-binding: t; -*-


(def-modeline-format! 'mu4e
  '(+modeline-matches " %b  %2l:%c %p  "))

(add-hook! (mu4e-main-mode mu4e-headers-mode)
  (set-modeline! 'mu4e)
  (setq header-line-format mode-line-format
        mode-line-format nil))

(after! mu4e

  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-maildir (expand-file-name "~/docs/mail"))
  (setq mu4e-attachment-dir (expand-file-name "~/docs/attachments"))
  (setq mu4e-get-mail-command "offlineimap -o")
  (setq mu4e-hide-index-messages nil)


  (setq message-kill-buffer-on-exit t)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)

  (require 'mu4e-contrib)

  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-use-fancy-chars t)

  (setq shr-color-visible-luminance-min 80)

  (setq smtpmail-stream-type 'starttls)
  (setq smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)

  (setq sendmail-program "msmtp")

  (setq send-mail-function 'smtpmail-send-it)

  (set-email-account! "David"
    '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
      (mu4e-drafts-folder     . "/[Gmail].Drafts")
      (mu4e-trash-folder      . "/[Gmail].Trash")
      (user-full-name         . "David Janssen")
      (user-mail-address      . "janssen.dhj@gmail.com")
      (smtpmail-smtp-user     . "janssen.dhj@gmail.com")) t)

  (setq mu4e-bookmarks
    '(("flag:unread AND NOT (maildir:/[Gmail]/Drafts) AND NOT (maildir:/[Gmail]/Trash)" "Unread messages" 117)
      ("date:today..now AND NOT (maildir:/[Gmail]/Drafts) AND NOT (maildir:/[Gmail]/Trash)" "Today's messages" 116)
      ("date:7d..now AND NOT (maildir:/[Gmail]/Drafts) AND NOT (maildir:/[Gmail]/Trash)" "Last 7 days" 119)
      ("mime:image/*" "Messages with images" 112)))


  ;; (add-hook 'mu4e-main-mode-hook '+private/m4ue-reduce-header)
  ;; (add-hook 'mu4e-index-updated-hook '+private/force-mail-index)
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime))

;; (def-package! mu4e-maildirs-extension
;;   :after mu4e
;;   :config
;;   (mu4e-maildirs-extension)
;;   (setq mu4e-maildirs-extension-action-text "\n    * [g] Update mail and database\n")
;;   (setq mu4e-maildirs-extension-maildir-expanded-prefix "")
;;   (setq mu4e-maildirs-extension-maildir-default-prefix "")
;;   (setq mu4e-maildirs-extension-title "  Inboxes"))

(provide '+mail)

;;; +mail.el ends here
