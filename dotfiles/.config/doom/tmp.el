;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


;; Each path is relative to `+email-mu4e-mail-path', which is ~/.mail by default
(set! :email "David"
    '((+email-mu4e-mail-path  . "/home/david/docs/mail")
      (mu4e-mail-path         . "/home/david/docs/mail")
      (mu4e-sent-folder       . "[Gmail].Sent Mail")
      (mu4e-drafts-folder     . "[Gmail].Drafts")
      (mu4e-trash-folder      . "[Gmail].Trash")
      ;; (mu4e-refile-folder     . "/Lissner.net/All Mail")
      (smtpmail-smtp-user     . "janssen.dhj@gmail.com")
      (user-mail-address      . "janssen.dhj@gmail.com")
      (mu4e-compose-signature . "---\nDavid Janssen"))
    t)

