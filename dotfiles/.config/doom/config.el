;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-



;; (after! email (setq +email-mu4e-mail-path "/home/david/docs/mail"))
;; (defvar +email-mu4e-mail-path "/home/david/docs/mail")
;; (setq mu4e-maildir "/home/david/docs/mail")

(setq mu4e-maildir "/home/david/docs/mail"
      mu4e-sent-folder        "/[Gmail].Sent Mail"
      mu4e-drafts-folder      "/[Gmail].Drafts"
      mu4e-trash-folder       "/[Gmail].Trash"
      smtpmail-smtp-user      "janssen.dhj@gmail.com"
      user-mail-address       "janssen.dhj@gmail.com"
      mu4e-compose-signature  "---\nDavid Janssen")

(setq projectile-project-search-path '("~/proj/" "~/opt"))
