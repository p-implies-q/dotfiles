;;; ~/opt/dotfiles/dotfiles/.config/doom/+email.el -*- lexical-binding: t; -*-




;; Make sure we load mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(def-modeline-format! 'mu4e
  '(+modeline-matches " %b  %2l:%c %p  "))

(add-hook! (mu4e-main-mode mu4e-headers-mode)
  (set-modeline! 'mu4e)
  (setq header-line-format mode-line-format
        mode-line-format nil))

(after! mu4e

  ;; General config
  (require 'mu4e-contrib)
  (setq mail-user-agent          'mu4e-user-agent
        mu4e-use-fancy-chars     t
        mu4e-html2text-command   'mu4e-shr2text
        mu4e-hide-index-messages nil)

  (setq mu4e-maildirs-extension-custom-list
        '("/INBOX"))

  ;; Local storage config
  (setq mu4e-maildir        (expand-file-name "~/dcs/mail/gmail")
        mu4e-attachment-dir (expand-file-name "~/dcs/mail/attachments"))

  ;; Setup acounts
  (set-email-account! "David"
    '((mu4e-sent-folder . "/[Gmail].Sent Mail")
      (mu4e-drafts-folder . "/[Gmail].Drafts")
      (mu4e-trash-folder . "/[Gmail].Trash")

      ;; (mu4e-sent-folder       . "/sent")
      ;; (mu4e-drafts-folder     . "/drafts")
      ;; (mu4e-trash-folder      . "/trash")
      ;; (mu4e-refile-folder     . "/archive")
      (user-full-name         . "David Janssen")
      (user-mail-address      . "janssen.dhj@gmail.com")
      (smtpmail-smtp-user     . "janssen.dhj@gmail.com")) t)

  ;; Views
  (setq mu4e-bookmarks
    '(("maildir:/inbox and flag:unread"     "Unread messages" 117)
      ("maildir:/inbox and date:today..now" "Today's messages" 116)
      ("maildir:/inbox and date:7d..now"    "Last 7 days" 119)))

  ;; Fetching and sending mail
  (setq mu4e-get-mail-command "offlineimap -o")
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t
        sendmail-program           "/usr/bin/msmtp"
        user-full-name             "David Janssen")

  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Restore delete and refile to mu4e marks
  ;; (add-to-list 'mu4e-marks
  ;;    '(delete
  ;;      :char ("D" . "❌")
  ;;      :prompt "Delete"
  ;;      :show-target (lambda (target) "delete")
  ;;      :action (lambda (docid msg target) (mu4e~proc-remove docid))))
  ;; (setf (alist-get 'refile mu4e-marks)
  ;;   '(:char ("r" . "▶")
  ;;     :prompt "refile"
  ;;     :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
  ;;     :action (lambda (docid msg target) (mu4e~proc-move docid
  ;;   					    (mu4e~mark-check-target target) "-N"))))


  ;; (setq mu4e-maildirs-extension-use-maildirs nil)
  (setq mu4e-maildirs-extension-action-text nil)
  ;; (setq mu4e-change-filenames-when-moving t)
  ;; )
  )
(provide '+mail)

