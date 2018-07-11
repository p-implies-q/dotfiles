;;; packages.el --- my-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: David Janssen <janssen.dhj@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-org/pre-init-PACKAGE' and/or
;;   `my-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-org-packages
  '(org
    dash
    dash-functional))

(defun my-org/post-init-org ()

  (defconst my-org-dir
    (expand-file-name "~/docs/org/"))

  (defconst my-org-files-alist
    (--map (cons (car it) (expand-file-name (cdr it) my-org-dir))
           '((inbox    . "inbox.org")
             (agenda   . "agenda.org")
             (projects . "projects.org")
             (log      . "log.org")))
    "An alist of paths to my various org files")



  (setq org-agenda-files (--map (cdr it) my-org-files-alist))
  (setq org-todo-keywords
        '("TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)" ))
  (setq org-tag-persistent-alist
        '(("@home"    . ?h)
          ("@gym"    . ?g)
          ("laptop"   . ?l)))
  (setq org-capture-templates
   `(("t" "Todo" entry
      (file+headline ,(my-org-file 'inbox) "Todos")
      "* TODO %i%?")
     ("m" "At time" entry
      (file+olp+datetree ,(my-org-file 'agenda))
      "* TODO %? %^G \n%^T")
     ("d" "At date" entry
      (file+olp+datetree ,(my-org-file 'agenda))
      "* TODO %? %^G \n%^t")))
  (setq org-startup-folded t)
  (setq org-log-done nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-todo-ignore-deadlines t
        org-agenda-todo-ignore-with-date t
        org-agenda-todo-ignore-scheduled t)

  )

;;; packages.el ends here
