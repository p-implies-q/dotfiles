;;; packages.el --- my-gtd layer packages file for Spacemacs.
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
;; added to `my-gtd-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-gtd/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-gtd/pre-init-PACKAGE' and/or
;;   `my-gtd/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-gtd-packages
  '(org))

(defconst my-gtd-dir
  (expand-file-name "~/docs/org/"))

(defconst my-gtd-files-alist
  (--map (cons (car it) (expand-file-name (cdr it) my-gtd-dir))
         '((inbox    . "inbox.org")
           (agenda   . "agenda.org")
           (projects . "projects.org")))
  "An alist of paths to my various gtd files")

(defun my-gtd-file (x)
  (cdr (assoc x my-gtd-files-alist)))

(defun my-gtd/post-init-org ()

  (setq org-agenda-files (--map (cdr it) my-gtd-files-alist))

  ;; (setq org-capture-templates
  ;;       '(
  ;;         ("t" "Todo" entry
  ;;          (file+headline (my-gtd-file 'inbox) "Todos")
  ;;          "* TODO %i%?")

  ;;         ("T" "Tickler" entry
  ;;          (file+headline (my-gtd-file 'tickler))))))

  )

;;; packages.el ends here
