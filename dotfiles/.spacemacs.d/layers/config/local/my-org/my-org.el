(require 'org)
;; (require 'org-contacts)
;; (require 'org-bullets)

(provide 'my-org)

;;; Bingings and hooks

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'flyspell-mode)

(spacemacs/set-leader-keys
  "of" 'org-open-at-point-global
  "oc" 'org-capture
  "oa" 'org-agenda)


(evil-define-key '(normal visual motion) org-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

;;; Set up GTD-things

    ;; Set up org-directories
(setq org-directory         "~/org"
      org-archive-location  "~/org/archive"
      org-agenda-files      '("~/org/plan.org"
                              "~/org/appts.org"))

;; Basic properties
(setq org-agenda-dim-blocked-tasks                       nil
      org-agenda-window-setup                            'only-window
      org-cycle-separator-lines                          0
      org-hide-leading-stars                             t
      org-indirect-buffer-display                        'current-window
      org-link-frame-setup                               '(file . find-file)
      org-log-done                                       t
      org-outline-path-complete-in-steps                 nil
      org-refile-allow-creating-parent-nodes             'confirm
      org-refile-use-outline-path                        t
      org-return-follows-link                            t
      org-reverse-note-order                             t
      org-special-ctrl-a/e                               t
      org-src-fontify-natively                           t
      org-startup-indented                               t
      org-treat-S-cursor-todo-selection-as-state-change  nil
      org-use-fast-todo-selection                        t)

;; Capture-templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline "~/org/plan.org" "Todos")
               "* TODO %? %^g\n" :kill-buffer)
              ("T" "todo-here" entry (file+headline "~/org/plan.org" "Todos")
               "* TODO %? %^g\n%a\n" :kill-buffer)
              ("a" "appt" entry (file+datetree+prompt "~/org/appts.org")
               "* TODO %? %^g\n%T" :kill-buffer)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :kill-buffer)
              ("j" "journal" entry (file+datetree "~/org/journal.org")
               "* %?\n%U\n" :kill-buffer)
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t :kill-buffer))))

;; Todo-keyword options
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")))

;; Standard tags
(setq org-tag-alist '(("work"         . "?w")
                      ("maintenance"  . "?m")
                      ("admin"        . "?a")))


;; Configure refiling behavior
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
;; Set the file-openers
(setq org-file-apps '(("\\.pdf\\'"    . "zathura %s")
                      (auto-mode      . emacs)
                      ("\\.mm\\'"     . default)
                      ("\\.x?html\\'" . default)))

(setq org-agenda-custom-commands
      '(("v" "Agenda and todo's"
         ((agenda "")
          (org-todo-list)))))
