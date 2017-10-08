(setq config-packages
  '(

    ;; Core
    evil

    ;; Navigation
    avy
    outshine
    neotree
    projectile

    ;; Misc
    all-the-icons
    ispell
    mu4e
    yasnippet

    ;; Large sections
    (my-org  :location local)
    (my-mu4e :location local)
    (my-exwm :location local)
    ))

;;; Core
;;;; Evil

(defun config/post-init-evil ()
  (setq evil-escape-key-sequence           "fp"
        evil-escape-unordered-key-sequence "true")

;;   (evil-global-set-keys
;;    '(normal visual motion)
;;    "H" 'evil-first-non-blank
;;    "L" (lambda () (interactive) (evil-end-of-line))
;;    "0" 'evil-jump-item)

  (advice-add 'evil-ex-search-next     :after 'config/scroll-to-center-advice)
  (advice-add 'evil-ex-search-previous :after 'config/scroll-to-center-advice))

;;; Navigation
;;;; Avy

(defun config/post-init-avy ()
  (setq avy-timeout-seconds 0.35)
  (evil-global-set-key 'normal (kbd "s") 'avy-goto-char-timer)
  (global-set-key (kbd "C-h") 'avy-pop-mark)
  (global-set-key (kbd "C-l") 'evil-avy-goto-line))

;;;; Outshine

(defun config/init-outshine ()
  (use-package outshine
    :init
    (progn
      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen)

      (let ((kmap outline-minor-mode-map))
        (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
        (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)))

    :config
    (progn
      (advice-add 'outshine-narrow-to-subtree :before
                  (lambda (&rest args) (unless (outline-on-heading-p t)
                                         (outline-previous-visible-heading 1))))
      (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
      (add-hook 'prog-mode-hook 'outline-minor-mode))))

;;;; Neotree

(defun config/pre-init-neotree ()
  (evil-global-set-key 'normal (kbd "M-f") 'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "M-p") 'neotree-find-project-root))

(defun config/post-init-neotree ()
  ;; (setq neo-theme 'icons)
  (setq neo-window-width 28)
  (setq neo-hidden-regexp-list '("^\\." "^#.*#$" "\\.elc$")))

;;;; Dired

(defun config/pre-init-dired ()
  (setq dired-listing-switches "-lhgoBF --group-directories-first"))

;;;; Projectile

(defun config/post-init-projectile ()
  (setq projectile-indexing-method 'native))

;;; Misc
;;;; Ispell
(defun config/post-init-ispell ()
  (setq ispell-program-name "aspell"))

;;; Local packages
(defun config/init-my-org ()
  (use-package my-org
    :after org))

(defun config/init-mu4e ()
  (use-package mu4e
    :load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e"))

(defun config/init-my-mu4e ()
  (use-package my-mu4e
    :after mu4e))

(defun config/init-my-exwm ()
  (use-package my-exwm))
