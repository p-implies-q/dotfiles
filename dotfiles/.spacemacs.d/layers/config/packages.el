(setq config-packages
  '(

    ;; Core
    dired
    evil
    helm

    ;; Navigation
    avy
    outshine
    neotree
    projectile

    ;; Misc
    emms
    all-the-icons
    ispell
    mu4e
    yasnippet

    ;; Large sections
    (my-org    :location local)
    (my-mu4e   :location local)
    ))

;;; Core
;;;; Evil

(defun config/post-init-evil ()
  (setq evil-escape-unordered-key-sequence "true")

  (advice-add 'evil-ex-search-next     :after 'config/scroll-to-center-advice)
  (advice-add 'evil-ex-search-previous :after 'config/scroll-to-center-advice))

;;; Navigation
;;;; Avy

(defun config/post-init-avy ()
  (setq avy-timeout-seconds 0.35)
  (evil-global-set-key 'normal (kbd "s") 'avy-goto-char-timer)
  ;; (global-set-key (kbd "C-h") 'avy-pop-mark)
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

(defun config/init-all-the-icons ()
  (use-package all-the-icons))

(defun config/pre-init-neotree ()
  (evil-global-set-key 'normal (kbd "M-f") 'winum-select-window-0)
  (evil-global-set-key 'normal (kbd "M-p") 'neotree-find-project-root))

(defun config/post-init-neotree ()
  (setq neo-theme 'icons)
  (setq neo-window-width 28)
  (setq neo-hidden-regexp-list '("^\\." "^#.*#$" "\\.elc$")))

;;;; Dired

(defun config/pre-init-dired ()
  (setq dired-listing-switches "-lhgoBF --group-directories-first"))

(defun config/init-dired ()
  (use-package dired))

(defun config/post-init-dired ()
  (add-hook 'dired-after-readin-hook 'dired-dotfiles-toggle)
  (define-key dired-mode-map (kbd "C-s") 'dired-dotfiles-toggle)
  )


;;;; Projectile

(defun config/post-init-projectile ()
  (setq projectile-indexing-method 'native))

;;; Misc
;;;; Ispell
(defun config/post-init-ispell ()
  (setq ispell-program-name "aspell"))
;;;; EMMS
(defun config/init-emms ()
  (use-package emms
    :ensure t :defer t
    :config
    (progn
      (require 'emms-setup)
      (emms-all)
      (emms-default-players)
      (require 'emms-player-simple)
      (require 'emms-source-file)
      (require 'emms-source-playlist)
      (require 'emms-player-vlc)
      (setq emms-player-list '(emms-player-vlc)
            emms-source-file-default-directory "~/docs/audio/music")
      )))

;;; Local packages

(defun config/init-my-org ()
  (use-package my-org
    :after org))

(defun config/pre-init-mu4e ()
  (add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e"))

(defun config/init-my-mu4e ()
  (use-package my-mu4e
    :after mu4e))

(defun config/init-my-eshell ()
  (use-package my-eshell))
