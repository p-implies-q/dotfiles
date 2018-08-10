;;;; Dotspacemacs

;; -- David Janssen's Spacemacs Configuration --
;; -- Contact: janssen.dhj@gmail.com

(defun dotspacemacs/init ()
  "Spacemacs core settings."

  (setq-default
   ;; General settings
   dotspacemacs-distribution               'spacemacs
   dotspacemacs-enable-lazy-installation   'unused
   dotspacemacs-ask-for-lazy-installation  t
   dotspacemacs-configuration-layer-path   '("~/.spacemacs.d/local/")


   ;; Programming settings
   dotspacemacs-search-tools              '("ag" "pt" "ack" "grep")
   dotspacemacs-smooth-scrolling          t
   dotspacemacs-folding-method            'evil
   dotspacemacs-smartparens-strict-mode   nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters      'all
   dotspacemacs-line-numbers              nil
   dotspacemacs-whitespace-cleanup        'trailing

   ;; Appearance settings
   dotspacemacs-themes                     '(spacemacs-dark
                                             leuven)
   dotspacemacs-default-font               '("Mononoki Nerd Font"
                                             :size 16
                                             :weight normal
                                             :width normal
                                             :powerline-scale 1.6)
   dotspacemacs-fullscreen-at-startup      nil
   dotspacemacs-fullscreen-use-non-native  nil
   dotspacemacs-maximized-at-startup       nil
   dotspacemacs-active-transparency        100
   dotspacemacs-inactive-transparency      100
   dotspacemacs-mode-line-unicode-symbols  t
   dotspacemacs-frame-title-format         "%I@%S"
   dotspacemacs-icon-title-format          nil

   ;; Evil settings
   dotspacemacs-editing-style                       'vim
   dotspacemacs-colorize-cursor-according-to-state  t
   dotspacemacs-remap-Y-to-y$                       t
   dotspacemacs-retain-visual-state-on-shift        t
   dotspacemacs-visual-line-move-text               nil
   dotspacemacs-ex-substitute-global                nil
   dotspacemacs-enable-paste-transient-state        nil
   dotspacemacs-show-transient-state-title          t
   dotspacemacs-show-transient-state-color-guide    t

   ;; Key settings
   dotspacemacs-leader-key                  "SPC"
   dotspacemacs-emacs-command-key           "SPC"
   dotspacemacs-ex-command-key              ":"
   dotspacemacs-emacs-leader-key            "s-m"
   dotspacemacs-major-mode-leader-key       ","
   dotspacemacs-major-mode-emacs-leader-key "s-M"
   dotspacemacs-which-key-delay             0.4
   dotspacemacs-which-key-position          'bottom
   dotspacemacs-distinguish-gui-tab         nil

   ;; Layouts settings
   dotspacemacs-scratch-mode                        'org-mode
   dotspacemacs-default-layout-name                 "Default"
   dotspacemacs-display-default-layout              nil
   dotspacemacs-auto-resume-layouts                 nil
   dotspacemacs-auto-generate-layout-names          t
   dotspacemacs-switch-to-buffer-prefers-purpose    nil

   ;; Misc settings
   dotspacemacs-large-file-size          1
   dotspacemacs-auto-save-file-location  'cache
   dotspacemacs-max-rollback-slots       5
   dotspacemacs-persistent-server        nil
   dotspacemacs-helm-resize              nil
   dotspacemacs-helm-no-header           nil
   dotspacemacs-helm-position            'bottom
   custom-file                           "~/.spacemacs.d/custom.el"

   ;; Packages settings
   dotspacemacs-additional-packages     '(password-store
                                          esh-autosuggest
                                          keychain-environment
                                          )
   dotspacemacs-delete-orphan-packages  t
   dotspacemacs-excluded-packages       '(exec-path-from-shell
                                          org-projectile
                                          )
   dotspacemacs-frozen-packages         '()
   dotspacemacs-install-packages        'used-but-keep-unused
   dotspacemacs-default-package-repository  nil
   dotspacemacs-elpa-https                  t
   dotspacemacs-elpa-timout                 5
   dotspacemacs-check-for-update            nil ;; Untill internet is back and trustworthy
   dotspacemacs-elpa-subdirectory           nil

   ;; Startup settings
   dotspacemacs-verbose-loading            nil
   dotspacemacs-startup-banner             'official
   dotspacemacs-startup-lists              '()
   dotspacemacs-startup-buffer-responsive  t
   dotspacemacs-loading-progress-bar       t
   )


   ;; Global toggles
   (fringe-mode '(0 . 8))
  )
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-configuration-layers
   '(
     ;; Layers for general use
     (my-org :location local)
     docker
     git
     org
     shell-scripts
     themes-megapack
     yaml

     (auto-completion
      :variables
      auto-completion-return-key-behavior              'complete
      auto-completion-tab-key-behavior                 'cycle
      auto-completion-enable-snippets-in-popup         t)
     ;; (shell
     ;;  :variables
     ;;  shell-default-shell 'ansi-term
     ;;  shell-default-position 'bottom
     ;;  shell-default-height 30
     ;;  shell-default-term-shell "/bin/fish"
     ;;  shell-default-full-span nil
     ;;  )
     (shell
      :variables
      shell-default-shell    'eshell
      shell-default-position 'bottom
      shell-default-height   30
      )

     (syntax-checking
      :variables
      syntax-checking-enable-tooltips                  nil)
     (version-control
      :variables
      version-control-global-margin t
      version-control-diff-tool 'git-gutter+)

     ;; Layers for specifec modes

     emacs-lisp
     html
     javascript
     markdown
     nixos

     (haskell
      :variables
      haskell-completion-backend     'intero
      haskell-enable-ghc-mod-support nil
      haskell-process-type           'stack-ghci
      haskell-process-args-cabal-repl '("--ghc-options=-ferror-spans -fshow-loaded-modules"))
     (mu4e
      :variables
      ;; mu4e-installation-path "/usr/share/emacs/site-lisp"
      mu4e-installation-path "/run/current-system/sw/share/emacs/site-lisp/"
      )
     (python
      :variables
      python-sort-imports-on-save t
      python-test-runner 'pytest)

     (purescript)
     )))
(defun dotspacemacs/user-config ()
  (keychain-refresh-environment)

  ;; Little fix for shell-mode
  (add-hook 'term-mode-hook 'toggle-truncate-lines)
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)

  ;; Some more customization for html-mode
  (setq-default
   web-mode-css-indent-offset    2
   web-mode-code-indent-offset   2
   web-mode-markup-indent-offset 2
   css-indent-offset             2)

  (defadvice load-theme (before theme-dont-propagate activate)
    (mapcar #'disable-theme custom-enabled-themes))

  (setq mu4e-maildir       "~/docs/mail"
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash")

  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"              . ?i)
           ("/[Gmail].Sent Mail"  . ?s)
           ("/[Gmail].All Mail"   . ?a)))

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "janssen.dhj@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (setq message-kill-buffer-on-exit t)

  (setq
   user-mail-address "janssen.dhj@gmail.com"
   user-full-name     "David Janssen")

  (spacemacs/set-leader-keys "op" 'password-store-copy)
  (spacemacs/set-leader-keys "oa" 'org-agenda)
  (spacemacs/set-leader-keys "oc" 'org-capture)
  (spacemacs/set-leader-keys "ob" 'kill-some-buffers)
  (define-key evil-motion-state-map "!" nil)
  (load-theme 'darktooth t)

  ;; Configure org
  (add-hook 'org-mode-hook
    (lambda ()
      (org-indent-mode    t)
      (auto-fill-mode     t)
      ))

  ;; Add nix-mode to the automode alist
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

  )
