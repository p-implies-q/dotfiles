;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   '(
     (auto-completion
      (haskell :variables haskell-completion-backend 'intero)
      :variables auto-completion-complete-with-key-sequence-delay 0.01)
     emacs-lisp
     evil-cleverparens
     git
     (haskell :variables haskell-enable-ghc-mod-support nil
              haskell-process-type 'stack-ghci)
     html
     javascript
     latex
     markdown
     (mu4e :variables
           mu4e-installation-path "/run/current-system/sw/share/emacs/site-lisp")
     nixos
     org
     pandoc
     purescript
     python
     shell-scripts
     sml
     (syntax-checking :variables syntax-checking-enable-by-default t)
     themes-megapack
     theming
     yaml
   )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(bbdb-handy
                                      noflet)

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(exec-path-from-shell)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before Layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gruvbox)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Mononoki Nerd Font"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.6)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.01
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (load-file "~/.spacemacs.d/dash.el")
  (load-file "~/.spacemacs.d/autothemer.el")
  )

(defun dotspacemacs/user-config ()

  (defun magit-single-window ()
    (interactive)
    (let ((display-buffer-alist `(("^\\*magit: " display-buffer-same-window),
                                  display-buffer-alist)))
      (progn (magit-status)
             (delete-other-windows))))

  ;; The sequence of keys that causes an 'esc' to occur
  (setq-default evil-escape-key-sequence "fp")

  ;; ORG-MODE
  (with-eval-after-load 'org

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

    ;; Set common org-mode commands to shorter sequence
    (spacemacs/set-leader-keys "oc" 'org-capture)
    (spacemacs/set-leader-keys "oa" 'org-agenda)

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



    ;; enable auto-fill-mode
    (add-hook 'org-mode-hook
              '(lambda ()
                 (auto-fill-mode)))

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

    ;; Setup BBDB
    ;; (bbdb-handy-enable)

    )



  ;; Helm configuration
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")   'helm-select-action)
  )


  ;; Mu4e configuration
  (add-to-list 'load-path "/run/current-system/sw/share/emacs/site-lisp/mu4e")
  (with-eval-after-load 'mu4e

    (setq mu4e-maildir        "~/docs/mail"
          mu4e-sent-folder    "/[Gmail].Drafts"
          mu4e-drafts-folder  "/[Gmail].Sent Mail"
          mu4e-trash-folder   "/[Gmail].Trash")

    (setq mu4e-bookmarks '(("maildir:/INBOX AND flag:unread" "Inbox" ?i)))

    (setq user-mail-address       "janssen.dhj@gmail.com"
          user-full-name          "David H.J. Janssen"
          mu4e-compose-signature  "\nDavid H.J. Janssen")

    (setq message-send-mail-function    'smtpmail-send-it
          smtpmail-starttls-credentials '(("smtp.gmail.com" "587" nil nil))
          smtpmail-auth-credentials     '(("smtp.gmail.com" 587 "janssen.dhj@gmail.com" nil))
          smtpmail-default-smtp-server  "smtp.gmail.com"
          smtpmail-smtp-server          "smtp.gmail.com"
          smtpmail-smtp-service         587
          smtpmail-debug-info           t
          starttls-use-gnutls           t)

    (setq mu4e-get-mail-command "offlineimap"
          mu4e-update-interval 300
          mu4e-sent-messages-behavior 'delete
          message-kill-buffer-on-exit t)

    (setq org-mu4e-link-query-in-headers-mode nil)

    (setq mu4e-maildirs-extension-maildir-format "  %n: %u"
          mu4e-maildirs-extension-custom-list '("/INBOX"))

    (defun my/mu4e-maildirs-formatter (item)
      "Pretty-print a maildir"
      (let* ((unread (or (plist-get item :unread) 0))
             (faced  (cond
                      ((> unread 0) 'mu4e-maildirs-extension-maildir-hl-face)
                      (t            'mu4e-maildirs-extension-maildir-face))))
        (format "\t%s : %s"
                (plist-get item :name)
                (propertize (number-to-string unread) 'face faced))))
    (setq mu4e-maildirs-extension-propertize-func 'my/mu4e-maildirs-formatter)

    (setq mu4e-headers-date-format "%Y-%m-%d %H:%M:%S"
          mu4e-headers-fields '((:date . 20)
                                (:flags . 5)
                                (:mailing-list . 10)
                                (:from-or-to . 25)
                                (:subject . nil)))

    (define-key mu4e-view-mode-map
      [remap mu4e-headers-mark-for-refile] 'mu4e-view-mark-as-read)

    )

  (set-face-attribute 'variable-pitch nil :family "Inconsolata for Powerline")

  ;; At startup, switch to org-agenda
  (org-agenda-list)
  (switch-to-buffer "*Org Agenda*")
  (spacemacs/toggle-maximize-buffer)

  ;; Set up easy org-capture stuff
  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))

  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-destroy to close the frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))

  (use-package noflet
    :ensure t)

  (defun make-capture-frame ()
    "Create a new frame and run org-capture"
    (interactive)
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (org-capture)))
)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0bec8e74ce41664f0e3ce76c0d2cc82804089df70164419af313483651b16bd1" "e654ce0507ae5b2d7feeaef2c07354206781527941e7feb178c0a94be4a98e90" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "6ae174add87509daef7a844174f4f985592d70ea05c3d82377ad0a38a380ae80" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "3d0142352ce19c860047ad7402546944f84c270e84ae479beddbc2608268e0e5" "a40eac965142a2057269f8b2abd546b71a0e58e733c6668a62b1ad1aa7669220" "3eb2b5607b41ad8a6da75fe04d5f92a46d1b9a95a202e3f5369e2cdefb7aac5c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(evil-want-Y-yank-to-eol t)
 '(org-fontify-whole-heading-line nil)
 '(package-selected-packages
   (quote
    (gruvbox-theme-theme zonokai-theme zenburn-theme
 zen-and-art-theme underwater-theme ujelly-theme twilight-theme
 twilight-bright-theme twilight-anti-bright-theme toxi-theme
 tao-theme tangotango-theme tango-plus-theme tango-2-theme
 sunny-day-theme sublime-themes subatomic256-theme
 subatomic-theme spacegray-theme soothe-theme soft-stone-theme
 soft-morning-theme soft-charcoal-theme smyx-theme seti-theme
 reverse-theme railscasts-theme purple-haze-theme
 professional-theme planet-theme phoenix-dark-pink-theme
 phoenix-dark-mono-theme organic-green-theme
 omtose-phellack-theme oldlace-theme occidental-theme
 obsidian-theme noctilux-theme naquadah-theme mustang-theme
 monokai-theme monochrome-theme molokai-theme moe-theme
 minimal-theme material-theme majapahit-theme madhat2r-theme
 lush-theme light-soap-theme jbeans-theme jazz-theme
 ir-black-theme inkpot-theme heroku-theme hemisu-theme
 hc-zenburn-theme gruvbox-theme gruber-darker-theme
 grandshell-theme gotham-theme gandalf-theme flatui-theme
 flatland-theme farmhouse-theme espresso-theme dracula-theme
 django-theme darktooth-theme autothemer darkokai-theme
 darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme
 color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized
 clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme
 birds-of-paradise-plus-theme badwolf-theme apropospriate-theme
 anti-zenburn-theme ample-zen-theme ample-theme alect-themes
 afternoon-theme insert-shebang fish-mode company-shell
 web-beautify livid-mode skewer-mode json-mode json-snatcher
 json-reformat js2-refactor multiple-cursors js2-mode js-doc
 company-tern tern coffee-mode psci purescript-mode psc-ide
 dash-functional nix-mode helm-nixos-options
 company-nixos-options nixos-options fuzzy web-mode tagedit
 slim-mode scss-mode sass-mode pug-mode less-css-mode
 helm-css-scss haml-mode emmet-mode company-web
 web-completion-data solarized-theme erc-yt erc-view-log
 erc-social-graph erc-image erc-hl-nicks yaml-mode yapfify
 xterm-color ws-butler window-numbering which-key
 volatile-highlights vi-tilde-fringe uuidgen use-package toc-org
 spaceline smeargle shell-pop restart-emacs rainbow-delimiters
 pyvenv pytest pyenv-mode py-isort pip-requirements persp-mode
 pcre2el paradox spinner pandoc-mode ox-pandoc orgit
 org-projectile pcache org-present org-pomodoro org-plus-contrib
 org-download org-bullets open-junk-file ob-sml sml-mode neotree
 multi-term mu4e-maildirs-extension mu4e-alert ht alert log4e
 gntp move-text mmm-mode markdown-toc markdown-mode magit-gitflow
 macrostep lorem-ipsum live-py-mode linum-relative link-hint
 intero info+ indent-guide ido-vertical-mode hydra hy-mode
 hungry-delete htmlize hlint-refactor hl-todo hindent
 highlight-parentheses highlight-numbers parent-mode
 highlight-indentation hide-comnt help-fns+ helm-themes
 helm-swoop helm-pydoc helm-projectile helm-mode-manager
 helm-make projectile helm-hoogle helm-gitignore request helm-flx
 helm-descbinds helm-company helm-c-yasnippet helm-ag
 haskell-snippets google-translate golden-ratio gnuplot
 gitignore-mode gitconfig-mode gitattributes-mode git-timemachine
 git-messenger git-link gh-md flyspell-correct-helm
 flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell
 flycheck pkg-info epl flx-ido flx fill-column-indicator
 fancy-battery eyebrowse expand-region exec-path-from-shell
 evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor
 evil-surround evil-search-highlight-persist evil-numbers
 evil-nerd-commenter evil-mc evil-matchit evil-magit magit
 magit-popup git-commit with-editor evil-lisp-state
 evil-indent-plus evil-iedit-state iedit evil-exchange
 evil-escape evil-ediff evil-cleverparens smartparens paredit
 evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu
 highlight eshell-z eshell-prompt-extras esh-help elisp-slime-nav
 elfeed-web simple-httpd elfeed-org org elfeed-goodies
 ace-jump-mode noflet powerline popwin elfeed dumb-jump diminish
 define-word cython-mode company-statistics company-ghci
 company-ghc ghc haskell-mode company-cabal company-auctex
 company-anaconda company column-enforce-mode cmm-mode
 clean-aindent-mode bind-map bind-key auto-yasnippet yasnippet
 auto-highlight-symbol auto-dictionary auto-compile packed
 auctex-latexmk auctex anaconda-mode pythonic f dash s
 aggressive-indent adaptive-wrap ace-window ace-link
 ace-jump-helm-line helm avy helm-core async ac-ispell
 auto-complete popup quelpa package-build spacemacs-theme)))
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 16777215)) (:background "#282828" :foreground "#fdf4c1" :family "mononoki Nerd Font" :foundry "UKWN" :slant normal :weight normal :height 119 :width normal)) (((class color) (min-colors 255)) (:background "#262626" :foreground "#ffffaf" :family "mononoki Nerd Font" :foundry "UKWN" :slant normal :weight normal :height 119 :width normal)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "66132890ee1f884b4f8e901f0c61c5ed078809626a547dbefbb201f900d03fd8" "3d0142352ce19c860047ad7402546944f84c270e84ae479beddbc2608268e0e5" "a40eac965142a2057269f8b2abd546b71a0e58e733c6668a62b1ad1aa7669220" "3eb2b5607b41ad8a6da75fe04d5f92a46d1b9a95a202e3f5369e2cdefb7aac5c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(evil-want-Y-yank-to-eol t)
 '(org-fontify-whole-heading-line nil)
 '(package-selected-packages
   (quote
    (white-sand-theme symon string-inflection rebecca-theme password-generator org-brain impatient-mode helm-purpose window-purpose imenu-list flycheck-bashate exotica-theme evil-org evil-lion editorconfig dante zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme insert-shebang fish-mode company-shell web-beautify livid-mode skewer-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern tern coffee-mode psci purescript-mode psc-ide dash-functional nix-mode helm-nixos-options company-nixos-options nixos-options fuzzy web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data solarized-theme erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks yaml-mode yapfify xterm-color ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline smeargle shell-pop restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort pip-requirements persp-mode pcre2el paradox spinner pandoc-mode ox-pandoc orgit org-projectile pcache org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file ob-sml sml-mode neotree multi-term mu4e-maildirs-extension mu4e-alert ht alert log4e gntp move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lorem-ipsum live-py-mode linum-relative link-hint intero info+ indent-guide ido-vertical-mode hydra hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-hoogle helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag haskell-snippets google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck pkg-info epl flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-cleverparens smartparens paredit evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight eshell-z eshell-prompt-extras esh-help elisp-slime-nav elfeed-web simple-httpd elfeed-org org elfeed-goodies ace-jump-mode noflet powerline popwin elfeed dumb-jump diminish define-word cython-mode company-statistics company-ghci company-ghc ghc haskell-mode company-cabal company-auctex company-anaconda company column-enforce-mode cmm-mode clean-aindent-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex-latexmk auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build spacemacs-theme)))
 '(psc-ide-add-import-on-completion t t)
 '(psc-ide-rebuild-on-save nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#eee" :background "#011827")) (((class color) (min-colors 256)) (:foreground "#eee" :background "black"))))
 '(company-tooltip-common ((t (:foreground "#009982" :background "#000e18"))))
 '(company-tooltip-common-selection ((t (:background "#bebebe" :foreground "#E318FF" :weight bold :underline t))))
 '(org-level-1 ((((class color) (min-colors 89)) (:bold t :foreground "#5fafd7"))))
 '(org-level-2 ((((class color) (min-colors 89)) (:bold t :foreground "#5fd700"))))
 '(org-level-3 ((((class color) (min-colors 89)) (:bold t :foreground "#ff8700"))))
 '(org-level-4 ((((class color) (min-colors 89)) (:bold t :foreground "#00d7af"))))
 '(org-level-5 ((((class color) (min-colors 89)) (:bold t :foreground "#cc0000"))))
 '(org-level-6 ((((class color) (min-colors 89)) (:bold t :foreground "#b218b2"))))
 '(org-level-7 ((((class color) (min-colors 89)) (:bold t :foreground "#ff4ea3"))))
 '(org-level-8 ((((class color) (min-colors 89)) (:bold t :foreground "#ffd700")))))
)
