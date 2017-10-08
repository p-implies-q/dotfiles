;;; Dotspacemacs

;; -- David Janssen's Spacemacs Configuration --
;; -- Contact: janssen.dhj@gmail.com

(setq is-linuxp (eq system-type 'gnu/linux))
(defun os-path (x) (if is-linuxp x (expand-file-name x "c:")))

(defun dotspacemacs/init ()
  "Spacemacs core settings."
  (message "hello")
  (dotspacemacs/init/coding)
  (dotspacemacs/init/display)
  (dotspacemacs/init/evil)
  (dotspacemacs/init/keys)
  (dotspacemacs/init/layouts)
  (dotspacemacs/init/misc)
  (dotspacemacs/init/packages)
  (dotspacemacs/init/startup)
  )

(defun dotspacemacs/layers ()
  "Spacemacs layers declaration and package configurations."
  (dotspacemacs/layers/config)
  (dotspacemacs/layers/packages))

;;; Layers

(defvar dotspacemacs/layers/local
  '((macros :location local)   ; All local layers depend on this
    (config :location local)
   )
  "Local layers in ~/.spacemacs.d/layers")

(defvar dotspacemacs/layers/core
  '(better-defaults
    git
    syntax-checking
    (auto-completion
        :variables
        auto-completion-complete-with-key-sequence-delay 0.01
        auto-completion-return-key-behavior              'complete
        auto-completion-tab-key-behavior                 'complete
        auto-completion-enable-snippets-in-popup         t)
    (org
        :variables
        org-want-todo-bindings t)
    (shell
        :variables
        shell-default-shell 'eshell)
    (version-control
        :variables
        version-control-global-margin t
        version-control-diff-tool 'git-gutter+)
    )
  "Core Spacemacs configuration layers")

(defvar dotspacemacs/layers/langs
  '(emacs-lisp
    html
    javascript
    latex
    markdown
    pandoc
    purescript
    nixos
    (haskell
        :variables
        haskell-completion-backend 'intero
        haskell-enable-ghc-mod-support nil
        haskell-process-type           'stack-ghci)
    (python
        :variables
        python-sort-imports-on-save t
        python-test-runner 'pytest))
  "Language specific layers")

(defvar dotspacemacs/layers/extra
  '(ranger
    search-engine
    (mu4e
        :variables
        mu4e-installation-path "/run/current-system/sw/share/emacs/site-lisp")
    (syntax-checking
        :variables
        syntax-checking-enable-by-default t)
    )
  "Miscellaneous layers")

(defun dotspacemacs/layers/config ()
  (setq-default
   dotspacemacs-distribution               'spacemacs
   dotspacemacs-enable-lazy-installation   'unused
   dotspacemacs-ask-for-lazy-installation  t

   dotspacemacs-configuration-layer-path   `(,(os-path "~/.spacemacs.d/layers"))
   dotspacemacs-configuration-layers       (append dotspacemacs/layers/core
                                                   dotspacemacs/layers/langs
                                                   dotspacemacs/layers/extra
                                                   dotspacemacs/layers/local)
   ))

(defun dotspacemacs/layers/packages ()
  (setq-default
   dotspacemacs-additional-packages     '(exwm)
   dotspacemacs-delete-orphan-packages  t
   dotspacemacs-excluded-packages       '(exec-path-from-shell)
   dotspacemacs-frozen-packages         '()
   dotspacemacs-install-packages        'used-but-keep-unused
   ))

;;; Spacemacs init

(defun dotspacemacs/init/coding ()
  (setq-default
   dotspacemacs-search-tools              '("ag" "pt" "ack" "grep")
   dotspacemacs-smooth-scrolling          t
   dotspacemacs-folding-method            'evil
   dotspacemacs-smartparens-strict-mode   t
   dotspacemacs-smart-closing-parenthesis t
   dotspacemacs-highlight-delimiters      'all
   dotspacemacs-line-numbers              nil
   dotspacemacs-whitespace-cleanup        'trailing
   ))

(defun dotspacemacs/init/display ()
  (setq-default
   dotspacemacs-themes                     '(
                                             ;; spacemacs-dark
                                             material
                                             material-light
                                             base16-ocean
                                             leuven
                                             )
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
   ))

(defun dotspacemacs/init/evil ()
  (setq-default
   dotspacemacs-editing-style                       'vim
   dotspacemacs-colorize-cursor-according-to-state  t
   dotspacemacs-remap-Y-to-y$                       t
   dotspacemacs-retain-visual-state-on-shift        t
   dotspacemacs-visual-line-move-text               nil
   dotspacemacs-ex-substitute-global                nil
   dotspacemacs-enable-paste-transient-state        nil
   dotspacemacs-show-transient-state-title          t
   dotspacemacs-show-transient-state-color-guide    t
   ))

(defun dotspacemacs/init/keys ()
  (setq-default
   dotspacemacs-leader-key                  "SPC"
   dotspacemacs-emacs-command-key           "SPC"
   dotspacemacs-ex-command-key              ":"
   dotspacemacs-emacs-leader-key            "s-m"
   dotspacemacs-major-mode-leader-key       ","
   dotspacemacs-major-mode-emacs-leader-key "s-M"
   dotspacemacs-which-key-delay             0.4
   dotspacemacs-which-key-position          'bottom
   dotspacemacs-distinguish-gui-tab         nil
   ))

(defun dotspacemacs/init/layouts ()
  (setq-default
   dotspacemacs-scratch-mode                        'org-mode
   dotspacemacs-default-layout-name                 "Default"
   dotspacemacs-display-default-layout              nil
   dotspacemacs-auto-resume-layouts                 t
   dotspacemacs-auto-generate-layout-names          t
   dotspacemacs-switch-to-buffer-prefers-purpose    nil
   ))

(defun dotspacemacs/init/misc ()
  (setq-default
   dotspacemacs-large-file-size          1
   dotspacemacs-auto-save-file-location  'cache
   dotspacemacs-max-rollback-slots       5
   dotspacemacs-persistent-server        nil
   dotspacemacs-helm-resize              nil
   dotspacemacs-helm-no-header           nil
   dotspacemacs-helm-position            'bottom
   custom-file                           "~/.spacemacs.d/custom.el"
   ))

(defun dotspacemacs/init/packages ()
  (setq-default
   dotspacemacs-default-package-repository  nil
   dotspacemacs-elpa-https                  t
   dotspacemacs-elpa-timout                 5
   dotspacemacs-check-for-update            nil
   dotspacemacs-elpa-subdirectory           nil
   ))

(defun dotspacemacs/init/startup ()
  (setq-default
   dotspacemacs-verbose-loading            nil
   dotspacemacs-startup-banner             'official
   dotspacemacs-startup-lists              '()
   dotspacemacs-startup-buffer-responsive  t
   dotspacemacs-loading-progress-bar       t
   ))

;;; Spacemacs user-config
(defun dotspacemacs/user-config/toggles ()
  "Spacemacs toggles not intended for layers"
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (spacemacs/toggle-mode-line-minor-modes-off)
  (spacemacs/toggle-aggressive-indent-globally-on)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode-enable)
  (fringe-mode '(0 . 8)))
