;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "my-bindings")
(load! "my-mail")

(setq projectile-project-search-path '("~/prj/" "~/opt"))
(setq doom-font (font-spec :family "mononoki Nerd Font" :size 16))
(setq evil-snipe-scope 'whole-visible)


(setq haskell-process-type 'cabal-new-repl)
(setq haskell-process-args-cabal-new-repl
     '("-ferror-spans" "--verbose=0"))
(setq haskell-interactive-popup-errors nil)
(setq haskell-process-log t)
