;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+email")

(setq projectile-project-search-path '("~/prj/" "~/opt"))
(setq doom-font (font-spec :family "mononoki Nerd Font" :size 16))
(setq evil-snipe-scope 'whole-visible)
