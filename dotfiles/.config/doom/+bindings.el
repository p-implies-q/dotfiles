;;; ~/opt/dotfiles/dotfiles/.config/doom/+bindings.el -*- lexical-binding: t; -*-


(map!
  (:leader
    (:desc "helm-mini" :nv "a" #'helm-mini)
    (:desc "mu4e"      :nv "m" #'mu4e)))

(map!
  :nv "C-a" #'helm-mini)

