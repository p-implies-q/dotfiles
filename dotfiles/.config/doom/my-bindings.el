;;; ~/opt/dotfiles/dotfiles/.config/doom/+bindings.el -*- lexical-binding: t; -*-


(map!
  (:leader
    :desc "helm-mini" :nv "a" #'helm-mini
    :desc "mu4e"      :nv "m" #'mu4e

    (:desc "+search" :prefix "/"
      :desc "swiper" :n "/" #'swiper-helm)
    )
  :nmvo "C-a" #'helm-mini)

