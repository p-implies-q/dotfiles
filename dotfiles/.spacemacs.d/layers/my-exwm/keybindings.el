
;;; Push "s-m" to global input key
(push (kbd dotspacemacs-emacs-leader-key) exwm-input--global-keys)

;;; Set the global exwm-control keys
(exwm-global-set-keys
 '(
   ;; Buffer navigation
   "s-a"    spacemacs/helm-buffers-smart-do-search
   "s-r"    helm-filtered-bookmarks
   "s-s"    helm-find-files
   "s-t"    helm-mini
   "s-d"    (lambda () (interactive) (kill-buffer nil))

   ;; Launch things
   "s-O"    evil-ex
   "s-o"    helm-M-x
   "s-u"    run-program
   "s-y"    run-command
   "C-c i"  exwm-input-toggle-keyboard

   ;; Focusing, moving, and resizing
   "s-h"    evil-window-left
   "s-j"    evil-window-down
   "s-k"    evil-window-up
   "s-l"    evil-window-right

   "s-H"    evil-window-move-far-left
   "s-J"    evil-window-move-very-bottom
   "s-K"    evil-window-move-very-top
   "s-L"    evil-window-move-far-right

   "C-s-h"  (lambda () (interactive) (spacemacs/shrink-window-horizontally 3))
   "s-f"    (lambda () (interactive) (spacemacs/shrink-window-horizontally 3))
   "C-s-j"  spacemacs/shrink-window
   "C-s-k"  spacemacs/enlarge-window
   "C-s-l"  (lambda () (interactive) (spacemacs/enlarge-window-horizontally 3))
   "s-p"    (lambda () (interactive) (spacemacs/enlarge-window-horizontally 3))

   ;; Multimedia keys
   "<XF86AudioRaiseVolume>"  inc-volume
   "<XF86AudioLowerVolume>"  dec-volume
   "<XF86AudioMute>"         toggle-mute
   "<XF86MonBrightnessUp>"   inc-brightness
   "<XF86MonBrightnessDown>" dec-brightness
   ))


;;; Set some workspace keys
(exwm-input-set-key (kbd "s-1") (lambda ()
                                  (interactive)
                                  (exwm-workspace-switch-create 1)))
(exwm-input-set-key (kbd "s-2") (lambda ()
                                  (interactive)
                                  (exwm-workspace-switch-create 2)))
