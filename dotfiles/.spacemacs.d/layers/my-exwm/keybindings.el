
;;; Push "s-m" to global input key
(push dotspacemacs-emacs-leader-key exwm-input--global-keys)

;;; Set the global exwm-control keys
(exwm-global-set-keys
 '(
    "C-c i"  exwm-input-toggle-keyboard
    "s-a"    run-external
    "s-d"    (lambda () (interactive) (kill-buffer nil))
    "s-;"    evil-ex
    "s-:"    helm-M-x
    "s-n"    helm-mini
    "s-t"    run-external

    ;; Focusing, moving, and resizing
    "s-h"    evil-window-left
    "s-j"    evil-window-down
    "s-k"    evil-window-up
    "s-l"    evil-window-right

    "s-H"    evil-window-move-far-left
    "s-J"    evil-window-move-very-bottom
    "s-K"    evil-window-move-very-top
    "s-L"    evil-window-move-far-right

    "C-s-h"  spacemacs/shrink-window-horizontally
    "C-s-j"  spacemacs/shrink-window
    "C-s-k"  spacemacs/enlarge-window
    "C-s-l"  spacemacs/enlarge-window-horizontally

    ;; Multimedia keys
    "<XF86AudioRaiseVolume>"  pulseaudio-control-increase-volume
    "<XF86AudioLowerVolume>"  pulseaudio-control-decrease-volume
    "<XF86AudioMute>"         pulseaudio-contorl-toggle-current-sink-mute
    "<XF86MonBrightnessUp>"   inc-brightness
    "<XF86MonBrightnessDown>" inc-brightness
    ))


;;; Set some workspace keys
(exwm-input-set-key (kbd "s-1") (lambda ()
                                  (interactive)
                                  (exwm-workspace-switch-create 1)))
(exwm-input-set-key (kbd "s-2") (lambda ()
                                  (interactive)
                                  (exwm-workspace-switch-create 2)))
