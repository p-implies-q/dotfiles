(defconst my-exwm-packages
  '(

    ;; Core
    exwm
    (exwm-config :after exwm)
    (exwm-randr  :after exwm)

    ;; OS controls
    pulseaudio-control

    ))

;;; Core
;;;; exwm

(defun my-exwm/init-exwm ()
  (use-package exwm))

(defun my-exwm/post-init-exwm ()

  ;; Define workspaces
  (setq exwm-workspace-number 2)

  ;; Setup buffer-naming for different processes
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-title))))

  ;; Disable the modeline on X-windows
  (add-hook 'exwm-manage-finish-hook (lambda () (exwm-layout-toggle-mode-line)))

  ;; Use a temporary mini-buffer positioned in the bottom
  (setq exwm-workspace-minibuffer-position 'bottom)

  ;; Add some prefixes and globals to EXWM
  (push (kbd "C-c") exwm-input--global-prefix-keys)
  (push (kbd "s-s") exwm-input--global-keys)
  (push (kbd "s-m") exwm-input--global-keys)
  (mapc (lambda (pair) (exwm-input-set-key (kbd (car pair)) (cdr pair)))
        '(

          ("s-g"   . (lambda () (interactive) (lookup-key evil-normal-state-map (kbd "SPC"))))

          ("C-c i" . exwm-input-toggle-keyboard)
          ("s-a"   . run-external)
          ;; ("s-s"   . spacemacs/application-launcher-transient-state/body)
          ("s-d"   . (lambda () (interactive) (kill-buffer nil)))
          ("s-;"   . evil-ex)
          ("s-:"   . helm-M-x)
          ("s-n"   . helm-mini)

          ;; Focusing, moving, and resizing
          ("s-h"   . evil-window-left)
          ("s-j"   . evil-window-down)
          ("s-k"   . evil-window-up)
          ("s-l"   . evil-window-right)

          ("s-H"   . evil-window-move-far-left)
          ("s-J"   . evil-window-move-very-bottom)
          ("s-K"   . evil-window-move-very-top)
          ("s-L"   . evil-window-move-far-right)

          ("C-s-h" . spacemacs/shrink-window-horizontally)
          ("C-s-j" . spacemacs/shrink-window)
          ("C-s-k" . spacemacs/enlarge-window)
          ("C-s-l" . spacemacs/enlarge-window-horizontally)

          ;; Multimedia keys
          ("<XF86AudioRaiseVolume>" . (lambda () (interactive)
                                        (let ((cmd "pulsevolume plus"))
                                          (start-process-shell-command cmd nil cmd))))
          ("<XF86AudioLowerVolume>" . (lambda () (interactive)
                                        (let ((cmd "pulsevolume minus"))
                                          (start-process-shell-command cmd nil cmd))))
          ("<XF86AudioMute>"        . (lambda () (interactive)
                                        (let ((cmd "pulsevolume mute"))
                                          (start-process-shell-command cmd nil cmd))))
          ("<XF86MonBrightnessUp>"  . (lambda () (interactive)
                                        (let ((cmd "sudo brightness +33%"))
                                          (start-process-shell-command cmd nil cmd))))
          ("<XF86MonBrightnessDown>". (lambda () (interactive)
                                        (let ((cmd "sudo brightness -33%"))
                                          (start-process-shell-command cmd nil cmd))))

          ))
  (exwm-input-set-key (kbd "s-1") (lambda ()
                                    (interactive)
                                    (exwm-workspace-switch-create 1)))
  (exwm-input-set-key (kbd "s-2") (lambda ()
                                    (interactive)
                                    (exwm-workspace-switch-create 2)))

  ;; Application launcher
  (exwm-input-set-key (kbd "s-t") (lambda (cmd)
                                    (interactive (list (read-shell-command "$ ")))
                                    (start-process-shell-command cmd nil cmd)))

  ;; The function that starts exwm
  (defun start-exwm ()
    (interactive)
    (shell-command "setxkbmap us -variant colemak -option ctrl:nocaps")
    (spacemacs/toggle-mode-line)
    (exwm-enable))
  )

;;;; exwm-randr
(defun my-exwm/post-init-exwm-randr ()

  ;; Enable xrandr for exwm
  (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  (exwm-randr-enable)

  ;; Immediately call xrandr on detected monitor (un)plugging
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  )

;;;; exwm-config
(defun my-exwm/post-init-exwm-config ()
  ;; TODO figure out what this does
  (exwm-config-misc))


(defun my-exwm/post-init-pulseaudio-control
    (setq pulseaudio-control-volume-step 2))
