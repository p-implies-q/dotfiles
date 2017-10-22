(defconst my-exwm-packages
  '(
    ;; EPA
    epa

    ;; Core
    exwm

    ;; OS controls
    pulseaudio-control

    ))

(defun my-exwm/init-epa ()
  (use-package epa))

(defun my-exwm/post-init-epa ()
  (setq epa-pinentry-mode 'loopback))

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

  ;; Enable xrandr for exwm
  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  (exwm-randr-enable)

  ;; Immediately call xrandr on detected monitor (un)plugging
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))

  )




(defun my-exwm/post-init-pulseaudio-control
    (setq pulseaudio-control-volume-step 2))
