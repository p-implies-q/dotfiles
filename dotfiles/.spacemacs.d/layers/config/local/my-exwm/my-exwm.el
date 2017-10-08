(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)

(provide 'my-exwm)

(defun start-exwm ()
  "Lightdm is configured to call this function on startup of exwm"
  (interactive)

  ;; Setup multi-head support
  (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
                "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
  (exwm-randr-enable)

  ;; Fix problems with ido
  (exwm-config-ido)

  (setq exwm-workspace-number 2)

  ;; Just Copy-Pasted from the example, used to rename windows
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
  (defvar common-commands
    '("setxkbmap us -variant colemak -option ctrl:nocaps"
      "setxkbmap us"
      "sudo pkill lightdm"
      "sudo nixos-rebuild switch")

    "A list of commands that I execute often")

  (defun run-external (cmd)
    "Run external shell command using helm-completion"
    (interactive (list (helm-comp-read "$: " common-commands)))
    (shell-command cmd))


  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (exwm-layout-toggle-mode-line)))
  ;; Define some useful global keybindings
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

  (defun mod-volume (amount)
    "Change the system volume using external shellscript"
    (let ((cmd "pulsevolume plus")))
    (start-process-shell-command))
  ;; (defmacro expand-run (cmd)
  ;;   `(defun )
  ;;     ,(concat "\"" cmd  "\"")
  ;;     `(start-process-shell-command ,cmd nil ,cmd))

  ;; (expand-run "chromium")

  (push (kbd "C-c") exwm-input--global-prefix-keys)
  (push (kbd "s-s") exwm-input--global-keys)
  (push (kbd "s-m") exwm-input--global-keys)
  ;; (exwm-input-set-key (kbd "s-s c") (expand-run "chromium"))

  ;; (push (kbd "SPC") exwm-input-prefix-keys)
  ;; (push (kbd "SPC") exwm-input--global-keys)
  ;; Define workspace-switching keybingings
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

  (exwm-config-misc)
  (shell-command "setxkbmap us -variant colemak -option ctrl:nocaps")
  (spacemacs/toggle-mode-line)
  (setq exwm-workspace-minibuffer-position 'bottom)
  (exwm-enable))
