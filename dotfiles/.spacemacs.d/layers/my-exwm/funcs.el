;; Brightness controls

(defun quiet-command (cmd)
  (start-process-shell-command cmd nil cmd))

(defun inc-brightness ()
  (interactive)
  (quiet-command "xbacklight -inc 10"))

(defun dec-brightness ()
  (interactive)
  (quiet-command "xbacklight -dec 5"))

(defun inc-volume ()
  (interactive)
  (quiet-command "pulsevolume plus"))

(defun dec-volume ()
  (interactive)
  (quiet-command "pulsevolume minus"))

(defun toggle-mute ()
  (interactive)
  (quiet-command "pulsevolume mute"))

;;; Starting external command

(defvar my-exwm-common-commands
  '("setxkbmap us -variant colemak -option ctrl:nocaps"
    "setxkbmap us"
    "sudo pkill lightdm"
    "sudo nixos-rebuild switch")
  "Commands that are often run from the command-line")

(defun run-command (cmd)
  "Run external shell command using helm-completion"
  (interactive (list (helm-comp-read "$: " my-exwm-common-commands)))
  (push cmd my-exwm-common-commands)
  (shell-command cmd))

(defvar my-exwm-common-programs
  '("chromium"
    "chromium https://www.reddit.com"
    "chromium https://www.youtube.com"
    "obs"
    "vlc"
    "fontmatrix"
    "pavucontrol")
  "Programs that I launch often"
  )
;;; Start a program
(defun run-program (cmd)
  "Start a program"
  (interactive (list (helm-comp-read "$: " my-exwm-common-programs)))
  (push cmd my-exwm-common-programs)
  (start-process-shell-command cmd nil cmd))


;;; Adding a global key
(defun exwm-global-set-keys (BINDINGS)
  "Set up keybindings for exwm"
  (-each (-partition 2 BINDINGS)
        (-lambda ((key cmd))
          (progn
            (exwm-input-set-key (kbd key) cmd)))))

;;; Start exwm
(defun start-exwm ()
  (interactive)
  (spacemacs/toggle-mode-line)
  (exwm-enable)
  (switch-to-buffer "*spacemacs*")
  (quiet-command "setxkbmap us -variant colemak -option ctrl:nocaps")
  (quiet-command "xcape")
  )
