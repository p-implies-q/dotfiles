
;;; Brightness controls
(defun inc-brightness ()
  (interactive)
  (shell-command "xbacklight -inc 10"))

(defun dec-brightness ()
  (interactive)
  (shell-command "xbacklight -dec 10"))

;;; Starting external processes
(defun run-external (cmd)
  "Run external shell command using helm-completion"
  (interactive (list (helm-comp-read "$: " common-commands)))
  (shell-command cmd))

;;; Adding a global key
(defun exwm-global-set-keys (&rest BINDINGS)
  "Add exwm keys, add them to global-keys explicitly"
  (-each (-partition 2 BINDINGS)
        (-lambda ((key cmd))
          (progn
            (exwm-input-set-key (kbd key) cmd)
            (push (kbd key) exwm-input--global-keys)))))

;;; Start exwm
(defun start-exwm ()
  (interactive)
  (shell-command "setxkbmap us -variant colemak -option ctrl:nocaps")
  (spacemacs/toggle-mode-line)
  (exwm-enable)
  (switch-to-buffer "*spacemacs*")
  )
