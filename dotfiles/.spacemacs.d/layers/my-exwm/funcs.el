
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

;;; Start exwm
