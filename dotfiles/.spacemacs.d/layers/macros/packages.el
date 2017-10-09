;;; Macros Layer

(defconst macros-packages
      '(
        dash
        dash-functional
        ))

(defun macros/init-dash ()
  (use-package dash))

(defun macros/post-init-dash ()
  (dash-enable-font-lock))

(defun macros/init-dash-functional ()
  (use-package dash-functional))
