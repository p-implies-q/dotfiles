;;; Macros Layer

(setq macros-packages
      '(
        dash-functional
        ))

(defun macros/init-dash-functional ()
  (use-package dash-functional))
