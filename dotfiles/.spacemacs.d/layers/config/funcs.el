
(defun config/scroll-to-center-advice (&rest args)
  "Scoll line to center, for advising functions."
  (evil-scroll-line-to-center (line-number-at-pos)))
