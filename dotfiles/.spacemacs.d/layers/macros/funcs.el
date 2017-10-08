(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

;; (defun evil-global-set-keys (STATES &rest BINDINGS)
;;   "'Evil-global-set-key' for all STATES with many BINDINGS."
;;   (--each STATES
;; 	  (-each (-partition 2 BINDINGS)
;; 		 (-lambda ((key cmd))
;; 			  (evil-global-set-key it key cmd)))))
