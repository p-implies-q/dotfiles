
(defun config/scroll-to-center-advice (&rest args)
  "Scoll line to center, for advising functions."
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p))
            dired-dotfiles-show-p)
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          ;; (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer)
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun evil-global-set-keys (STATES &rest BINDINGS)
  "'Evil-global-set-key' for all STATES with many BINDINGS."
  (--each STATES
	  (-each (-partition 2 BINDINGS)
      (-lambda ((key cmd))
			  (evil-global-set-key it key cmd)))))
