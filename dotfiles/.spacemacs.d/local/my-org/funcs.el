(defun my-org-file (x) (cdr (assoc x my-org-files-alist)))

(defun make-capture-frame (&optional capture-url)
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")
                (width . 120)
                (height . 15)))
  (select-frame-by-name "capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (if capture-url (org-protocol-capture capture-url) (org-capture)))
