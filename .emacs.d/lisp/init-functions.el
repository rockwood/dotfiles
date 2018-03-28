(defun rock/upgrade-all-packages (&optional no-fetch)
  "Upgrade all packages. No questions asked."

  (interactive "P")
  (let ((package-menu-async nil))
    (save-window-excursion
      (package-list-packages no-fetch)
      (package-menu-mark-upgrades)
      (package-menu-execute 'noquery))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))

(provide 'init-functions)
