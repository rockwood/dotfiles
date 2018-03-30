(defun rock/upgrade-all-packages (&optional no-fetch)
  "Upgrade all packages. No questions asked."

  (interactive "P")
  (let ((package-menu-async nil))
    (save-window-excursion
      (package-list-packages no-fetch)
      (package-menu-mark-upgrades)
      (package-menu-execute 'noquery))))

(defun eshell/clear()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(provide 'init-functions)
