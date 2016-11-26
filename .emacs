;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

;; Downloaded archive descriptions.
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package - https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Default to `:ensure t` to always install packages at startup.
(setq use-package-always-ensure t)

;; Use one folder for all save/history/cache files
(defconst my-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)

(scroll-bar-mode -1)

(global-linum-mode 1)

(set-frame-font "Monaco 14" nil t)

(add-to-list 'default-frame-alist
             '(font . "Monaco 14"))

;; Disable the bell
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package monokai-theme
  :config (load-theme 'monokai t))

(use-package spaceline-config
  :ensure spaceline
  :config (spaceline-spacemacs-theme)
  :init (setq powerline-default-separator 'bar))

(use-package evil
  :init
  (evil-mode t)
  :config
  (use-package evil-leader
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "s" 'save-buffer
      "k" 'kill-this-buffer
      "d" 'delete-window)
    (global-evil-leader-mode))
  :bind (:map evil-normal-state-map
              ("C-h" . evil-window-left)
              ("C-j" . evil-window-down)
              ("C-k" . evil-window-up)
              ("C-l" . evil-window-right)))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (setq whitespace-line-column 100)
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package swiper)

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (evil-leader/set-key "b" 'ivy-switch-buffer)
  :config
  (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
	 ("TAB"      . ivy-alt-done)
	 ("<escape>" . minibuffer-keyboard-quit)
	 ("C-j"      . ivy-next-line)
	 ("C-k"      . ivy-previous-line)))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-cache-file (expand-file-name  "projectile.cache" my-savefile-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-savefile-dir))
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :init
  (defun counsel-projectile-ag ()
    (interactive)
    (counsel-ag nil (projectile-project-root)))
  (evil-leader/set-key
    "pp" 'counsel-projectile-switch-project
    "pf" 'counsel-projectile-find-file
    "pa" 'counsel-projectile-ag))

;; Emacs config goes here:
