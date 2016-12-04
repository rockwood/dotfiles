;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
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
(defconst my-backup-dir (expand-file-name "backups" user-emacs-directory))
(unless (file-exists-p my-backup-dir)
  (make-directory my-backup-dir))

;; Backup and lockfiles
(setq create-lockfiles nil
      backup-directory-alist `((".*" . ,my-backup-dir))
      auto-save-file-name-transforms `((".*" ,my-backup-dir t)))

;; Set keys for MacOS
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-control-modifier 'control
      ns-function-modifier 'hyper)


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

;; Tabs
(setq-default
 indent-tabs-mode nil
 tab-width 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package monokai-theme
  :config (load-theme 'monokai 1))

(use-package spaceline-config
  :ensure spaceline
  :config (spaceline-spacemacs-theme)
  :init (setq powerline-default-separator 'bar))

(use-package evil
  :init
  (evil-mode 1)
  :config
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "s" 'save-buffer
      "k" 'kill-this-buffer
      "d" 'delete-window))
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

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (evil-leader/set-key
    "b" 'ivy-switch-buffer)
  :config
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
  (evil-leader/set-key
    "pt" 'projectile-toggle-between-implementation-and-test
    "pT" 'projectile-find-test-file
    "pP" 'projectile-test-project
    "pk" 'projectile-kill-buffers
    "pr" 'projectile-replace
    "pk" 'projectile-kill-buffers
    "pj" 'projectile-find-tag
    "pR" 'projectile-regenerate-tags
    "pi" 'projectile-invalidate-cache)
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :init
  (evil-leader/set-key
    "pp" 'counsel-projectile-switch-project
    "pb" 'counsel-projectile-switch-to-buffer
    "pd" 'counsel-projectile-find-dir
    "pf" 'counsel-projectile-find-file
    "pa" 'counsel-projectile-ag))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init
  (drag-stuff-global-mode 1)
  :bind (:map drag-stuff-mode-map
              ("s-k" . drag-stuff-up)
              ("s-j" . drag-stuff-down)
              ("s-h" . drag-stuff-left)
              ("s-l" . drag-stuff-right)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  (evil-leader/set-key
    "u" 'undo-tree-visualize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode)

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2))

;; Emacs config goes here:
