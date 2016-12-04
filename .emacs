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

;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Font
(set-frame-font "Monaco 14" nil t)
(add-to-list 'default-frame-alist
             '(font . "Monaco 14"))

;; GUI
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Startup
(setq inhibit-startup-screen t
      ring-bell-function 'ignore)

;; Startup with a fill frame window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Vim style scrolling
(setq scroll-step 1
      scroll-conservatively 10000
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2))

;; Tabs and Spaces
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Show line numbers
(global-linum-mode 1)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Fringe
(fringe-mode '(4 . 0))
(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))

;; Buffers
(setq uniquify-buffer-name-style 'forward
      truncate-partial-width-windows nil)

;; Enhanded dired
(require 'dired-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package seoul256-theme
  :config
  (setq seoul256-background 233)
  (load-theme 'seoul256 1)
  (custom-theme-set-faces 'seoul256
                          '(fringe ((t (:background "#252525"))))))

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
      "d" 'dired-jump
      "bk" 'kill-this-buffer
      "wm" 'maximize-window
      "w=" 'balance-windows
      "wk" 'delete-window
      "wr" 'window-configuration-to-register))
  :bind (:map evil-normal-state-map
              ("C-h" . evil-window-left)
              ("C-j" . evil-window-down)
              ("C-k" . evil-window-up)
              ("C-l" . evil-window-right)))

(use-package buffer-move
  :bind (("C-M-h" . buf-move-left)
         ("C-M-j" . buf-move-down)
         ("C-M-k" . buf-move-up)
         ("C-M-l" . buf-move-right)))

(use-package whitespace
  :diminish whitespace-mode
  :init
  (global-whitespace-mode 1)
  (setq whitespace-line-column 100
        whitespace-action '(auto-cleanup)
        whitespace-style '(face tabs empty trailing lines-tail))
  (custom-set-faces '(whitespace-trailing ((t (:background "beige"))))
                    '(whitespace-empty ((t (:background "beige"))))))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (evil-leader/set-key
    "bb" 'ivy-switch-buffer)
  :config
  :bind (:map ivy-minibuffer-map
              ("TAB"      . ivy-alt-done)
              ("<escape>" . minibuffer-keyboard-quit)
              ("C-j"      . ivy-next-line)
              ("C-k"      . ivy-previous-line)))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-cache-file (expand-file-name  "projectile.cache" my-backup-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my-backup-dir))
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
  :init
  (global-undo-tree-mode)
  (evil-leader/set-key
    "u" 'undo-tree-visualize))

(use-package company
  :diminish company-mode
  :commands company-complete
  :init
  (global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))

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

(use-package js2-mode
  :mode "\\.js\\'"
  :commands js2-mode
  :init
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2
                                              js2-strict-trailing-comma-warning nil
                                              js2-warn-about-unused-function-arguments t)))))

;; Emacs config goes here:
