;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

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

;; Autoload buffers when changed
(global-auto-revert-mode t)

;; Only require `y/n` rather than `yes/no`
(defalias 'yes-or-no-p 'y-or-n-p)

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
(fringe-mode '(4 . 4))
(set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background))

;; Buffers
(setq uniquify-buffer-name-style 'forward
      truncate-partial-width-windows nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rock/upgrade-all-packages (&optional no-fetch)
  "Upgrade all packages. No questions asked."

  (interactive "P")
  (let ((package-menu-async nil))
    (save-window-excursion
      (package-list-packages no-fetch)
      (package-menu-mark-upgrades)
      (package-menu-execute 'noquery))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package seoul256-theme
  :config
  (setq seoul256-background 233)
  (load-theme 'seoul256 1)
  (custom-theme-set-faces 'seoul256
                          '(fringe ((t (:background "#252525"))))))

(use-package evil
  :init
  (evil-mode 1)

  ;; My leader key map
  (bind-keys :map evil-normal-state-map :prefix-map rock-leader :prefix "SPC")

  :bind (:map rock-leader
              ("ss" . flyspell-mode)
              ("sn" . flyspell-goto-next-error)
              ("sc" . ispell-word)
              ("bk" . kill-this-buffer)
              ("bm" . buffer-menu)
              ("wm" . maximize-window)
              ("w=" . balance-windows)
              ("wk" . delete-window)
              ("wr" . window-configuration-to-register))

  :bind (:map evil-normal-state-map
              ("C-h" . evil-window-left)
              ("C-j" . evil-window-down)
              ("C-k" . evil-window-up)
              ("C-l" . evil-window-right))

  :config
  (use-package evil-commentary
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode))

  (use-package evil-surround
    :init
    (global-evil-surround-mode 1)))

(use-package buffer-move
  :bind (("C-S-h" . buf-move-left)
         ("C-S-j" . buf-move-down)
         ("C-S-k" . buf-move-up)
         ("C-S-l" . buf-move-right)))

(use-package whitespace
  :diminish global-whitespace-mode
  :init
  (global-whitespace-mode 1)
  (setq whitespace-line-column 100
        whitespace-action '(auto-cleanup)
        whitespace-style '(face tabs empty trailing lines-tail))
  (custom-set-faces '(whitespace-trailing ((t (:background "Grey16"))))
                    '(whitespace-empty ((t (:background "Grey16"))))))

(use-package dired-x
  :ensure nil
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map rock-leader
              ("dd" . dired-jump)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (add-hook 'minibuffer-setup-hook (lambda () (linum-mode 0)))
  (setq ivy-format-function 'ivy-format-function-arrow
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))

  :bind (:map rock-leader
              ("bb" . ivy-switch-buffer))

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
  :config
  (projectile-global-mode)
  :bind (:map rock-leader
              ("pt" . projectile-toggle-between-implementation-and-test)
              ("pT" . projectile-find-test-file)
              ("pP" . projectile-test-project)
              ("pk" . projectile-kill-buffers)
              ("pr" . projectile-replace)
              ("pk" . projectile-kill-buffers)
              ("pj" . projectile-find-tag)
              ("pR" . projectile-regenerate-tags)
              ("pi" . projectile-invalidate-cache)))

(use-package counsel-projectile
  :bind (:map rock-leader
              ("pp" . counsel-projectile-switch-project)
              ("pb" . counsel-projectile-switch-to-buffer)
              ("pd" . counsel-projectile-find-dir)
              ("pf" . counsel-projectile-find-file)
              ("pa" . counsel-projectile-ag)))

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
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :bind (:map rock-leader
              ("u" . undo-tree-visualize)))

(use-package company
  :diminish company-mode
  :commands company-complete
  :init
  (global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))

(use-package linum-relative
  :config
  (progn
    (setq linum-relative-current-symbol ""))
  :init
  :bind (:map rock-leader
              ("tl" . linum-relative-toggle)))

(use-package magit
  :config
  (use-package evil-magit)
  :init
  (add-hook 'git-commit-setup-hook (lambda () (linum-mode 0)))
  :bind (:map rock-leader
              ("mm" . magit-status)
              ("ml" . magit-log-current)
              ("mx" . magit-blame)
              ("mbb" . magit-branch-popup)
              ("mbc" . magit-branch-and-checkout)
              ("mpp" . magit-pull-popup)))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package dash-at-point
  :bind (:map rock-leader
              ("hh" . dash-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-mode
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package alchemist
  :commands alchemist-mode
  :config
  (setq alchemist-iex-program-name "~/.asdf/shims/iex")
  :bind (:map rock-leader
              ("att" . alchemist-mix-test)
              ("atb" . alchemist-mix-test-this-buffer)
              ("atp" . alchemist-mix-test-at-point)
              ("atr" . alchemist-mix-rerun-last-test)
              ("ats" . alchemist-mix-test-stale)
              ("agt" . alchemist-project-toggle-file-and-tests)
              ("agd" . alchemist-goto-definition-at-point)
              ("agg" . alchemist-goto-jump-back)
              ("amm" . alchemist-mix)
              ("amc" . alchemist-mix-compile)
              ("amr" . alchemist-mix-run)
              ("aii" . alchemist-iex-project-run)
              ("ail" . alchemist-iex-send-current-line)
              ("aib" . alchemist-iex-compile-this-buffer)
              ("ais" . alchemist-iex-send-region)
              ("ael" . alchemist-eval-current-line)
              ("aer" . alchemist-eval-region)))

(use-package markdown-mode
  :init (setq markdown-command "multimarkdown"))

(use-package web-mode
  :mode (("\\.eex?\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :init
  (defun my-web-mode-hook ()
    (setq indent-tabs-mode nil
          web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-style-padding 2
          web-mode-script-padding 2
          web-mode-enable-current-element-highlight t))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

(use-package js2-mode
  :mode "\\.js\\'"
  :commands js2-mode
  :init
  (defun my-js2-mode-hook ()
    (setq js2-basic-offset 2
          js2-strict-trailing-comma-warning nil
          js2-warn-about-unused-function-arguments t))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(use-package css-mode
  :commands css-mode
  :init
  (setq-default css-indent-offset 2))

(use-package rainbow-mode
  :diminish rainbow-mode
  :init
  (dolist (hooks '(css-mode-hook html-mode-hook scss-mode-hook))
    (add-hook hooks 'rainbow-mode)))

(use-package fish-mode)

;; Emacs config goes here:
