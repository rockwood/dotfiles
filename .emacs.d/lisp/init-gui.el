;; Use one folder for all save/history/cache files
(defconst rock/backup-dir (expand-file-name "backups" user-emacs-directory))
(unless (file-exists-p rock/backup-dir)
  (make-directory rock/backup-dir))

;; Backup and lockfiles
(setq create-lockfiles nil
      backup-directory-alist `((".*" . ,rock/backup-dir))
      auto-save-file-name-transforms `((".*" ,rock/backup-dir t)))

;; Encoding
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

;; Autoload buffers when changed
(global-auto-revert-mode t)

;; Only require `y/n` rather than `yes/no`
(defalias 'yes-or-no-p 'y-or-n-p)

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
      scroll-conservatively 101
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2))

;; Tabs and Spaces
(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 100)

(setq display-line-numbers-type 'relative)

;; Show line numbers
(global-display-line-numbers-mode 1)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Turn off fringes
(set-fringe-mode 0)

;; Buffers
(setq uniquify-buffer-name-style 'forward
      truncate-partial-width-windows nil)

;; Disable the default version control process (we're using Magit)
(setq vc-handled-backends nil)

;; Remap help since we use vim-style window navigation
(global-set-key "\C-ch" help-map)

; Merge system's and Emacs' clipboard
(setq select-enable-clipboard t
      save-interprogram-paste-before-kill t)

;; Make sure files end in a newline
(setq require-final-newline t)

;; Garbage-collect on focus-out, Emacs should feel snappier.
(add-hook 'focus-out-hook #'garbage-collect)

;; Highlight the current line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :foreground nil :background "#333")

;; Optimize lsp performance
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

;; Mac specific config
(let ((is-mac (string-equal system-type "darwin")))
  (when is-mac

    ;; Fancy titlebar
    (add-to-list 'default-frame-alist '(ns-appearance . dark))

    ;; Modifier keys
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super
          mac-control-modifier 'control
          mac-function-modifier 'hyper)

    ;; Don't make new frames when opening a new file with Emacs
    (setq ns-pop-up-frames nil)

    ;; Disable default fullscreen style
    (setq ns-use-native-fullscreen nil)

    ;; Don't use mac scrolling
    (setq mac-mouse-wheel-smooth-scroll nil)

    ;; Mac-style Keybindings
    (global-set-key (kbd "s-=") 'text-scale-increase)
    (global-set-key (kbd "s--") 'text-scale-decrease)))

(provide 'init-gui)
