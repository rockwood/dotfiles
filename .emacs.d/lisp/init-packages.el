(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package evil
  :init
  (evil-mode 1)

  ;; Spacemacs style leader bindings
  (bind-keys :map evil-normal-state-map :prefix "SPC" :prefix-map rock-leader)
  (bind-keys :map rock-leader :prefix "s" :prefix-map rock/spelling)
  (bind-keys :map rock-leader :prefix "b" :prefix-map rock/buffers)
  (bind-keys :map rock-leader :prefix "w" :prefix-map rock/windows)
  (bind-keys :map rock-leader :prefix "d" :prefix-map rock/directories)
  (bind-keys :map rock-leader :prefix "p" :prefix-map rock/projects)
  (bind-keys :map rock-leader :prefix "a" :prefix-map rock/alchemist)
  (bind-keys :map rock-leader :prefix "m" :prefix-map rock/magit)
  (bind-keys :map rock-leader :prefix "h" :prefix-map rock/help)
  (bind-keys :map rock-leader :prefix "t" :prefix-map rock/toggles)
  (bind-keys :map rock-leader :prefix "g" :prefix-map rock/goto)
  (bind-keys :map rock-leader :prefix "c" :prefix-map rock/commands)

  :bind (:map rock-leader
              ("1" . select-window-1)
              ("2" . select-window-2)
              ("3" . select-window-3)
              ("4" . select-window-4)
              ("5" . select-window-5)
              ("6" . select-window-6))

  :bind (:map rock/help
              ("i" . info)
              ("w" . where-is)
              ("b" . describe-bindings)
              ("k" . describe-key)
              ("f" . describe-function)
              ("m" . describe-mode)
              ("v" . describe-variable))

  :bind (:map rock/spelling
              ("s" . flyspell-mode)
              ("n" . flyspell-goto-next-error)
              ("c" . ispell-word))

  :bind (:map rock/buffers
              ("k" . kill-this-buffer)
              ("m" . ibuffer))

  :bind (:map rock/windows
              ("m" . maximize-window)
              ("=" . balance-windows)
              ("k" . delete-window)
              ("r" . window-configuration-to-register))

  :bind (:map rock/goto
              ("f" . find-file-at-point)
              ("u" . browse-url-at-point))

  :bind (:map rock/toggles
              ("t" . toggle-truncate-lines)
              ("w" . toggle-word-wrap))

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
    (global-evil-surround-mode))

  (use-package evil-escape
    :diminish evil-escape-mode
    :config
    (evil-escape-mode)
    (setq evil-escape-key-sequence "jk")))

(use-package seoul256-theme
  :config
  (setq seoul256-background 233)
  (load-theme 'seoul256 t)
  (custom-theme-set-faces 'seoul256
                          `(fringe              ((t (:background "#252525"))))
                          `(mode-line           ((t (:foreground "#ffffff" :background "#48667E"))))
                          `(mode-line-buffer-id ((t (:foreground "#ffffff" :background "#48667E"))))
                          `(mode-line-emphasis  ((t (:foreground "#222222" :slant italic))))
                          `(mode-line-highlight ((t (:foreground "#ffffff" :background "#48667E"))))
                          `(mode-line-inactive  ((t (:foreground "#666666" :background "#1c1c1c"))))
                          `(ivy-minibuffer-match-face-1  ((t (:inherit isearch)))))
  (set-face-attribute 'vertical-border nil :foreground (face-attribute 'fringe :background)))

(use-package dumb-jump
  :bind (:map rock/goto
              ("g" . dumb-jump-go)
              ("b" . dumb-jump-back)
              ("o" . dumb-jump-go-other-window)
              ("p" . dumb-jump-prompt)
              ("l" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-force-searcher 'ag))

(use-package window-numbering
  :init
  (window-numbering-mode t))

(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-height 22
        powerline-default-separator 'alternate
        powerline-image-apple-rgb t
        spaceline-window-numbers-unicode t
        spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (set-face-attribute 'powerline-active2 nil :background "#222222")
  (set-face-attribute 'powerline-inactive2 nil :background "#1c1c1c")
  (spaceline-spacemacs-theme))

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
                    '(whitespace-empty ((t (:background "Grey16")))))
  :bind (:map rock/toggles
              ("f" . global-whitespace-mode)))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package counsel
  :bind (("M-y" . counsel-yank-pop)
         ("M-x" . counsel-M-x)
         (:map rock/commands
               ("y" . counsel-yank-pop))))

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (add-hook 'minibuffer-setup-hook (lambda () (linum-mode 0)))
  (setq ivy-count-format "%d/%d "
        ivy-display-style 'fancy
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (defun rock/ignore-buffers (buf)
    (when (get-buffer buf)
      (with-current-buffer buf
        (when (member major-mode '(dired-mode ranger-mode)) t))))
  :config
  (add-hook 'ivy-ignore-buffers 'rock/ignore-buffers)
  :bind (:map rock/buffers
              ("b" . ivy-switch-buffer)
              ("B" . ivy-switch-buffer-other-window))
  :bind (:map ivy-minibuffer-map
              ("TAB"      . ivy-alt-done)
              ("<escape>" . minibuffer-keyboard-quit)
              ("C-j"      . ivy-next-line)
              ("C-k"      . ivy-previous-line)))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-cache-file (expand-file-name  "projectile.cache" rock/backup-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" rock/backup-dir))
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode)
  :bind (:map rock/projects
              ("t" . projectile-toggle-between-implementation-and-test)
              ("T" . projectile-find-test-file)
              ("P" . projectile-test-project)
              ("k" . projectile-kill-buffers)
              ("r" . projectile-replace)
              ("k" . projectile-kill-buffers)
              ("j" . projectile-find-tag)
              ("R" . projectile-regenerate-tags)
              ("i" . projectile-invalidate-cache)))

(use-package counsel-projectile
  :bind (:map rock/projects
              ("p" . counsel-projectile-switch-project)
              ("b" . counsel-projectile-switch-to-buffer)
              ("D" . counsel-projectile-find-dir)
              ("f" . counsel-projectile-find-file)
              ("a" . counsel-projectile-ag)))

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
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode)
  :bind (:map rock/commands
              ("u" . undo-tree-visualize)))

(use-package company
  :diminish company-mode
  :init
  (global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))

(use-package linum-relative
  :config
  (progn
    (setq linum-relative-current-symbol ""))
  :bind (:map rock/toggles
              ("l" . linum-relative-toggle)))

(use-package magit
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-hook 'git-commit-setup-hook (lambda () (linum-mode 0)))
  :config
  (bind-key "SPC" 'rock-leader magit-mode-map)
  (use-package evil-magit)
  :bind (:map rock/magit
              ("m" . magit-status)
              ("l" . magit-log-current)
              ("x" . magit-blame)
              ("b" . magit-branch-popup)
              ("c" . magit-branch-and-checkout)
              ("p" . magit-pull-popup)))

(use-package ediff
  :ensure nil
  :defer t
  :config (use-package evil-ediff))

(use-package git-timemachine
  :bind (:map rock/magit
              ("t" . git-timemachine))
  :config
  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package dash-at-point
  :bind (:map rock/help
              ("h" . dash-at-point)))

(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  (setq which-key-max-description-length 40))

(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode))

(use-package ranger
  :bind (:map rock/directories
              ("d" . ranger)
              ("e" . deer))
  :config
  (bind-key "SPC" 'rock-leader ranger-mode-map)
  (setq ranger-show-hidden t
        ranger-cleanup-eagerly t
        ranger-dont-show-binary t
        ranger-max-preview-size 10
        ranger-hide-cursor nil))

(use-package treemacs
  :defer t
  :bind (:map rock/directories
              ("t" . treemacs-toggle)
              ("f" . treemacs-find-file))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (use-package treemacs-evil
    :demand t)
  (use-package treemacs-projectile
    :defer t
    :bind (:map rock/projects
                ("d" . treemacs-projectile-toggle))
    :config
    (setq treemacs-header-function #'treemacs-projectile-create-header)))

(use-package elixir-mode
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package alchemist
  :commands alchemist-mode
  :config
  :bind (:map rock/alchemist
              ("t" . alchemist-mix-test)
              ("b" . alchemist-mix-test-this-buffer)
              ("a" . alchemist-mix-test-at-point)
              ("r" . alchemist-mix-rerun-last-test)
              ("s" . alchemist-mix-test-stale)
              ("T" . alchemist-project-toggle-file-and-tests)
              ("d" . alchemist-goto-definition-at-point)
              ("g" . alchemist-goto-jump-back)
              ("H" . alchemist-help)
              ("h" . alchemist-help-search-at-point)
              ("m" . alchemist-mix)
              ("c" . alchemist-mix-compile)
              ("i" . alchemist-iex-project-run)
              ("l" . alchemist-iex-send-current-line)
              ("z" . alchemist-iex-compile-this-buffer)
              ("x" . alchemist-iex-send-region)
              ("e" . alchemist-eval-current-line)
              ("E" . alchemist-eval-region)))

(use-package erlang)

(use-package markdown-mode
  :init (setq markdown-command "markdown"))

(use-package web-mode
  :mode (("\\.eex?\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :init
  (setq scss-compile-at-save nil)
  (defun rock/web-mode-hook ()
    (setq indent-tabs-mode nil
          web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-style-padding 2
          web-mode-script-padding 2
          web-mode-enable-current-element-highlight t))
  (add-hook 'web-mode-hook  'rock/web-mode-hook))

(use-package js2-mode
  :mode "\\.js\\'"
  :commands js2-mode
  :init
  (defun rock/js2-mode-hook ()
    (setq js2-basic-offset 2
          js2-strict-trailing-comma-warning nil
          js2-warn-about-unused-function-arguments t))
  (add-hook 'js2-mode-hook 'rock/js2-mode-hook))

(use-package rjsx-mode)

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

(provide 'init-packages)
