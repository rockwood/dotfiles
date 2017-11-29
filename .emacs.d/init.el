(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(package-initialize)

(require 'init-use-package)
(require 'init-gui)
(require 'init-functions)
(require 'init-packages)

;; Emacs config goes here:
