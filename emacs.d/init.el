(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(auto-complete
                      better-defaults
                      cider
                      clj-refactor
                      clojure-mode
                      dash
                      epl
                      expand-region
                      find-file-in-project
                      git-commit
                      magit
                      multi-term
                      paredit
                      queue
                      rainbow-delimiters
                      smex
                      zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(load-theme 'zenburn t)

;; Prevent loading screen
(setq-default inhibit-startup-screen t)

;; Spaces only (no tab characters at all)!
(setq-default indent-tabs-mode nil)

;; Always show column numbers.
(setq-default column-number-mode t)

;; Stop flashing!
(setq ring-bell-function 'ignore)

;; ready clojure setup
(require 'clojure-mode)
(require 'cider)
(require 'cider-test)

;; clojure-mode configuration
(add-hook    'clojure-mode-hook 'paredit-mode)
(add-hook    'clojure-mode-hook 'subword-mode)
(add-hook    'clojure-mode-hook 'rainbow-delimiters-mode)
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; cider-mode configuration
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; line numbering
(global-linum-mode t)
(setq linum-format "%d ")

;; 80 characters
(global-whitespace-mode t)
(setq whitespace-line-column 80)

;; trim trailing whitespace on saves
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; clean up whitespace display
(setq whitespace-style '(face trailing lines-tail tabs))

;; keep backups out of project
(setq backup-directory-alist `(("." . , (concat user-emacs-directory "backups"))))

;; Scroll only half-pages.
(require 'view)
(global-set-key "\C-v"   'View-scroll-half-page-forward)
(global-set-key "\M-v"   'View-scroll-half-page-backward)

;; Expand Region
(require 'expand-region)

;; Multi-Term
(require 'multi-term)
(global-set-key (kbd "C-x RET") 'multi-term)
(add-to-list 'term-bind-key-alist '("C-z" . term-stop-subjob))
(setq term-bind-key-alist (delete '("C-r" . isearch-backward) term-bind-key-alist))
(add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))

;; SMEX
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; cljr-refactor
(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
