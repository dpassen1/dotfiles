(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      clojure-mode
                      cider
                      zenburn-theme
                      better-defaults
                      rainbow-delimiters
                      auto-complete
                      expand-region
                      multi-term))

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

;; Scroll only half-pages.
(require 'view)
(global-set-key "\C-v"   'View-scroll-half-page-forward)
(global-set-key "\M-v"   'View-scroll-half-page-backward)

;; Disable idle-highlight-mode from starter-kit
(remove-hook 'prog-mode-hook 'idle-highlight-mode)

;; Expand Region
(require 'expand-region)

;; Multi-Term
(require 'multi-term)
(global-set-key (kbd "C-x RET") 'multi-term)
(add-to-list 'term-bind-key-alist '("C-z" . term-stop-subjob))
(setq term-bind-key-alist (delete '("C-r" . isearch-backward) term-bind-key-alist))
(add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history))
