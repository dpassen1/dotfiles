(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package ace-jump-mode
  :ensure t
  :pin    melpa-stable
  :defer  t
  :bind   ("C-c SPC" . ace-jump-mode))

(use-package better-defaults
  :ensure t
  :pin    melpa)

(use-package cider
  :ensure t
  :pin    melpa-stable
  :defer  t
  :init   (add-hook 'cider-repl-mode-hook 'paredit-mode)
          (add-hook 'cider-repl-mode-hook 'subword-mode)
          (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  :config (setq cider-repl-history-file (let ((default-directory (nrepl-project-directory-for (nrepl-current-dir))))
                                          (expand-file-name ".cider-repl-history"))))

(use-package clojure-mode
  :ensure t
  :pin    melpa-stable
  :mode   (("\\.clj\\'" . clojure-mode)
           ("\\.edn\\'" . clojure-mode))
  :init   (add-hook 'clojure-mode-hook 'paredit-mode)
          (add-hook 'clojure-mode-hook 'subword-mode)
          (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(use-package expand-region
  :ensure t
  :pin    melpa
  :defer  t
  :bind   ("C-c w" . er/expand-region))

(use-package magit
  :ensure t
  :pin    melpa-stable
  :defer  t
  :bind   ("C-x g" . magit-status))

(use-package multi-term
  :ensure t
  :pin    melpa
  :defer  t
  :bind   ("C-x RET" . multi-term)
  :config (add-to-list 'term-bind-key-alist '("C-z" . term-stop-subjob))
          (setq term-bind-key-alist (delete '("C-r" . isearch-backward) term-bind-key-alist))
          (add-to-list 'term-bind-key-alist '("C-r" . term-send-reverse-search-history)))

(use-package paredit
  :ensure t
  :pin    melpa-stable
  :config (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
          (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp))

(use-package rainbow-delimiters
  :ensure t
  :pin    melpa-stable)

(use-package smex
  :ensure t
  :pin    melpa-stable
  :bind   (("M-x" . smex)
           ("M-X" . smex-major-mode-commands)
           ("C-c C-c M-x" .  execute-extended-command)))

(use-package zenburn-theme
  :ensure t
  :pin    melpa-stable
  :config (load-theme 'zenburn t))

;; Prevent loading screen
(setq-default inhibit-startup-screen t)

;; Spaces only (no tab characters at all)!
(setq-default indent-tabs-mode nil)

;; Always show column numbers.
(setq-default column-number-mode t)

;; Stop flashing!
(setq ring-bell-function 'ignore)

;; line numbering
(global-linum-mode t)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string (max 2 w)) "d ")))
    ad-do-it))

;; 80 characters
(global-whitespace-mode t)
(setq whitespace-line-column 80)

;; trim trailing whitespace on saves
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; clean up whitespace display
(setq whitespace-style '(face trailing lines-tail tabs))

;; Scroll only half-pages.
(require 'view)
(global-set-key "\C-v" 'View-scroll-half-page-forward)
(global-set-key "\M-v" 'View-scroll-half-page-backward)

;; Automatically reload changed buffers
(global-auto-revert-mode t)
