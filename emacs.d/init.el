;;; init.el --- Initialization code ;;; -*- lexical-binding: t;-*-
;;; Commentary:

;;; Code:

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config (leaf-keywords-init))

  (leaf blackout
    :ensure t))

(leaf emacs
  :custom ((column-number-mode . t)
           (confirm-kill-processes . nil)
           (indent-tabs-mode . nil)
           (inhibit-startup-screen . t)
           (max-mini-window-height . 1)
           (read-quoted-char-radix . 16)
           (ring-bell-function . 'ignore))
  :setq ((frame-title-format . '("%b - emacs"))
         (kill-buffer-query-functions . nil)
         (message-truncate-lines . t))
  :config
  (fset 'yes-or-no-p 'y-or-n-p)

  (leaf abbrev
    :blackout t)

  (leaf autorevert
    :global-minor-mode global-auto-revert-mode)

  (leaf better-defaults
    :ensure t
    :require t)

  (leaf customize
    :custom `(custom-file . ,(concat user-emacs-directory "custom.el"))
    :config (load custom-file 'noerror))

  (leaf default-text-scale
    :ensure t
    :bind (("C-M-+" . default-text-scale-increase)
           ("C-M--" . default-text-scale-decrease)
           ("C-M-0" . default-text-scale-reset)))

  (leaf ido
    :defvar ido-decorations
    :config
    (setcar (nthcdr 0 ido-decorations) "")
    (setcar (nthcdr 1 ido-decorations) ""))

  (leaf initial-size
    :when (display-graphic-p)
    :config
    (add-to-list 'default-frame-alist '(height . 50))
    (add-to-list 'default-frame-alist '(width . 120)))

  (leaf ligatures
    :when (fboundp 'mac-auto-operator-composition-mode)
    :global-minor-mode mac-auto-operator-composition-mode)

  (leaf modus-operandi-theme
    :ensure t
    :config (load-theme 'modus-operandi t))

  (leaf perspective
    :ensure t
    :init (require 'ibuffer)
    :bind ("C-x C-b" . persp-ibuffer)
    :custom `((persp-mode-prefix-key . ,(kbd "C-x C-x"))
              (persp-sort . 'created))
    :global-minor-mode persp-mode)

  (leaf pragmata-pro
    :config
    (let ((default-font "PragmataPro Liga 12"))
      (add-to-list 'default-frame-alist `(font . ,default-font))
      (set-face-attribute 'default t :font default-font)))

  (leaf railwaycat
    :when (and (eq system-type 'darwin) (display-graphic-p))
    :custom ((mac-command-modifier . nil)
             (mac-option-modifier . 'meta))
    :global-minor-mode menu-bar-mode)

  (leaf smex
    :ensure t
    :bind ("M-x" . smex)
    :custom (smex-prompt-string . ""))

  (leaf view
    :bind (("C-v" . View-scroll-half-page-forward)
           ("M-v" . View-scroll-half-page-backward)))

  (leaf vterm
    :ensure t
    :bind ("C-x RET" . vterm-other-window)
    :defvar vterm-exit-functions
    :custom ((vterm-always-compile-module . t)
             (vterm-clear-scrollback-when-clearing . t)))

  (leaf with-editor
    :ensure t
    :hook (vterm-mode-hook . with-editor-export-editor))

  (leaf zoom
    :ensure t
    :blackout t
    :custom (zoom-size . '(0.618 . 0.618))
    :global-minor-mode t))

(leaf general-programming
  :config
  (leaf company
    :ensure t
    :blackout t
    :hook (prog-mode-hook cider-repl-mode-hook))

  (leaf deadgrep
    :ensure t
    :bind ("C-c r" . deadgrep))

  (leaf display-line-numbers
    :hook prog-mode-hook)

  (leaf dumb-jump
    :ensure t
    :custom (dumb-jump-prefer-searcher . 'rg)
    :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

  (leaf editorconfig
    :ensure t
    :blackout t
    :hook prog-mode-hook)

  (leaf expand-region
    :ensure t
    :bind ("C-c w" . er/expand-region))

  (leaf find-file-in-repository
    :ensure t
    :bind ("C-x M-f" . find-file-in-repository))

  (leaf flycheck
    :ensure t
    :blackout t
    :hook prog-mode-hook)

  (leaf hideshow
    :blackout hs-minor-mode
    :hook (prog-mode-hook . hs-minor-mode))

  (leaf magit
    :ensure t
    :bind (("C-x g" . magit-status)
           ("C-c g" . magit-file-dispatch)))

  (leaf markdown-mode
    :ensure t)

  (leaf paredit
    :ensure t
    :blackout t
    :hook (cider-repl-mode-hook clojure-mode-hook emacs-lisp-mode-hook))

  (leaf subword
    :blackout t
    :hook prog-mode-hook)

  (leaf whitespace
    :blackout t
    :custom ((whitespace-line-column . 80)
             (whitespace-style . '(face trailing lines-tail tabs)))
    :hook prog-mode-hook))

(leaf clojure
  :config
  (leaf clojure-mode
    :ensure t
    :custom (clojure-align-forms-automatically . t))

  (leaf cider
    :ensure t
    :blackout cider-auto-test-mode
    :custom ((cider-mode-line-show-connection . nil)
             (cider-prompt-for-symbol . nil)
             (cider-repl-display-help-banner . nil)
             (cider-save-file-on-load . t)
             (cider-use-fringe-indicators . nil))
    :defvar cider-repl-history-file
    :defun (clojure-project-dir . clojure-mode)
    :defer-config (setq cider-repl-history-file
                        (expand-file-name
                         ".cider-repl-history"
                         (clojure-project-dir))))

  (leaf flycheck-clj-kondo
    :ensure t
    :require t
    :after clojure-mode))

(leaf python
  :config
  (leaf blacken
    :ensure t
    :blackout t
    :custom (blacken-only-if-project-is-blackened . t)
    :hook python-mode-hook)

  (leaf poetry
    :ensure t))

(leaf web-development
  :config
  (leaf emmet-mode
    :ensure t
    :custom (emmet-preview-default . nil)
    :bind (emmet-mode-keymap
           ("C-c w" . nil))
    :hook (web-mode-hook . emmet-mode))

  (leaf restclient
    :ensure t)

  (leaf web-mode
    :ensure t
    :mode ("\\.html?\\'" "\\.jsx?\\'" "\\.css")
    :custom ((web-mode-enable-auto-closing . t)
             (web-mode-markup-indent-offset . 2))))

(provide 'init)

;;; init.el ends here
