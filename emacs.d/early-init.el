;;; early-init.el --- Early Initialization code ;;; -*- lexical-binding: t;-*-
;;; Commentary:

;;; Code:

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(provide 'early-init)

;;; early-init.el ends here
