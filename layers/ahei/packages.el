;;; packages.el --- ahei layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: ZWB <zwb@zhangwenbodeMacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(setq ahei-packages
      '(color-theme
        dired+
        smart-mode-line-powerline-theme
        smart-mode-line
        (theme :location local)
        ))

(defun ahei/init-color-theme ()
  (use-package color-theme
    :defer t))

(defun ahei/init-dired+ ()
  (use-package dired+
    :defer t))

(defun ahei/init-smart-mode-line-powerline-theme ()
  (use-package smart-mode-line-powerline-theme
    :defer t))

(defun ahei/init-smart-mode-line ()
  (use-package smart-mode-line
    :defer t))

 (defun ahei/init-theme ()
   (use-package theme
     :defer t
     :init (require 'theme)))

;;; packages.el ends here
