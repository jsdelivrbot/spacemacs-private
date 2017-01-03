;;; packages.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  张文博

;; Author: 张文博 <zwb@zhangwenbodeMacBook-Pro.local>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(setq zwb-packages
      '(
        multi-term
        beacon
        easy-kill
        git-messenger
        pangu-spacing
        nodejs-repl
        (systemtap :location local)
        ))

(defun zwb/post-init-multi-term ()
  (global-set-key (kbd "C-x m") 'multi-term)
  (add-hook 'term-mode-hook
            (lambda ()
              (progn
                (setq term-buffer-maximum-size 0)
                (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
                (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
                (define-key term-raw-map (kbd "C-y") 'term-paste)
                (yas-minor-mode -1))))

  (setq multi-term-scroll-to-bottom-on-output t)

  (setq term-bind-key-alist
        `(("C-c C-c" . term-interrupt-subjob)
          ("C-c C-e" . term-send-esc)
          ("C-c C-j" . term-line-mode)
          ("C-c C-k" . term-char-mode)
          ("C-p" . previous-line)
          ("C-n" . next-line)
          ("C-s" . isearch-forward)
          ("C-r" . isearch-backward)
          ("C-m" . term-send-return)
          ("C-y" . term-paste)
          ("M-f" . term-send-forward-word)
          ("M-b" . term-send-backward-word)
          ("M-o" . term-send-backspace)
          ("M-p" . term-send-up)
          ("M-n" . term-send-down)
          ("M-M" . term-send-forward-kill-word)
          ("M-N" . term-send-backward-kill-word)
          ("<C-backspace>" . term-send-backward-kill-word)
          ("M-r" . term-send-reverse-search-history)
          ("M-," . term-send-raw)
          ("M-." . comint-dynamic-complete))))

(defun zwb/init-beacon ()
  (use-package beacon
    :defer t
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")

      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)))


(defun zwb/init-easy-kill ()
  (use-package easy-kill
    :defer t
    :init
    (global-set-key [remap kill-ring-save] 'easy-kill)))

(defun zwb/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (defun my-vc-visit-file-revision (file rev)
        "Visit revision REV of FILE in another window.
With prefix argument, uses the current window instead.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
        ;; based on `vc-revision-other-window'.
        (interactive
         (let ((file (expand-file-name
                      (read-file-name
                       (if (buffer-file-name)
                           (format "File (%s): " (file-name-nondirectory
                                                  (buffer-file-name)))
                         "File: ")))))
           (require 'vc)
           (unless (vc-backend file)
             (error "File %s is not under version control" file))
           (list file (vc-read-revision
                       "Revision to visit (default is working revision): "
                       (list file)))))
        (require 'vc)
        (unless (vc-backend file)
          (error "File %s is not under version control" file))
        (let ((revision (if (string-equal rev "")
                            (vc-working-revision file)
                          rev))
              (visit (if current-prefix-arg
                         'switch-to-buffer
                       'switch-to-buffer-other-window)))
          (funcall visit (vc-find-revision file revision))))

      (define-key git-messenger-map (kbd "f") 'my-vc-visit-file-revision))))

(defun zwb/post-init-hungry-delete ()
  (global-hungry-delete-mode t))

(defun zwb/post-init-pangu-spacing ()
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))

(defun zwb/init-nodejs-repl()
  (use-package nodejs-repl
    :init
    :defer t))

(defun zwb/init-systemtap()
  (use-package systemtap
    :init
    (progn
      (add-to-list 'load-path "/Users/zwb/.spacemacs.d/layers/zwb/local/systemtap/")
      (load-file "/Users/zwb/.spacemacs.d/layers/zwb/local/systemtap/systemtap-init.el")
      )))

(provide 'packages.el)

;;; packages.el ends here
