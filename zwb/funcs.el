;;; funcs.el --- <enter description here>  -*- lexical-binding: t; -*-

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

(setq hexo-dir "~/Blog")

(defun zwb/hexo-publish (commit-msg)
  "git add . & git commit & git push & hexo d -g"
  (interactive "sInput commit message:")
  (async-shell-command (format "cd %s ;git add . ;git commit -m \"%s\" ;git push ;hexo d -g"
                               hexo-dir
                               commit-msg)))

(defun zwb/hexo-org-add-read-more ()
  "add <!--more-->"
  (interactive)
  (insert "#+BEGIN_HTML\n<!--more-->\n#+END_HTML"))

(defun zwb/hexo-org-new-open-post (post-name)
  "create a hexo org post"
  (interactive "sInput post name:")
  (find-file (format "%s/source/_posts/%s.org" hexo-dir post-name))
  (insert (format "#+TITLE: %s
#+DATE: %s
#+LAYOUT: post
#+TAGS:
#+CATEGORIES:
"  post-name (format-time-string "<%Y-%m-%d %a %H:%M>"))))

(defun zwb/hexo-org-source ()
  "use dired open hexo source dir"
  (interactive)
  (ido-find-file-in-dir (format "%s/source/" hexo-dir))
  )

(defun zwb/hexo-move-article ()
  "Move current file between _post and _draft;
You can run this function in dired or a hexo article."
  (interactive)
  (if (string-match "/\\(_posts/\\|_drafts/\\)$" default-directory)
      (let* ((parent-dir (file-truename (concat default-directory "../")))
             (dest-dir (if (string-match "_drafts/$" default-directory) "_posts/" "_drafts/"))))
    (cond (or (eq major-mode 'markdown-mode) (eq major-mode 'org-mode))
          (let* ((cur-file (buffer-file-name))
                 (new-file (concat parent-dir dest-dir (buffer-name))))
            (save-buffer)
            (kill-buffer)
            (rename-file cur-file new-file)
            (find-file new-file)
            (message (format "Now in %s" dest-dir))))
    ((eq major-mode 'dired-mode)
     (dired-rename-file (dired-get-filename nil)
                        (concat parent-dir dest-dir (dired-get-filename t))
                        nil)
     (message (format "The article has been moved to %s" dest-dir))))
  (message "You have to run this in a hexo article buffer or dired"))

(defun zwb/counsel-yank-zsh-history ()
  "Yank the zsh history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Zsh history:") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))
(provide 'funcs)

;;; funcs.el ends here
