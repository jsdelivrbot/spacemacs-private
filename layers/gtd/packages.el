(setq gtd-packages
      '(
        (org-query :location local)
        ))

(defun gtd/init-org-query ()
  (use-package org-query
    :defer t
    :init
    (progn
      (with-eval-after-load 'org
        (require 'org-query-gtd)
        (defun gtd/agendablock-tasks-waiting ()
          `(tags-todo "/+WAITING|+DEFERRED"
                      ((org-agenda-overriding-header "Tasks waiting for something")
                       (org-tags-match-list-sublevels nil)
                       (org-agenda-skip-function (org-query-select "headline" (not (org-query-gtd-project))))
                       (org-agenda-todo-ignore-scheduled t)
                       (org-agenda-todo-ignore-deadlines t)
                       )))

        (defun gtd/agendablock-next-in-active ()
          `(tags-todo "/+NEXT"
                      ((org-agenda-overriding-header "Next tasks in active projects")
                       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-active-project-next-task)))
                       (org-tags-match-list-sublevels t)
                       (org-agenda-todo-ignore-scheduled 't)
                       (org-agenda-todo-ignore-deadlines 't)
                       (org-agenda-todo-ignore-with-date 't)
                       (org-agenda-sorting-strategy
                        '(todo-state-down effort-up category-keep)))))

        (defun gtd/agendablock-backlog-of-active ()
          `(tags-todo "/+TODO"
                      ((org-agenda-overriding-header "Backlog of active projects")
                       (org-agenda-skip-function (org-query-select "headline" (org-query-gtd-backlog-task)))
                       (org-agenda-todo-ignore-scheduled 't)
                       (org-agenda-todo-ignore-deadlines 't)
                       (org-agenda-todo-ignore-with-date 't)
                       (org-agenda-sorting-strategy
                        '(category-keep)))))

        (defun gtd/agendablock-active-projects-without-next ()
          `(tags-todo "/+NEXT"
                      ((org-agenda-overriding-header "Active projects without next task")
                       (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-active-project-stuck)))
                       (org-tags-match-list-sublevels 't)
                       (org-agenda-sorting-strategy
                        '(category-keep)))))

        (defun gtd/agendablock-active-projects-with-next ()
          `(tags-todo "/+NEXT"
                      ((org-agenda-overriding-header "Active projects with a next task")
                       (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-active-project-armed)))
                       (org-tags-match-list-sublevels 't)
                       (org-agenda-sorting-strategy
                        '(category-keep)))))

        (defun gtd/agendablock-waiting-projects ()
          `(tags-todo "/+WAITING"
                      ((org-agenda-overriding-header "Waiting projects")
                       (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-project)))
                       (org-tags-match-list-sublevels 't)
                       (org-agenda-sorting-strategy
                        '(category-keep)))))

        (defun gtd/agendablock-loose-tasks ()
          (require 'org-habit)
          `(tags-todo "/+TODO"
                      ((org-agenda-overriding-header "Tasks not belonging to a project")
                       (org-agenda-skip-function
                        (org-query-select "headline" (and (org-query-gtd-loose-task) (not (org-is-habit-p)))))
                       (org-agenda-todo-ignore-scheduled 't)
                       (org-agenda-todo-ignore-deadlines 't)
                       (org-agenda-todo-ignore-with-date 't)
                       (org-agenda-sorting-strategy
                        '(category-keep)))))

        (defun gtd/agendablock-checklists ()
          `(tags "CHECKLIST"
                 ((org-agenda-overriding-header "Checklists")
                  (org-tags-match-list-sublevels nil))))

        (defun gtd/agendablock-inbox ()
          `(tags-todo "LEVEL=2"
                      ((org-agenda-overriding-header "Tasks to refile")
                       (org-agenda-skip-function (org-query-select "tree" (org-query-gtd-refile)))
                       (org-tags-match-list-sublevels nil))))

        (defun gtd/show-agenda ()
          "Show the agenda fullscreen."
          (interactive)
          (org-agenda current-prefix-arg "r")
          (delete-other-windows))

        (setq org-todo-keywords
              (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                      (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

        (setq org-todo-keyword-faces
              (quote (("TODO" :foreground "red" :weight bold)
                      ("NEXT" :foreground "blue" :weight bold)
                      ("DONE" :foreground "forest green" :weight bold)
                      ("WAITING" :foreground "orange" :weight bold)
                      ("HOLD" :foreground "magenta" :weight bold)
                      ("CANCELLED" :foreground "forest green" :weight bold)
                      ("MEETING" :foreground "forest green" :weight bold)
                      ("PHONE" :foreground "forest green" :weight bold))))

        (setq org-use-fast-todo-selection t)

        (setq org-treat-S-cursor-todo-selection-as-state-change nil)

        (setq org-todo-state-tags-triggers
              (quote (("CANCELLED" ("CANCELLED" . t))
                      ("WAITING" ("WAITING" . t))
                      ("HOLD" ("WAITING") ("HOLD" . t))
                      (done ("WAITING") ("HOLD"))
                      ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                      ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                      ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

        (setq org-default-notes-file "~/GTD/refile.org")

        ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
        (setq org-capture-templates
              (quote (("t" "todo" entry (file "~/GTD/refile.org")
                       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("r" "respond" entry (file "~/GTD/refile.org")
                       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                      ("n" "note" entry (file "~/GTD/refile.org")
                       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                      ("j" "Journal" entry (file+datetree "~/GTD/diary.org")
                       "* %?\n%U\n" :clock-in t :clock-resume t)
                      ("w" "org-protocol" entry (file "~/GTD/refile.org")
                       "* TODO Review %c\n%U\n" :immediate-finish t)
                      ("m" "Meeting" entry (file "~/GTD/refile.org")
                       "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                      ("c" "Phone call" entry (file "~/GTD/refile.org")
                       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                      ("h" "Habit" entry (file "~/GTD/refile.org")
                       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

        ;; Remove empty LOGBOOK drawers on clock out
        (defun zwb/remove-empty-drawer-on-clock-out ()
          (interactive)
          (save-excursion
            (beginning-of-line 0)
            (org-remove-empty-drawer-at "LOGBOOK" (point))))

        (add-hook 'org-clock-out-hook 'zwb/remove-empty-drawer-on-clock-out 'append)

                                        ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                         (org-agenda-files :maxlevel . 9))))

                                        ; Use full outline paths for refile targets - we file directly with IDO
        (setq org-refile-use-outline-path t)

                                        ; Targets complete directly with IDO
        (setq org-outline-path-complete-in-steps nil)

                                        ; Allow refile to create parent tasks with confirmation
        (setq org-refile-allow-creating-parent-nodes (quote confirm))

        ;;;; Refile settings
                                        ; Exclude DONE state tasks from refile targets
        (defun zwb/verify-refile-target ()
          "Exclude todo keywords with a done state from refile targets"
          (not (member (nth 2 (org-heading-components)) org-done-keywords)))

        (setq org-refile-target-verify-function 'zwb/verify-refile-target)

        ;; Do not dim blocked tasks
        (setq org-agenda-dim-blocked-tasks nil)

        ;; Compact the block agenda view
        ;; (setq org-agenda-compact-blocks t)

        (setq org-agenda-custom-commands
              `(("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("h" "Habits" tags-todo "STYLE=\"habit\""
                 ((org-agenda-overriding-header "Habits")
                  (org-agenda-sorting-strategy
                   '(todo-state-down effort-up category-keep))))
                (" " "Agenda"
                 ((agenda "" ((org-agenda-ndays 1)))
                  ,(gtd/agendablock-inbox)
                  ,(gtd/agendablock-tasks-waiting)
                  ,(gtd/agendablock-next-in-active)
                  ,(gtd/agendablock-active-projects-with-next)
                  ,(gtd/agendablock-active-projects-without-next)
                  ,(gtd/agendablock-waiting-projects)
                  ,(gtd/agendablock-backlog-of-active)
                  ,(gtd/agendablock-checklists))
                 nil)
                ("r" "Review Agenda"
                 ((agenda "" ((org-agenda-ndays 1)))
                  ,(gtd/agendablock-inbox)
                  ,(gtd/agendablock-loose-tasks)
                  ,(gtd/agendablock-tasks-waiting)
                  ,(gtd/agendablock-next-in-active)
                  ,(gtd/agendablock-active-projects-with-next)
                  ,(gtd/agendablock-active-projects-without-next)
                  ,(gtd/agendablock-backlog-of-active)
                  ,(gtd/agendablock-checklists))
                 nil)))
        ;;
        ;; Resume clocking task when emacs is restarted
        (org-clock-persistence-insinuate)
        ;;
        ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        (setq org-clock-history-length 23)
        ;; Resume clocking task on clock-in if the clock is open
        (setq org-clock-in-resume t)
        ;; Change tasks to NEXT when clocking in
        (setq org-clock-in-switch-to-state 'zwb/clock-in-to-next)
        ;; Separate drawers for clocking and logs
        (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
        ;; Save clock data and state changes and notes in the LOGBOOK drawer
        (setq org-clock-into-drawer t)
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
        (setq org-clock-out-remove-zero-time-clocks t)
        ;; Clock out when moving task to a done state
        (setq org-clock-out-when-done t)
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        (setq org-clock-persist t)
        ;; Do not prompt to resume an active clock
        (setq org-clock-persist-query-resume nil)
        ;; Enable auto clock resolution for finding open clocks
        (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
        ;; Include current clocking task in clock reports
        (setq org-clock-report-include-clocking-task t)

        (setq zwb/keep-clock-running nil)

        (defun zwb/clock-in-to-next (kw)
          "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
          (when (not (and (boundp 'org-capture-mode) org-capture-mode))
            (cond
             ((and (member (org-get-todo-state) (list "TODO"))
                   (zwb/is-task-p))
              "NEXT")
             ((and (member (org-get-todo-state) (list "NEXT"))
                   (zwb/is-project-p))
              "TODO"))))

        (defun zwb/find-project-task ()
          "Move point to the parent (project) task if any"
          (save-restriction
            (widen)
            (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
              (while (org-up-heading-safe)
                (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                  (setq parent-task (point))))
              (goto-char parent-task)
              parent-task)))

        (defun zwb/punch-in (arg)
          "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
          (interactive "p")
          (setq zwb/keep-clock-running t)
          (if (equal major-mode 'org-agenda-mode)
              ;;
              ;; We're in the agenda
              ;;
              (let* ((marker (org-get-at-bol 'org-hd-marker))
                     (tags (org-with-point-at marker (org-get-tags-at))))
                (if (and (eq arg 4) tags)
                    (org-agenda-clock-in '(16))
                  (zwb/clock-in-organization-task-as-default)))
            ;;
            ;; We are not in the agenda
            ;;
            (save-restriction
              (widen)
                                        ; Find the tags on the current task
              (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
                  (org-clock-in '(16))
                (zwb/clock-in-organization-task-as-default)))))

        (defun zwb/punch-out ()
          (interactive)
          (setq zwb/keep-clock-running nil)
          (when (org-clock-is-active)
            (org-clock-out))
          (org-agenda-remove-restriction-lock))

        (defun zwb/clock-in-default-task ()
          (save-excursion
            (org-with-point-at org-clock-default-task
              (org-clock-in))))

        (defun zwb/clock-in-parent-task ()
          "Move point to the parent (project) task if any and clock in"
          (let ((parent-task))
            (save-excursion
              (save-restriction
                (widen)
                (while (and (not parent-task) (org-up-heading-safe))
                  (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                    (setq parent-task (point))))
                (if parent-task
                    (org-with-point-at parent-task
                      (org-clock-in))
                  (when zwb/keep-clock-running
                    (zwb/clock-in-default-task)))))))

        (defvar zwb/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

        (defun zwb/clock-in-organization-task-as-default ()
          (interactive)
          (org-with-point-at (org-id-find zwb/organization-task-id 'marker)
            (org-clock-in '(16))))

        (defun zwb/clock-out-maybe ()
          (when (and zwb/keep-clock-running
                     (not org-clock-clocking-in)
                     (marker-buffer org-clock-default-task)
                     (not org-clock-resolving-clocks-due-to-idleness))
            (zwb/clock-in-parent-task)))

        (add-hook 'org-clock-out-hook 'zwb/clock-out-maybe 'append)

        ; (require 'org-id)
        (defun zwb/clock-in-task-by-id (id)
          "Clock in a task by id"
          (org-with-point-at (org-id-find id 'marker)
            (org-clock-in nil)))

        (defun zwb/clock-in-last-task (arg)
          "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
          (interactive "p")
          (let ((clock-in-to-task
                 (cond
                  ((eq arg 4) org-clock-default-task)
                  ((and (org-clock-is-active)
                        (equal org-clock-default-task (cadr org-clock-history)))
                   (caddr org-clock-history))
                  ((org-clock-is-active) (cadr org-clock-history))
                  ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
                  (t (car org-clock-history)))))
            (widen)
            (org-with-point-at clock-in-to-task
              (org-clock-in nil))))

        (setq org-time-stamp-rounding-minutes (quote (1 1)))

        (setq org-agenda-clock-consistency-checks
              (quote (:max-duration "4:00"
                                    :min-duration 0
                                    :max-gap 0
                                    :gap-ok-around ("4:00"))))

        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
        (setq org-clock-out-remove-zero-time-clocks t)

        ;; Agenda clock report parameters
        (setq org-agenda-clockreport-parameter-plist
              (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

                                        ; Set default column view headings: Task Effort Clock_Summary
        (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
        (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                            ("STYLE_ALL" . "habit"))))

        ;; Agenda log mode items to display (closed and state changes by default)
        (setq org-agenda-log-mode-items (quote (closed state)))

                                        ; Tags with fast selection keys
        (setq org-tag-alist (quote ((:startgroup)
                                    ("@errand" . ?e)
                                    ("@office" . ?o)
                                    ("@home" . ?H)
                                    ("@farm" . ?f)
                                    (:endgroup)
                                    ("WAITING" . ?w)
                                    ("HOLD" . ?h)
                                    ("PERSONAL" . ?P)
                                    ("WORK" . ?W)
                                    ("FARM" . ?F)
                                    ("ORG" . ?O)
                                    ("NORANG" . ?N)
                                    ("crypt" . ?E)
                                    ("NOTE" . ?n)
                                    ("CANCELLED" . ?c)
                                    ("FLAGGED" . ??))))

                                        ; Allow setting single tags without the menu
        (setq org-fast-tag-selection-single-key (quote expert))

                                        ; For tag searches ignore tasks with scheduled and deadline dates
        (setq org-agenda-tags-todo-honor-ignore-options t)
        ))))
