(require 'org)
(require 'ox-gfm)

(setq org-time-stamp-rounding-minutes '(0 1))
(setq org-src-fontify-natively t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE")
              (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "blue" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-in-resume t)
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist (quote history))
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
(setq org-clock-report-include-clocking-task t)

(setq org-directory '("~/Dropbox/org-files/"))
(setq org-agenda-files '("~/Dropbox/org-files/"))

(global-set-key (kbd "C-M-t") 'org-capture)

(setq org-capture-templates
      (quote (("n" "Notes" entry (file+datetree "~/Dropbox/org-files/notes.org")
               "* %?\n%U\n  %i" :clock-in t :clock-resume t)
              ("i" "Interruptions" entry  (file "~/Dropbox/org-files/interruptions.org")
               "* %? :interruption:\n%T" :clock-in t :clock-resume t))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (perl . t)
   (sh . t)
   (ruby . t)
   (ledger . t)
   (sqlite . t)))
