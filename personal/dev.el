(require 'cl)
(require 'use-package)

(defun scroll-down-keep-cursor ()
  ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
  ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1))

(bind-key "M-n"  'scroll-down-keep-cursor)
(bind-key "M-p" 'scroll-up-keep-cursor)


(use-package rbenv
  :ensure t
  :init (global-rbenv-mode))

(use-package golden-ratio
  :ensure t
  :init (golden-ratio-mode 1))

(use-package projectile-rails
  :init (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("C-M-o" . change-outer)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m e" . mc/edit-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this-dwim)))

(use-package paredit
  :ensure t)

(use-package jist
  :ensure t
  :init (setq jist-github-token "b89bfefe7531d143e93c9dc613290572271734aa"))


(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

(use-package edit-server
  :ensure t
  :init (edit-server-start))

(use-package foreman
  :load-path "vender/foreman.el")

(use-package dired
  :init
  (progn
    (defun dired-back-to-top ()
      (interactive)
      (beginning-of-buffer)
      (dired-next-line 4))

    (defun dired-jump-to-bottom ()
      (interactive)
      (end-of-buffer)
      (dired-next-line -1))

    (define-key dired-mode-map
      (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

    (define-key dired-mode-map
      (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)))

(use-package org
  :defer t
  :init
  (progn
    (org-clock-persistence-insinuate)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (perl . t)
       (sh . t)
       (ruby . t)
       (ledger . t)
       (sqlite . t))))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)" "PHONE"))))

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

  (setq org-agenda-start-on-weekday 0)
  (setq org-clock-in-resume t)
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  (setq org-clock-into-drawer t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  (setq org-clock-persist (quote history))
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  (setq org-clock-report-include-clocking-task t)
  (setq org-clock-persist 'history)
  (setq org-time-stamp-rounding-minutes '(0 1))
  (setq org-src-fontify-natively t)
  (setq org-directory '("~/Dropbox/org-files/"))
  (setq org-agenda-files '("~/Dropbox/org-files/personal.org" "~/Dropbox/org-files/gameplan.org" "~/Dropbox/org-files/chaione.org"))

  (setq org-capture-templates
        (quote (("n" "Notes" entry (file+datetree "~/Dropbox/org-files/notes.org")
                 "* %?\n%U\n  %i" :clock-in t :clock-resume t)
                ("i" "Interruptions" entry  (file "~/Dropbox/org-files/interruptions.org")
                 "* %? :interruption:\n%T" :clock-in t :clock-resume t))))

  (defun rlb3/work-agenda ()
    (interactive)
    (let ((org-agenda-files '("~/Dropbox/org-files/gameplan.org" "~/Dropbox/org-files/chaione.org")))
      (org-agenda)))

  (defun rlb3/personal-agenda ()
    (interactive)
    (let ((org-agenda-files '( "~/Dropbox/org-files/personal.org")))
      (org-agenda)))


  :config (use-package ox-gfm :ensure t)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("<f10>" . rlb3/work-agenda)
         ("<f9>" . rlb3/personal-agenda)
         ("C-c b" . org-iswitchb)
         ("C-M-t" . org-capture)))
