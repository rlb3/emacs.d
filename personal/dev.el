(global-rbenv-mode)

(golden-ratio-mode 1)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(add-to-list 'load-path "~/.emacs.d/vendor/foreman.el")
(require 'foreman)


;; Change inner
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "C-M-o") 'change-outer)

;; Multiple Cursors
(global-set-key (kbd "C-c C-e") 'mc/edit-lines)
(global-set-key (kbd "C-c C-f") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-b") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-x f") 'mc/mark-all-like-this)

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(setq jist-github-token "b89bfefe7531d143e93c9dc613290572271734aa")

(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)


;; (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
