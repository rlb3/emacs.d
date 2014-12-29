(require 'cl)
(require 'use-package)

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
