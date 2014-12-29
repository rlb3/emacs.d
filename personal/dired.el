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
