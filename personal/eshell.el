(setq eshell-prompt-function
      (lambda ()
        (concat
         (eshell/pwd)
         (let ((name (eshell/git-branch-name)))
           (if name
               (concat " (" name ")"))
           " $ "))))

;; git symbolic-ref HEAD 2> /dev/null | cut -b 12-
(defun eshell/git-branch-name ()
  (interactive)
  (let* ((branch-ref (when (shell-command-to-string "git rev-parse --git-dir 2>/dev/null")
                       (shell-command-to-string "git symbolic-ref HEAD 2>/dev/null")))
         (branch (nth 0 (reverse (split-string branch-ref "/")))))
    (when (not (string= branch ""))
      (substring branch 0 -1))))

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/cpanel ()
  (eshell/cd "/usr/local/cpanel"))

(defun eshell/ec (file)
  (find-file file))
