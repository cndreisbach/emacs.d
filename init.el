;;;;; Clinton R. Nixon's Emacs 24 configuration
;;;;; Inspired by many sources, but especially:
;;;;; * https://github.com/avdi/.emacs24.d
;;;;; * http://technomancy.us/153

;;; Initial configuration
(require 'cl)
(setq inhibit-splash-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode t)
(global-visual-line-mode t)

;; Directories and file names
(setq user-config-dir (expand-file-name "~/.emacs.d"))
(setq user-elisp-dir (expand-file-name "elisp" user-config-dir))
(setq user-vendor-dir (expand-file-name "vendor" user-config-dir))

(add-to-list 'load-path user-elisp-dir)
(add-to-list 'load-path user-vendor-dir)

;; Add external projects to load path
(dolist (project (directory-files user-vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load all elisp files in ./init.d
(let ((init-dir (expand-file-name "init.d" user-config-dir)))
  (when (file-exists-p init-dir)
    (dolist (file (directory-files init-dir t "\\.el$"))
      (load file))))

;; Set up 'custom' system
(setq custom-file (expand-file-name "custom.el" user-config-dir))
(load custom-file)

;; Backups
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;; Configure packages
(setq ack-prompt-for-directory t)

(setq css-indent-level 2)
(setq css-indent-offset 2)
(setq scss-compile-at-save nil)

(setq deft-directory "~/Dropbox/Notes/")
(setq deft-text-mode 'org-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)

;;; Personal functions
(defalias 'qrr 'query-replace-regexp)

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

;;; Local config
(let ((local-config (expand-file-name "local.el" user-config-dir)))
  (when (file-exists-p local-config)
    (load local-config)))

