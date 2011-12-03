;;;;; Clinton R. Nixon's Emacs 24 configuration
;;;;; Inspired by many sources, but especially:
;;;;; * https://github.com/avdi/.emacs24.d
;;;;; * http://technomancy.us/153

;;; Initial configuration
(require 'cl)
(setq inhibit-splash-screen t)

;; Directories and file names
(setq crn-emacs-init-file (or load-file-name buffer-file-name))
(setq crn-emacs-config-dir
      (file-name-directory crn-emacs-init-file))
(setq user-emacs-directory crn-emacs-config-dir)
(setq crn-elisp-dir (expand-file-name "elisp" crn-emacs-config-dir))
(setq crn-vendor-dir (expand-file-name "vendor" crn-emacs-config-dir))
(setq crn-init-dir
      (expand-file-name "init.d" crn-emacs-config-dir))

(add-to-list 'load-path crn-elisp-dir)
(add-to-list 'load-path crn-vendor-dir)

; Add external projects to load path
(dolist (project (directory-files crn-vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

; Load all elisp files in ./init.d
(if (file-exists-p crn-init-dir)
    (dolist (file (directory-files crn-init-dir t "\\.el$"))
      (load file)))

;; Set up 'custom' system
(setq custom-file (expand-file-name "custom.el" crn-emacs-config-dir))
(load custom-file)

;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Configure packages
(setq ack-prompt-for-directory t)

(setq css-indent-level 2)
(setq css-indent-offset 2)

(setq deft-directory "~/Dropbox/Notes/")
(setq deft-text-mode 'org-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)

;;; Personal functions
(defalias 'qrr 'query-replace-regexp)
