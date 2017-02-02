;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; While we try to put most of our initialization in start.org, we
;; need to set up org-mode here. If we try to load start.org before
;; use-package, we'll end up with the built-in version of org-mode
;; which is old and not what we want.

;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil)

;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap =use-package=
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package org :ensure t :pin org)
(require 'diminish)
(require 'bind-key)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "start.org" user-emacs-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)
