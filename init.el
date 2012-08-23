;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

(require 'org-install)
(require 'ob-tangle)
(mapc #'org-babel-load-file (directory-files "~/.emacs.d/org/" t "\\.org$"))
