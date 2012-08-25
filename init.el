;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "start.org" user-emacs-directory))
