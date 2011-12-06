;;; Load packages
(setq my-packages '(magit
		    smex
		    ido-ubiquitous
		    undo-tree
		    full-ack
		    deft
		    find-file-in-project
		    htmlize
		    marmalade
		    yaml-mode
		    haml-mode
		    sass-mode scss-mode
		    feature-mode
		    coffee-mode
		    go-mode
		    cperl-mode
		    markdown-mode
		    ))

(setq package-archives 
      '(("marmalade" . "http://marmalade-repo.org/packages/") 
	("gnu" . "http://elpa.gnu.org/packages/") 	
	("Tromey" . "http://tromey.com/elpa/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
