;;; Load packages
(setq my-packages '(
		    anything anything-config
		    coffee-mode
		    cperl-mode
		    deft
		    feature-mode
		    find-file-in-project
		    full-ack
		    go-mode
		    haml-mode
		    htmlize
		    ido-ubiquitous
		    magit
		    markdown-mode
		    marmalade
		    sass-mode scss-mode
		    smex
		    undo-tree
		    yaml-mode
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
