;;; Load packages
(setq my-packages '(anything
                    anything-config
                    auto-indent-mode
                    clojure-mode
                    coffee-mode
                    cperl-mode
                    deft
                    feature-mode
                    find-file-in-git-repo
                    find-file-in-project
                    full-ack
                    go-mode
                    gist
                    haml-mode
                    htmlize
                    ido-ubiquitous
                    magit
                    markdown-mode
                    marmalade
                    org2blog
                    paredit
                    sass-mode
                    scss-mode
                    simp
                    slime
                    slime-repl
                    smex
                    undo-tree
                    yaml-mode
                    xml-rpc))

(setq package-archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("Tromey" . "http://tromey.com/elpa/")))

(load "package")
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
