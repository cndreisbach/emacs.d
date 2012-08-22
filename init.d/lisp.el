(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(defun cnd-turn-on-paredit ()
  (paredit-mode 1))

(dolist (mode lisp-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            #'cnd-turn-on-paredit))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
