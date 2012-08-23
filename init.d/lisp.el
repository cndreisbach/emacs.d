(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(defun cnd/engage-lisp-power ()
  (paredit-mode 1)
  (show-paren-mode 1))

(dolist (mode lisp-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook"))            
            #'cnd/engage-lisp-power))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
