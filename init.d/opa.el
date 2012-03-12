(autoload 'opa-js-mode "/usr/share/opa/emacs/opa-js-mode.el" "OPA JS editing mode." t)
(autoload 'opa-classic-mode "/usr/share/opa/emacs/opa-mode.el" "OPA CLASSIC editing mode." t)
(add-to-list 'auto-mode-alist '("\\.opa$" . opa-js-mode)) ;; <-- Set the default mode here
(add-to-list 'auto-mode-alist '("\\.js\\.opa$" . opa-js-mode))
(add-to-list 'auto-mode-alist '("\\.classic\\.opa$" . opa-classic-mode))
