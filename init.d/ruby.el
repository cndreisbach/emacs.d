(require 'yaml-mode)

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(autoload 'autopair-mode "autopair")
(add-hook 'ruby-mode-hook
          (lambda ()
            (autopair-mode)
            (add-hook 'before-save-hook 'whitespace-cleanup)
            (define-key ruby-mode-map (kbd "RET") 'newline-and-indent)))
