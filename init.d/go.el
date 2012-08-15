(add-hook 'go-mode-hook
          (lambda ()
            (autopair-mode)
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(require 'go-autocomplete)
(require 'auto-complete-config)
