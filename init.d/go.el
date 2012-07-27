(add-hook 'go-mode-hook
          (lambda ()
            (autopair-mode)
            (add-hook 'before-save-hook 'gofmt nil t)))
