(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default js-indent-level 2)

(setq auto-indent-modes '(ruby-mode
                          javascript-mode
                          emacs-lisp-mode))
(dolist (mode auto-indent-modes)
  (add-hook (intern (format "%s-hook" mode)) 'auto-indent-minor-mode))

(defun clean-buffer ()
  "Re-indent the entire buffer and cleanup whitespace."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (whitespace-cleanup-region (point-min) (point-max))))
