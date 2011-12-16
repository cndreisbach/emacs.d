(defun my-anything ()
  (interactive)
  (require 'anything-config)
  (anything-other-buffer
   '(anything-c-source-buffers
     anything-c-source-files-in-current-dir
     anything-c-source-recentf
     anything-c-source-locate)
   "*my-anything*"))
