(when (eq system-type 'darwin)
  (setq locate-command "mdfind")
  (setq anything-c-locate-command "mdfind %s")
  (setq ispell-program-name "aspell"))
