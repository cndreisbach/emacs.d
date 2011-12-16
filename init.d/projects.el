(require 'eproject)
(require 'eproject-extras)
(require 'eproject-tags)

(setq eproject-completing-read-function 'eproject--ido-completing-read)

(define-project-type ruby (generic)
  (or (look-for "Rakefile")
      (look-for "Gemfile")))

(define-project-type rails (ruby)
  (and (look-for "Rakefile")
       (look-for "config/environment.rb")))
