(require 'simp)

(simp-project-define
 '(:has (.git)
        :ignore (.git)))

(simp-project-define
 '(:type emacs
         :has (.git init.el)
         :ignore (.git auto-save-list backup backups elpa eshell url)))

 (simp-project-define
 '(:type rails
         :has (config.ru app/views app/models app/controllers)
         :ignore (.git tmp coverage log vendor public/system public/assets)))

(simp-project-define
 '(:type clojure
         :has (project.clj)
         :ignore (.git classes lib)))
