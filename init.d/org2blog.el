(require 'netrc)

(setq crnio-netrc
      (netrc-machine (netrc-parse "~/.netrc") "crn_io" t))
(setq org2blog/wp-blog-alist
      '(("crn_io"
         :url "http://crn.io/xmlrpc.php"
         :username (netrc-get crnio-netrc "login")
         :password (netrc-get crnio-netrc "password")
         :tags-as-categories nil)))

(setq org-emphasis-alist '(("*" bold "<strong>" "</strong>") ("/" italic "<em>" "</em>") ("_" underline "<span style=\"text-decoration:underline;\">" "</span>") ("=" org-code "<code>" "</code>" verbatim) ("~" org-verbatim "<code>" "</code>" verbatim) ("+" (:strike-through t) "<del>" "</del>")))
(setq org-export-htmlize-output-type 'css)
(setq org-export-htmlized-org-css-url nil)
