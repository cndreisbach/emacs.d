;;; init.el --- Where all the magic begins
;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "start.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "8b313e1793da427e90c034dbe74f3ad9092ac291846c0f855908c42a6bda1ff4" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "d30a78ecaf43c9816c328d4361b3ca21bafb49c6dee4da680997bd98b9e07787" "48441785d77807dce4e71b065f17504fbdfb6cbcd475523d20da4f1a14fbddac" "eacfc96fbe418c017f4a00fdde5d5029db8e2800a46251eb2174484fa431917e" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "2a0d95e0769b77dcf1d7053f832868153dddc5f6f21b9e70bdd414eb669ec262" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "15fa54dffe7ef4c91033739a8d2eba0fb897337dffe1f98b0629978183690c42" "d921083fbcd13748dd1eb638f66563d564762606f6ea4389ea9328b6f92723b7" "d1a574d57027c2bfadde6982455dfce8d27ced3ae4747c1c0313f95d23e96713" "fca8ce385e5424064320d2790297f735ecfde494674193b061b9ac371526d059" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "4ddc42a539280ec21ae202b6c12a4d7ce7d7af8a19e8c344b60b09f1ca1496d5" "d5b63a5da8bf90c7347e5e484dcde0380af010ec130f6f0d132113d807e49e03" "998e84b018da1d7f887f39d71ff7222d68f08d694fe0a6978652fb5a447bdcd2" "967c58175840fcea30b56f2a5a326b232d4939393bed59339d21e46cf4798ecf" "1760322f987b57884e38f4076ac586c27566a1d7ed421b67843c8c98a1501e3a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" default)))
 '(paradox-automatically-star t)
 '(safe-local-variable-values (quote ((org-export-html-style-include-scripts)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
