;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A mode for editing gherkin files.
;;
;; For more about gherkin, see https://github.com/alandipert/gherkin
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Oh so many things

(defconst gherkin-keywords-re
  (regexp-opt
   '("&" "nil" "t"
     "quote" "fn" "if" "set!" "def" "do" "recur"
     "eq?" "nil?" "car" "cdr" "cons" "list" "eval"
     "apply" "read" "+" "-" "/" "mod" "<" ">"
     "cons?" "symbol?" "number?" "string?" "fn?"
     "gensym" "random" "exit" "println" "sh" "sh!"
     "load-file" "gc" "error" "type" "str")
   'words))

(defconst gherkin-keywords
  `(("<.*>" . font-lock-constant-face)
    ("#.*$" . font-lock-comment-face)
    ,gherkin-keywords-re
    ("\\?\\w+" . font-lock-variable-name-face)
    ("\"[^\"]*\"" . font-lock-string-face)
    ("'[^']*'" . font-lock-string-face)))

(define-derived-mode gherkin-mode lisp-mode
  "GK"
  :group 'gherkin-mode
  ;; Comments
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  ;; Font-lock support
  (setq font-lock-defaults '(gherkin-keywords))
  ;; Key maps
  ;;(define-key gherkin-mode-map (kbd "C-c C-x") 'whatever)
  )

(provide 'gherkin-mode)
