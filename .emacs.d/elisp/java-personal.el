(provide 'java-personal)

; a la Google style guide
(c-add-style "java-google" '("user" (c-offsets-alist . ((case-label . +)
                                                        (arglist-intro . +)
                                                        (statement-cont . +)))))

(defun enable-java-style ()
  (setq c-basic-offset 2
        indent-tabs-mode nil)
  (c-set-style "java-google"))

(add-hook 'java-mode-hook 'enable-java-style)
