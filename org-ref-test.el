(when (require 'undercover nil t)
  (undercover "org-ref.el" (:exclude "*-test.el")))

(ert-deftest plain-and-simple ()
  (should t))
