(when (require 'undercover nil t)
  (undercover "doi.el" (:exclude "*-test.el")))

(ert-deftest plain-and-simple ()
  (should t))
