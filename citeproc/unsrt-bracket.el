;;; unsrt-bracket.el --- Numbered, bracket style

;;; Commentary:
;; An experiment to inherit a style, and modify part of it. We actually use parentheses, because org mixes [] up with a footnote.

(require 'unsrt)

;;; Code:

(setf (cdr (assoc 'prefix citation-style)) "("
      (cdr (assoc 'suffix citation-style)) ")"
      (cdr (assoc 'vertical-align citation-style)) 'baseline
      (cdr (assoc 'transpose-punctuation citation-style)) nil
      (cdr (assoc 'chomp-leading-space citation-style)) nil
      (cdr (assoc 'prefix (cdr (assoc 'citeauthor citation-style)))) ""
      (cdr (assoc 'suffix (cdr (assoc 'citeauthor citation-style)))) "")

(provide 'unsrt-bracket)

;;; unsrt-bracket.el ends here
