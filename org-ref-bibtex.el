;;; org-ref-bibtex.el -- org-ref-bibtex utilities

;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 0.1
;; Keywords: org-mode, bibtex
;; Package-Requires: ((org-ref) (s) (dash) (doi-utils) (key-chord))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; org-ref-bibtex-generate-longtitles
;; org-ref-bibtex-generate-shorttitles
;; org-ref-stringify-journal-name :: replace a journal name with a string in
;; `org-ref-bibtex-journal-abbreviations'
;; org-ref-set-journal-string :: in a bibtex entry run this to replace the
;; journal with a string
;;
;; org-ref-title-case-article :: title case the title in an article or book
;; org-ref-sentence-case-article :: sentence case the title in an article.

;; org-ref-replace-nonascii :: replace nonascii characters in a bibtex
;; entry.  Replacements are in `org-ref-nonascii-latex-replacements'.
;;
;; org-ref-title-case-article
;; org-ref-sentence-case-article
;;
;; org-ref-bibtex-next-entry :: bound to M-n
;; org-ref-bibtex-previous-entry :: bound to M-p
;;
;; Functions to act on a bibtex entry or file
;; org-ref-bibtex-hydra/body gives a hydra menu to a lot of useful functions.
;; org-ref-bibtex-new-entry/body gives a hydra menu to add new bibtex entries.
;; org-ref-bibtex-file/body gives a hydra menu of actions for the bibtex file
;;
;; org-ref-bibtex :: a deprecated menu of actions

(require 'bibtex)
(require 'dash)
(require 'hydra)
(require 'key-chord nil 'no-error)
(require 'message)
(require 's)

(require 'org-ref-citeproc)
(require 'doi-utils)

(defvar org-ref-pdf-directory)
(defvar org-ref-notes-directory)
(defvar org-ref-default-bibliography)

(declare-function reftex-get-bib-field "reftex-cite")
(declare-function key-chord-define-global "key-chord")
(declare-function org-ref-find-bibliography "org-ref-core")
(declare-function org-ref-open-bibtex-pdf "org-ref-core")
(declare-function org-ref-clean-bibtex-entry "org-ref-core")
(declare-function org-ref-open-in-browser "org-ref-core")
(declare-function org-ref-sort-bibtex-entry "org-ref-core")
(declare-function org-ref-build-full-bibliography "org-ref-core")
(declare-function helm-tag-bibtex-entry "org-ref-helm")
(declare-function bibtex-completion-edit-notes "bibtex-completion")
(declare-function bibtex-completion-get-value "bibtex-completion")
(declare-function bibtex-completion-get-entry "bibtex-completion")
(declare-function parsebib-find-next-item "parsebib")
(declare-function parsebib-read-entry "parsebib")
(declare-function helm-bibtex "helm-bibtex")


;;; Code:

;; This is duplicated from org-ref-core to try to avoid a byte-compile error.
(add-to-list 'load-path
	     (expand-file-name
	      "citeproc"
	      (file-name-directory (or load-file-name (buffer-file-name)))))

(add-to-list 'load-path
	     (expand-file-name
	      "citeproc/csl"
	      (file-name-directory (or load-file-name (buffer-file-name)))))

;;* Custom variables
(defgroup org-ref-bibtex nil
  "Customization group for org-ref-bibtex."
  :group 'org-ref-bibtex)


(defcustom org-ref-bibtex-hydra-key-chord
  nil
  "Key-chord to run `org-ref-bibtex-hydra'.
I like \"jj\""
  :type 'string
  :group 'org-ref-bibtex)


(defcustom org-ref-bibtex-hydra-key-binding
  nil
  "Key-binding to run `org-ref-bibtex-hydra'.
I like \C-cj."
  :type 'string
  :group 'org-ref-bibtex)


(defcustom org-ref-helm-cite-shorten-authors nil
  "If non-nil show only last names in the helm selection buffer."
  :type 'boolean
  :group 'org-ref-bibtex)


(defcustom org-ref-formatted-citation-formats
  '(("text"  . (("article"  . "${author}, ${title}, ${journal}, ${volume}(${number}), ${pages} (${year}). ${doi}")
		("inproceedings" . "${author}, ${title}, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
		("book" . "${author}, ${title} (${year}), ${address}: ${publisher}.")
		("phdthesis" . "${author}, ${title} (Doctoral dissertation) (${year}). ${school}, ${address}.")
		("inbook" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
		("incollection" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
		("proceedings" . "${editor} (Eds.), ${booktitle} (${year}). ${address}: ${publisher}.")
		("unpublished" . "${author}, ${title} (${year}). Unpublished manuscript.")
		(nil . "${author}, ${title} (${year}).")))
    ("org"  . (("article"  . "${author}, /${title}/, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
	       ("inproceedings" . "${author}, /${title}/, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
	       ("book" . "${author}, /${title}/ (${year}), ${address}: ${publisher}.")
	       ("phdthesis" . "${author}, /${title}/ (Doctoral dissertation) (${year}). ${school}, ${address}.")
	       ("inbook" . "${author}, /${title}/, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
	       ("incollection" . "${author}, /${title}/, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
	       ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
	       ("unpublished" . "${author}, /${title}/ (${year}). Unpublished manuscript.")
	       (nil . "${author}, /${title}/ (${year})."))))
  "Format strings for formatted bibtex entries for different citation backends.
Used in `org-ref-format-entry'."
  :type '(alist)
  :group 'org-ref-bibtex)

(defcustom org-ref-formatted-citation-backend "text"
  "The backend format for formatted citations.
Should be one of the cars of `org-ref-formatted-citation-formats'."
  :type 'string
  :group 'org-ref-bibtex)

;;* Journal abbreviations
(defvar org-ref-bibtex-journal-abbreviations
  '()
  "List of (string journal-full-name journal-abbreviation). Find
  new abbreviations at http://cassi.cas.org/search.jsp.")

(defcustom org-ref-bibtex-assoc-pdf-with-entry-move-function 'rename-file
  "Function to use when associating pdf files with bibtex entries.
The value should be either `rename-file' or `copy-file'. The former
will move and rename the original file. The latter will leave the
original file in place while creating a renamed copy in
`org-ref-pdf-directory'."
  :type 'function
  :group 'org-ref-bibtex)

(setq org-ref-bibtex-journal-abbreviations
      '(("ACR" "Accounts of Chemical Research" "Acc. Chem. Res.")
        ("ACAT" "ACS Catalysis" "ACS Catal.")
        ("AM" "Acta Materialia" "Acta Mater.")
        ("AMM" "Acta Metallurgica et Materialia" "Acta Metall. Mater.")
        ("AEM" "Advanced Energy Materials" "Adv. Energy Mater.")
        ("AAMI" "ACS Applied Materials \\& Interfaces"
         "ACS Appl. Mater. Interfaces")
        ("AMiner" "American Mineralogist" "Am. Mineral.")
        ("AngC" "Angewandte Chemie-International Edition"
         "Angew. Chem. Int. Edit.")
        ("APLM" "APL Materials" "APL Mat.")
        ("ACBE" "Applied Catalysis B: Environmental" "Appl. Catal. B-Environ.")
        ("APL" "Applied Physics Letters" "Appl. Phys. Lett.")
        ("ASS" "Applied Surface Science" "Appl. Surf. Sci.")
        ("CL" "Catalysis Letters" "Catal. Lett.")
        ("CC" "Catalysis Communications" "Catal. Commun.")
        ("CST" "Catalysis Science & Technology" "Catal. Sci. Technol.")
        ("CT" "Catalysis Today" "Catal. Today")
        ("ChC" "Chemical Communications" "Chem. Commun.")
        ("CPL" "Chemical Physics Letters" "Chem. Phys. Lett")
        ("CR" "Chemical Reviews" "Chem. Rev.")
        ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
        ("CSR" "Chemical Society Reviews" "Chem. Soc. Rev.")
        ("CM" "Chemistry of Materials" "Chem. Mater.")
        ("CSA" "Colloids and Surfaces, A: Physicochemical and Engineering Aspects"
         "Colloids Surf., A")
        ("CF" "Combustion and Flame" "Combust. Flame")
        ("CPMS" "Computational Materials Science" "Comp. Mater. Sci.")
        ("CPC" "Computer Physics Communications" "Comput. Phys. Commun.")
        ("CSE" "Computing in Science \\& Engineering" "Comput. Sci. Eng.")
        ("CGD" "Crystal Growth \\& Design" "Cryst. Growth Des.")
        ("CEC" "CrystEngComm" "CrystEngComm")
        ("EA" "Electrochimica Acta" "Electrochim. Acta")
        ("ECST" "ECS Transactions" "ECS Trans.")
        ("EES" "Energy \\& Environmental Science" "Energy Environ. Sci.")
        ("HPR" "High Pressure Research" "High Pressure Res.")
        ("IC" "Inorganic Chemistry" "Inorg. Chem.")
        ("IECR" "Industrial \\& Engineering Chemistry Research"
         "Ind. Eng. Chem. Res.")
        ("JJAP" "Japanese Journal of Applied Physics" "Jpn. J. Appl. Phys.")
        ("JMatR" "Journal of  Materials Research" "J. Mater. Res.")
        ("JALC" "Journal of Alloys and Compounds" "J. Alloy Compd.")
        ("JAC" "Journal of Applied Crystallography" "J. Appl. Crystallogr.")
        ("JAE" "Journal of Applied Electrochemistry" "J. Appl. Electrochem.")
        ("JAP" "Journal of Applied Physics" "J. Appl. Phys.")
        ("JC" "Journal of Catalysis" "J. Catal.")
        ("JCP" "Journal of Chemical Physics" "J. Chem. Phys.")
        ("JCC" "Journal of Computational Chemistry" "J. Comput. Chem.")
        ("JCG" "Journal of Crystal Growth" "J. Crys. Growth")
        ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
        ("JMC" "Journal of Materials Chemistry" "J. Mater. Chem.")
        ("JMSL" "Journal of Materials Science Letters" "J. Mater. Sci. Lett.")
        ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
        ("JPE" "Journal of Phase Equilibria" "J. Phase Equilib.")
        ("JPCS" "Journal of Physics and Chemistry of Solids"
         "J. Phys. Chem. Solids")
        ("JPCM" "Journal of Physics: Condensed Matter"
         "J. Phys.: Condens. Matter")
        ("JPS" "Journal of Power Sources" "J. Power Sources")
        ("JSSC" "Journal of Solid State Chemistry" "J. Solid State Chem.")
        ("JACerS" "Journal of the American Ceramic Society" "J. Am. Ceram. Soc.")
        ("JACS" "Journal of the American Chemical Society" "J. Am. Chem. Soc.")
        ("JASIST" "Journal of the American Society for Information Science and Technology"
         "J. Am. Soc. Inf. Sci. Technol.")
        ("JES" "Journal of The Electrochemical Society" "J. Electrochem. Soc.")
        ("JEaC" "Journal of Electroanalytical Chemistry" "J. Electroanal. Chem.")
        ("JMS" "Journal of Membrane Science" "J. Memb. Sci.")
        ("JRS" "Journal of Raman Spectroscopy" "J. Raman Spectrosc.")
        ("JVST" "Journal of Vacuum Science \\& Technology A"
         "J. Vac. Sci. Technol. A")
        ("ML" "Materials Letters" "Mater. Lett.")
        ("MSE-BS" "Materials Science and Engineering B" "Mat. Sci. Eng. B-Solid")
        ("MOLSIM" "Molecular Simulation" "Mol. Sim.")
        ("Nature" "Nature" "Nature")
        ("NM" "Nature Materials" "Nat. Mater.")
        ("NC" "Nature Chemistry" "Nat. Chem.")
        ("PML" "Philosophical Magazine Letters" "Phil. Mag. Lett.")
        ("PMA" "Philosophical Magazine A" "Phil. Mag. A")
        ("PA" "Physica A: Statistical Mechanics and its Applications" "Physica A")
        ("PB" "Physica B-Condensed Matter" "Physica B")
        ("PCCP" "Physical Chemistry Chemical Physics" "Phys. Chem. Chem. Phys.")
        ("PSSB" "physica status solidi (b)" "Phys. Status Solidi B")
        ("PRA" "Physical Review A" "Phys. Rev. A")
        ("PRB" "Physical Review B" "Phys. Rev. B")
        ("PRL" "Physical Review Letters" "Phys. Rev. Lett.")
        ("PCM" "Physics and Chemistry of Minerals" "Phys. Chem. Miner.")
        ("PNAS" "Proceedings of the National Academy of Sciences of the United States of America"
         "Proc. Natl. Acad. Sci. U. S. A.")
        ("PSurfSci" "Progress in Surface Science" "Prog. Surf. Sci.")
        ("Science" "Science" "Science")
        ("SM" "Scripta Materialia" "Scr. Mater.")
        ("SABC" "Sensors and Actuators B: Chemical" "Sensor. Actuat. B-Chem.")
        ("SS" "Surface Science" "Surf. Sci.")
        ("EPJB" "The European Physical Journal B" "Eur. Phys. J. B")
        ("JPC" "The Journal of Physical Chemistry" "J. Phys. Chem.")
        ("JPCB" "The Journal of Physical Chemistry B" "J. Phys. Chem. B")
        ("JPCC" "The Journal of Physical Chemistry C" "J. Phys. Chem. C")
        ("JPCL" "The Journal of Physical Chemistry Letters"
         "J. Phys. Chem. Lett.")
        ("JCP" "The Journal of Chemical Physics" "J. Chem. Phys.")
        ("MSMSE" "Modelling and Simulation in Materials Science and Engineering"
         "Modell. Simul. Mater. Sci. Eng.")
        ("TSF" "Thin Solid Films" "Thin Solid Films")
        ("TC" "Topics in Catalysis" "Top. Catal.")
        ("WR" "Water Research" "Water Res.")))


;;;###autoload
(defun org-ref-bibtex-generate-longtitles ()
  "Generate longtitles.bib which are @string definitions.
The full journal names are in `org-ref-bibtex-journal-abbreviations'."
  (interactive)
  (with-temp-file "longtitles.bib"
    (dolist (row org-ref-bibtex-journal-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
                      (nth 0 row)
                      (nth 1 row))))))


;;;###autoload
(defun org-ref-bibtex-generate-shorttitles ()
  "Generate shorttitles.bib which are @string definitions.
The abbreviated journal names in `org-ref-bibtex-journal-abbreviations'."
  (interactive)
  (with-temp-file "shorttitles.bib"
    (dolist (row org-ref-bibtex-journal-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
                      (nth 0 row)
                      (nth 2 row))))))


;;;###autoload
(defun org-ref-stringify-journal-name (&optional key start end)
  "Replace journal name in a bibtex entry with a string.
The strings are defined in
`org-ref-bibtex-journal-abbreviations'.  The optional arguments KEY,
START and END allow you to use this with `bibtex-map-entries'"
  (interactive)
  (bibtex-beginning-of-entry)
  (when
      (string= "article"
               (downcase
                (cdr (assoc "=type=" (bibtex-parse-entry)))))
    (let* ((full-names (mapcar
                        (lambda (row)
                          (cons  (nth 1 row) (nth 0 row)))
                        org-ref-bibtex-journal-abbreviations))
           (abbrev-names (mapcar
                          (lambda (row)
                            (cons  (nth 2 row) (nth 0 row)))
                          org-ref-bibtex-journal-abbreviations))
           (journal (s-trim (bibtex-autokey-get-field "journal")))
           (bstring (or
                     (cdr (assoc journal full-names))
                     (cdr (assoc journal abbrev-names)))))
      (when bstring
        (bibtex-set-field "journal" bstring t)
        (bibtex-fill-entry)))))


;;;###autoload
(defun org-ref-helm-set-journal-string ()
  "Helm interface to set a journal string in a bibtex entry.
Entries come from `org-ref-bibtex-journal-abbreviations'."
  (interactive)
  (bibtex-set-field
   "journal"
   (helm :sources
	 `((name . "journal")
	   (candidates . ,(mapcar
			   (lambda (x)
			     (cons (format "%s | %s"  (nth 1 x) (nth 2 x))
				   (car x)))
			   org-ref-bibtex-journal-abbreviations))
	   (action . (lambda (x) (identity x))))
         :input (s-trim (bibtex-autokey-get-field "journal")))
   t)
  (bibtex-fill-entry)
  (bibtex-clean-entry))


;;;###autoload
(defun org-ref-set-journal-string (full-journal-name)
  "Set a bibtex journal name to the string that represents FULL-JOURNAL-NAME.
This is defined in `org-ref-bibtex-journal-abbreviations'."
  (interactive (list
                (completing-read
                 "Journal: "
                 (mapcar
                  (lambda (x)
                    (nth 1 x))
                  org-ref-bibtex-journal-abbreviations))))
  ;; construct data alist for the string lookup.
  (let ((alist (mapcar
                (lambda (x)
                  (cons (nth 1 x) (nth 0 x)))
                org-ref-bibtex-journal-abbreviations)))
    (bibtex-set-field "journal" (cdr (assoc full-journal-name alist)) t)
    (bibtex-fill-entry)
    (bibtex-clean-entry)))

;;* Non-ascii character replacement
;; see https://github.com/fxcoudert/tools/blob/master/doi2bib for more replacements
(defvar org-ref-nonascii-latex-replacements
  '()
  "Cons list of non-ascii characters and their LaTeX representations.")


(setq org-ref-nonascii-latex-replacements
      '(("í" . "{\\\\'i}")
	("æ" . "{\\\\ae}")
	("ć" . "{\\\\'c}")
	("é" . "{\\\\'e}")
	("ä" . "{\\\\\"a}")
	("è" . "{\\\\`e}")
	("à" . "{\\\\`a}")
	("á" . "{\\\\'a}")
	("ø" . "{\\\\o}")
	("ë" . "{\\\\\"e}")
	("ü" . "{\\\\\"u}")
	("ñ" . "{\\\\~n}")
	("ņ" . "{\\\\c{n}}")
	("ñ" . "{\\\\~n}")
	("å" . "{\\\\aa}")
	("ö" . "{\\\\\"o}")
	("Á" . "{\\\\'A}")
	("í" . "{\\\\'i}")
	("ó" . "{\\\\'o}")
	("ó" . "{\\\\'o}")
	("ú" . "{\\\\'u}")
	("ú" . "{\\\\'u}")
	("ý" . "{\\\\'y}")
	("š" . "{\\\\v{s}}")
	("č" . "{\\\\v{c}}")
	("ř" . "{\\\\v{r}}")
	("š" . "{\\\\v{s}}")
	("İ" . "{\\\\.I}")
	("ğ" . "{\\\\u{g}}")
	("δ" . "$\\\\delta$")
	("ç" . "{\\\\c{c}}")
	("ß" . "{\\\\ss}")
	("≤" . "$\\\\le$")
	("≥" . "$\\\\ge$")
	("<" . "$<$")
	("θ" . "$\\\\theta$")
	("μ" . "$\\\\mu$")
	("→" . "$\\\\rightarrow$")
	("⇌" . "$\\\\leftrightharpoons$")
	("×" . "$\\\\times$")
	("°" . "$\\\\deg$")
	("ş" . "{\\\\c{s}}")
	("γ" . "$\\\\gamma$")
	("ɣ" . "$\\\\gamma$")
	("º" . "degC")
	("η" . "$\\\\eta$")
	("µ" . "$\\\\mu$")
	("α" . "$\\\\alpha$")
	("β" . "$\\\\beta$")
	("ɛ" . "$\\\\epsilon$")
	("Ⅵ" . "\\textrm{VI}")
	("Ⅲ" . "\\textrm{III}")
	("Ⅴ" . "\\textrm{V}")
	("λ" . "$\\\\lambda$")
	("π" . "$\\\\pi$")
	("∞" . "$\\\\infty$")
	("χ" . "$\\\\chi$")
	("∼" . "\\\\textasciitilde{}")
	("‑" . "\\\\textemdash{}")
	(" " . " ")
	("…" . "...")
	("•" . "\\\\textbullet ")
	;; I think these are non-ascii spaces. there seems to be more than one.
	(" " . " ")
	(" " . " ")
	(" " . " ")
	("–" . "-")
	("−" . "-")
	("–" . "-")
	("—" . "-")
	("‒" . "\\\\textemdash{}")
	("‘" . "'")
	("’" . "'")
	("’" . "'")
	("“" . "\"")
	("’" . "'")
	("”" . "\"")))


;;;###autoload
(defun org-ref-replace-nonascii ()
  "Hook function to replace non-ascii characters in a bibtex entry."
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x)
			    (car x))
			  org-ref-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
        (replace-match (cdr (assoc char org-ref-nonascii-latex-replacements))))
      (goto-char (point-min)))))

;;* Title case transformations
(defvar org-ref-lower-case-words
  '("a" "an" "on" "and" "for"
    "the" "of" "in")
  "List of words to keep lowercase when changing case in a title.")

(defcustom org-ref-title-case-types '("article" "book")
  "List of bibtex entry types in which the title will be converted to
title-case by org-ref-title-case."
  :type '(repeat string)
  :group 'org-ref-bibtex)

;;;###autoload
(defun org-ref-title-case (&optional key start end)
  "Convert a bibtex entry title to title-case if the entry type
is a member of the list org-ref-title-case-types. The arguments
KEY, START and END are optional, and are only there so you can
use this function with `bibtex-map-entries' to change all the
title entries in articles and books."
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
         (words (split-string title))
         (start 0))
    (when
        (member (downcase
		 (cdr (assoc "=type=" (bibtex-parse-entry))))
		org-ref-title-case-types)
      (setq words (mapcar
                   (lambda (word)
		     (cond
		      ;; words containing more than one . are probably
		      ;; abbreviations. We do not change those.
		      ((with-temp-buffer
			 (insert word)
			 (goto-char (point-min))
			 (> (count-matches "\\.") 1))
		       word)
		      ;; match words containing {} or \ which are probably
		      ;; LaTeX or protected words, ignore
		      ((string-match "\\$\\|{\\|}\\|(\\|)\\|\\\\" word)
		       word)
		      ;; these words should not be capitalized, unless they
		      ;; are the first word
		      ((-contains? org-ref-lower-case-words
				   (s-downcase word))
		       word)
		      ;; Words that are quoted
		      ((s-starts-with? "\"" word)
		       (concat "\"" (s-capitalize (substring word 1))))
		      (t
		       (s-capitalize word))))
                   words))

      ;; Check if first word should be capitalized
      (when (-contains? org-ref-lower-case-words (car words))
        (setf (car words) (s-capitalize (car words))))

      (setq title (mapconcat 'identity words " "))

      ;; Capitalize letters after a dash
      (while
          (string-match "[a-zA-Z]-\\([a-z]\\)" title start)
        (let ((char (substring title (match-beginning 1) (match-end 1))))
          (setf (substring title (match-beginning 1) (match-end 1))
                (format "%s" (upcase char)))
          (setq start (match-end 1))))

      ;; this is defined in doi-utils
      (bibtex-set-field
       "title"
       title)
      (bibtex-fill-entry))))

;;;###autoload
(defun org-ref-title-case-article (&optional key start end)
  "Convert a bibtex entry article or book title to title-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles and books."
  (interactive)
  (let ((org-ref-title-case-types '("article")))
    (org-ref-title-case)))


;;;###autoload
(defun org-ref-sentence-case-article (&optional key start end)
  "Convert a bibtex entry article title to sentence-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles."
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
         (words (split-string title))
         (start 0))
    (when
        (string= "article"
		 (downcase
		  (cdr (assoc "=type="
			      (bibtex-parse-entry)))))
      (setq words (mapcar
                   (lambda (word)
                     (if
                         ;; match words containing {} or \ which are probably
                         ;; LaTeX or protected words
                         (string-match "\\$\\|{\\|}\\|\\\\" word)
                         word
                       (s-downcase word)))
                   words))

      ;; capitalize first word
      (setf (car words) (s-capitalize (car words)))

      ;; join the words
      (setq title (mapconcat 'identity words " "))

      ;; capitalize a word after a :, eg. a subtitle, and protect it
      (while
          (string-match "[a-z]:\\s-+\\([A-Z]\\)" title start)
        (let ((char (substring title (match-beginning 1) (match-end 1))))
          (setf (substring title (match-beginning 1) (match-end 1))
                (format "%s" (upcase char)))
          (setq start (match-end 1))))

      ;; this is defined in doi-utils
      (bibtex-set-field
       "title" title)

      ;; clean and refill entry so it looks nice
      (bibtex-clean-entry)
      (bibtex-fill-entry))))

;;* Navigation in bibtex file
;;;###autoload
(defun org-ref-bibtex-next-entry (&optional n)
  "Jump to the beginning of the next bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing."
  (interactive "P")
  ;; Note if we start at the beginning of an entry, nothing
  ;; happens. We need to move forward a char, and call again.
  (when (= (point) (save-excursion
                     (bibtex-beginning-of-entry)))
    (forward-char)
    (org-ref-bibtex-next-entry))

  ;; search forward for an entry
  (when
      (re-search-forward bibtex-entry-head nil t (and (numberp n) n))
    ;; go to beginning of the entry
    (bibtex-beginning-of-entry)))


;;;###autoload
(defun org-ref-bibtex-previous-entry (&optional n)
  "Jump to beginning of the previous bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries back."
  (interactive "P")
  (bibtex-beginning-of-entry)
  (when
      (re-search-backward bibtex-entry-head nil t (and (numberp n) n))
    (bibtex-beginning-of-entry)))


(defun org-ref-bibtex-mode-keys ()
  "Modify keymaps used by `bibtex-mode'."
  (local-set-key (kbd "M-n") 'org-ref-bibtex-next-entry)
  (local-set-key (kbd "M-p") 'org-ref-bibtex-previous-entry))

;; add to bibtex-mode-hook
(add-hook 'bibtex-mode-hook 'org-ref-bibtex-mode-keys)

;;* Functions to act on an entry with a doi
;;;###autoload
(defun org-ref-bibtex-entry-doi ()
  "Get doi from entry at point."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (when (not (looking-at bibtex-any-valid-entry-type))
      (error "This entry does not appear to be a valid type."))
    (let ((entry (bibtex-parse-entry t)))
      (when (null entry)
	(error "Unable to parse this bibtex entry."))
      (reftex-get-bib-field "doi" entry))))

;; function that ensures that the url field of a bibtex entry is the
;; properly-formatted hyperlink of the DOI. See
;; http://blog.crossref.org/2016/09/new-crossref-doi-display-guidelines.html
;; for more information.
;;;###autoload
(defun org-ref-bibtex-format-url-if-doi ()
  "Hook function to format url to follow the current DOI conventions."
  (interactive)
  (if (eq (org-ref-bibtex-entry-doi) "") nil
    (let ((front-url "https://doi.org/")
          (doi (org-ref-bibtex-entry-doi)))
      (bibtex-set-field "url"
                        (concat front-url doi)))))


;;;###autoload
(defun org-ref-bibtex-wos ()
  "Open bibtex entry in Web Of Science if there is a DOI."
  (interactive)
  (doi-utils-wos (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-wos-citing ()
  "Open citing articles for bibtex entry in Web Of Science if
there is a DOI."
  (interactive)
  (doi-utils-wos-citing (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-wos-related ()
  "Open related articles for bibtex entry in Web Of Science if
there is a DOI."
  (interactive)
  (doi-utils-wos-related (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-crossref ()
  "Open the bibtex entry in Crossref by its doi."
  (interactive)
  (doi-utils-crossref (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-google-scholar ()
  "Open the bibtex entry at point in google-scholar by its doi."
  (interactive)
  (let ((doi (org-ref-bibtex-entry-doi)))
    (doi-utils-google-scholar
     (if (string= "" doi)
	 (save-excursion
	   (bibtex-beginning-of-entry)
	   (reftex-get-bib-field "title" (bibtex-parse-entry t)))
       doi))))


;;;###autoload
(defun org-ref-bibtex-pubmed ()
  "Open the bibtex entry at point in Pubmed by its doi."
  (interactive)
  (doi-utils-pubmed (org-ref-bibtex-entry-doi)))


;;;###autoload
(defun org-ref-bibtex-pdf (doi)
  "Open the pdf for the bibtex entry at point.
Thin wrapper to get `org-ref-bibtex' to open pdf, because it
calls functions with a DOI argument."
  (interactive)
  (org-ref-open-bibtex-pdf))

(defun org-ref-bibtex-get-file-move-func (prefix)
  "Determine whether to use `rename-file' or `copy-file' for `org-ref-bibtex-assoc-pdf-with-entry'.
When called with a PREFIX argument,
`org-ref-bibtex-assoc-pdf-with-entry-move-function' switches to the
opposite function from that which is defined in
`org-ref-assoc-pdf-with-entry-move-function'."
  (message (format "%s" prefix))
  (if (eq prefix nil)
      org-ref-bibtex-assoc-pdf-with-entry-move-function
    (if (eq org-ref-bibtex-assoc-pdf-with-entry-move-function 'rename-file)
	'copy-file
      'rename-file)))

;;;###autoload
(defun org-ref-bibtex-assoc-pdf-with-entry (&optional prefix)
  "Prompt for pdf associated with entry at point and rename it.
Check whether a pdf already exists in `org-ref-pdf-directory' with the
name '[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it in
`org-ref-pdf-directory'. Optional PREFIX argument toggles between
`rename-file' and `copy-file'."
  (interactive "P")
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((file (read-file-name "Select file associated with entry: "))
	   (bibtex-expand-strings t)
           (entry (bibtex-parse-entry t))
           (key (reftex-get-bib-field "=key=" entry))
           (pdf (concat org-ref-pdf-directory (concat key ".pdf")))
	   (file-move-func (org-ref-bibtex-get-file-move-func prefix)))
      (if (file-exists-p pdf)
	  (message (format "A file named %s already exists" pdf))
	(progn
	  (funcall file-move-func file pdf)
	  (message (format "Created file %s" pdf)))))))


;;* Hydra menus
;;** Hydra menu for bibtex entries
;; hydra menu for actions on bibtex entries
(defhydra org-ref-bibtex-hydra (:color blue)
  "
_p_: Open pdf     _y_: Copy key               _N_: New entry            _w_: WOS
_b_: Open url     _f_: Copy formatted entry   _o_: Copy entry           _c_: WOS citing
_r_: Refile entry _k_: Add keywords           _d_: delete entry         _a_: WOS related
_e_: Email entry  _K_: Edit keywords          _L_: clean entry          _P_: Pubmed
_U_: Update entry _N_: New entry              _R_: Crossref             _g_: Google Scholar
_s_: Sort entry   _a_: Remove nonascii        _h_: helm-bibtex          _q_: quit
_u_: Update field _F_: file funcs             _A_: Assoc pdf with entry
_n_: Open notes                               _T_: Title case
                                              _S_: Sentence case
"
  ("p" org-ref-open-bibtex-pdf)
  ("P" org-ref-bibtex-pubmed)
  ("w" org-ref-bibtex-wos)
  ("c" org-ref-bibtex-wos-citing)
  ("a" org-ref-bibtex-wos-related)
  ("R" org-ref-bibtex-crossref)
  ("g" org-ref-bibtex-google-scholar)
  ("N" org-ref-bibtex-new-entry/body)
  ("n" (progn
	 (bibtex-beginning-of-entry)
	 (bibtex-completion-edit-notes
	  (list (cdr (assoc "=key=" (bibtex-parse-entry t)))))))
  ("o" (lambda ()
	 (interactive)
	 (bibtex-copy-entry-as-kill)
	 (message "Use %s to paste the entry"
		  (substitute-command-keys (format "\\[bibtex-yank]")))))
  ("d" bibtex-kill-entry)
  ("L" org-ref-clean-bibtex-entry)
  ("y" (save-excursion
	 (bibtex-beginning-of-entry)
	 (when (looking-at bibtex-entry-maybe-empty-head)
	   (kill-new (bibtex-key-in-head)))))
  ("f" (progn
	 (bibtex-beginning-of-entry)
	 (kill-new
	  (org-ref-format-entry
	   (cdr (assoc "=key=" (bibtex-parse-entry t)))))))
  ("k" helm-tag-bibtex-entry)
  ("K" (lambda ()
         (interactive)
         (org-ref-set-bibtex-keywords
          (read-string "Keywords: "
                       (bibtex-autokey-get-field "keywords"))
          t)))
  ("b" org-ref-open-in-browser)
  ("r" (lambda ()
	 (interactive)
         (bibtex-beginning-of-entry)
         (bibtex-kill-entry)
         (find-file (completing-read
                     "Bibtex file: "
                     (f-entries "." (lambda (f) (f-ext? f "bib")))))
         (goto-char (point-max))
         (bibtex-yank)
         (save-buffer)
         (kill-buffer)))
  ("e" org-ref-email-bibtex-entry)
  ("U" (doi-utils-update-bibtex-entry-from-doi (org-ref-bibtex-entry-doi)))
  ("u" doi-utils-update-field)
  ("F" org-ref-bibtex-file/body)
  ("h" helm-bibtex)
  ("A" org-ref-bibtex-assoc-pdf-with-entry)
  ("a" org-ref-replace-nonascii)
  ("s" org-ref-sort-bibtex-entry)
  ("T" org-ref-title-case-article)
  ("S" org-ref-sentence-case-article)
  ("q" nil))

;; create key-chord and key binding for hydra
(when (and (featurep 'key-chord) org-ref-bibtex-hydra-key-chord)
  (key-chord-define-global
   org-ref-bibtex-hydra-key-chord
   'org-ref-bibtex-hydra/body))


(when org-ref-bibtex-hydra-key-binding
  (define-key bibtex-mode-map org-ref-bibtex-hydra-key-binding 'org-ref-bibtex-hydra/body))

;;** Hydra menu for new bibtex entries
;; A hydra for adding new bibtex entries.
(defhydra org-ref-bibtex-new-entry (:color blue)
  "New Bibtex entry:"
  ("a" bibtex-Article "Article")
  ("b" bibtex-Book "Book")
  ("i" bibtex-InBook "In book")
  ("l" bibtex-Booklet "Booklet")
  ("P" bibtex-Proceedings "Proceedings")
  ("p" bibtex-InProceedings "In proceedings")
  ("m" bibtex-Misc "Misc.")
  ("M" bibtex-Manual "Manual")
  ("T" bibtex-PhdThesis "PhD Thesis")
  ("t" bibtex-MastersThesis "MS Thesis")
  ("R" bibtex-TechReport "Report")
  ("u" bibtex-Unpublished "unpublished")
  ("c" bibtex-InCollection "Article in collection")
  ("q" nil "quit"))


;;** Hydra menu of functions to act on a bibtex file.
(defhydra org-ref-bibtex-file (:color blue)
  "Bibtex file functions: "
  ("v" bibtex-validate "Validate entries")
  ("s" bibtex-sort-buffer "Sort entries")
  ("r" bibtex-reformat "Reformat entries")
  ("c" bibtex-count-entries "Count entries")
  ("p" org-ref-build-full-bibliography "PDF bibliography"))


;;* DEPRECATED bibtex menu
(defvar org-ref-bibtex-menu-funcs '()
  "Functions to run in doi menu.
Each entry is a list of (key menu-name function).  The function
must take one argument, the doi.  This is somewhat deprecated, as
I prefer the hydra interfaces above.")

(setq org-ref-bibtex-menu-funcs
      '(("p" "df" org-ref-bibtex-pdf)
        ("C" "opy" (lambda (doi)
                     (kill-new (org-ref-bib-citation))
                     (bury-buffer)))
        ("w" "os" doi-utils-wos)
        ("c" "iting articles" doi-utils-wos-citing)
        ("r" "elated articles" doi-utils-wos-related)
        ("s" "Google Scholar" doi-utils-google-scholar)
        ("P" "Pubmed" doi-utils-pubmed)
        ("f" "CrossRef" doi-utils-crossref)))

;;;###autoload
(defun org-ref-bibtex ()
  "Menu command to run in a bibtex entry.
Functions from `org-ref-bibtex-menu-funcs'.  They all rely on the
entry having a doi."

  (interactive)
  ;; construct menu string as a message
  (message
   (concat
    (mapconcat
     (lambda (tup)
       (concat "[" (elt tup 0) "]"
               (elt tup 1) " "))
     org-ref-bibtex-menu-funcs "") ": "))
  (let* ((input (read-char-exclusive))
         (choice (assoc
                  (char-to-string input) org-ref-bibtex-menu-funcs)))
    (when choice
      (funcall
       (elt
        choice
        2)
       (org-ref-bibtex-entry-doi)
       ))))

(defalias 'jb 'org-ref-bibtex)


;;;###autoload
(defun org-ref-email-bibtex-entry ()
  "Email current bibtex entry at point and pdf if it exists."
  (interactive)

  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((key (reftex-get-bib-field "=key=" (bibtex-parse-entry t)))
	   pdf)
      ;; when we have org-ref defined we may have pdf to find.
      (when (boundp 'org-ref-pdf-directory)
	(setq pdf (expand-file-name
		   (concat key ".pdf")
		   org-ref-pdf-directory)))
      (bibtex-copy-entry-as-kill)
      (compose-mail)
      (message-goto-body)
      (insert (pop bibtex-entry-kill-ring))
      (message-goto-subject)
      (insert key)
      (message "%s exists %s" pdf (file-exists-p pdf))
      (when (file-exists-p pdf)
	(mml-attach-file pdf))
      (message-goto-to))))

;;* org-ref bibtex keywords
;; adapted from bibtex-utils.el
;; these are candidates for selecting keywords/tags
(defun org-ref-bibtex-keywords ()
  "Get keywords defined in current bibtex file.
These are in the keywords field, and are comma or semicolon separated."
  (save-excursion
    (goto-char (point-min))
    (let (keywords kstring)
      (while (re-search-forward "^\\s-*keywords.*{\\([^}]+\\)}" nil t)
        ;; TWS - remove newlines/multiple spaces:
        (setq kstring (replace-regexp-in-string
		       "[ \t\n]+" " "
		       (match-string 1)))
        (mapc
         (lambda (v)
           (add-to-list 'keywords v t))
         (split-string kstring "\\(,\\|;\\)[ \n]*\\|{\\|}" t)))
      keywords)))


;;;###autoload
(defun org-ref-set-bibtex-keywords (keywords &optional arg)
  "Add KEYWORDS to a bibtex entry.
If KEYWORDS is a list, it is converted to a comma-separated
string.  The KEYWORDS are added to the beginning of the
field.  Otherwise KEYWORDS should be a string of comma-separate
keywords.  Optional argument ARG prefix arg to replace keywords."
  (interactive
   (list
    (completing-read "Keyword: " (org-ref-bibtex-keywords))
    current-prefix-arg))
  (bibtex-set-field
   "keywords"
   (if arg
       ;; replace with arg
       (if (listp keywords)
           (mapconcat 'identity keywords ", ")
         keywords)
     ;; else concatentate
     (concat
      (if (listp keywords)
          (mapconcat 'identity keywords ", ")
        keywords)
      (when (not (string= "" (bibtex-autokey-get-field "keywords")))
        (concat ", "  (bibtex-autokey-get-field "keywords"))))))
  (when (buffer-file-name)
    (save-buffer)))


(defun org-ref-save-all-bibtex-buffers ()
  "Save all bibtex-buffers."
  (cl-loop for buffer in (buffer-list)
	   do
	   (with-current-buffer buffer
	     (when (and (buffer-file-name) (f-ext? (buffer-file-name) "bib"))
	       (save-buffer)))))


;;* org-ref bibtex cache
(defvar orhc-bibtex-cache-data
  '((hashes . ())
    (candidates . ()))
  "Cache data as an alist.
'hashes is a list of cons cells (bibfile . hash)
'candidates is a list of cons cells (bibfile . candidates).
Stored persistently in `orhc-bibtex-cache-file'.")


(defvar orhc-bibtex-cache-file
  "~/.orhc-bibtex-cache"
  "File to store cached data in.")


(defvar org-ref-bibtex-files nil
  "List of bibtex files to get entries from.
This is set internally.")


(defvar orhc-candidate-formats
  '(("article" . "${pdf}${notes}|${=key=}| ${author}, ${title}, ${journal} (${year}). ${keywords}")
    ("book" . "  |${=key=}| ${author}, ${title} (${year}) ${keywords}.")
    ("inbook" . "  |${=key=}| ${author}, ${chapter} in ${title} (${year}) ${keywords}")
    ("techreport" . "  |${=key=}| ${title}, ${institution} (${year}). ${keywords}")
    ("inproceedings" . "  |${=key=}| ${author}, ${title} in ${booktitle} (${year}). ${keywords}")
    ("incollection" . "  |${=key=}| ${author}, ${title} in ${booktitle} (${year}). ${keywords}")
    ("phdthesis" . "  |${=key=}| ${author}, ${title}, ${school} (${year}). Phd thesis. ${keywords}")
    ("mastersthesis" . "  |${=key=}| ${author}, ${title}, ${school} (${year}). MS thesis. ${keywords}")
    ("misc" . "  |${=key=}| ${author}, ${title}")
    ("unpublished" . "  |${=key=}| ${author}, ${title}"))
  "Formats for candidates.
It is an alist of (=type= . s-format-string).")


;; when you load, we should check the hashes and files
(defun orhc-load-cache-file ()
  "Load the cache file to set `orhc-bibtex-cache-data'."
  (when (file-exists-p orhc-bibtex-cache-file)
    (with-current-buffer (find-file-noselect orhc-bibtex-cache-file)
      (goto-char (point-min))
      (setq orhc-bibtex-cache-data (read (current-buffer))))
    (when (find-buffer-visiting orhc-bibtex-cache-file)
      (kill-buffer (find-buffer-visiting orhc-bibtex-cache-file)))))


(defun orhc-clear-cache ()
  "Clear the cache and delete `orhc-bibtex-cache-file'."
  (interactive)
  (setq orhc-bibtex-cache-data '((hashes . nil)
				 (candidates . nil)))
  (when (find-buffer-visiting orhc-bibtex-cache-file)
    (kill-buffer (find-buffer-visiting orhc-bibtex-cache-file)))
  (when (file-exists-p orhc-bibtex-cache-file)
    (delete-file orhc-bibtex-cache-file))
  (message "org-ref-helm-cite cache cleared."))


(defun orhc-bibtex-cache-up-to-date ()
  "Return if bibtex caches are up to date.
This means the hash of each bibfile is equal to the one for it in
the cache."
  (-all? 'identity
	 (cl-loop
	  for bibfile in org-ref-bibtex-files
	  collect
	  (string= (with-temp-buffer
		     (insert-file-contents bibfile)
		     (secure-hash 'sha256 (current-buffer)))
		   (or (cdr (assoc
			     bibfile
			     (cdr (assoc 'hashes orhc-bibtex-cache-data)))) "")))))

(defun orhc-bibtex-field-formatter (field entry)
  "Format FIELD in a bibtex parsed ENTRY.
A few fields are treated specially, e.g. authors are replaced by
comma-separated list, and I put :: around keywords to make it
easier to search specifically for them."
  (let ((s (replace-regexp-in-string
	    "^{\\|}$" ""
	    (replace-regexp-in-string
	     "[\n\\|\t\\|\s]+" " "
	     (or (cdr (assoc field entry))
		 (and (string= field "author")
		      (cdr (assoc "editor" entry)))
		 "")))))
    (cond
     ((string= field "author")
      (if org-ref-helm-cite-shorten-authors
	  ;; copied from `helm-bibtex-shorten-authors'
	  (cl-loop for a in (s-split " and " s)
		   for p = (s-split "," a t)
		   for sep = "" then ", "
		   concat sep
		   if (eq 1 (length p))
		   concat (-last-item (s-split " +" (car p) t))
		   else
		   concat (car p))
	(mapconcat 'identity (s-split " and " s) ", ")))
     ((string= field "keywords")
      (if (> (length s) 0)
	  (mapconcat (lambda (keyword)
		       (concat ":" (s-trim keyword) ":"))
		     (s-split "," s)
		     " ")
	""))
     ((string= field "pdf")
      (if (file-exists-p (expand-file-name
			  (concat (cdr (assoc "=key=" entry)) ".pdf")
			  org-ref-pdf-directory))
	  "⌘"
	" "))
     ((string= field "notes")
      (if (file-exists-p (expand-file-name
			  (concat (cdr (assoc "=key=" entry)) ".org")
			  org-ref-notes-directory))
	  "✎"
	" "))
     ;; catch all the other fields and just return them.
     (t
      s))))

(defun orhc-update-bibfile-cache (bibfile)
  "Update cache for BIBFILE.
This generates the candidates for the file. Some of this code is
adapted from `helm-bibtex-parse-bibliography'. This function runs
when called, it resets the cache for the BIBFILE."
  ;; check if the bibfile is already open, and preserve this state. i.e. if it
  ;; is not open close it, and if it is leave it open.
  (let ((bibfile-open (find-buffer-visiting bibfile)))
    (with-current-buffer (find-file-noselect bibfile)
      (bibtex-beginning-of-first-entry)
      (message "Updating cache for %s" bibfile)
      (let ((hash (secure-hash 'sha256 (current-buffer)))
	    (entries
	     (cl-loop
	      for entry-type = (parsebib-find-next-item)
	      while entry-type
	      unless (member-ignore-case entry-type
					 '("preamble" "string" "comment"))
	      collect
	      (let* ((entry (cl-loop for cons-cell in (parsebib-read-entry entry-type)
				     ;; we remove all properties too. they
				     ;; cause errors in reading/writing.
				     collect
				     (cons (substring-no-properties
					    (downcase (car cons-cell)))
					   (substring-no-properties
					    ;; clumsy way to remove surrounding
					    ;; brackets
					    (let ((s (cdr cons-cell)))
					      (if (or (and (s-starts-with? "{" s)
							   (s-ends-with? "}" s))
						      (and (s-starts-with? "\"" s)
							   (s-ends-with? "\"" s)))
						  (substring s 1 -1)
						s))))))
		     ;; (key (cdr (assoc "=key=" entry)))
		     )
		(cons
		 ;; this is the display string for helm. We try to use the formats
		 ;; in `orhc-candidate-formats', but if there isn't one we just put
		 ;; all the fields in.
		 (s-format
		  (or (cdr (assoc (downcase entry-type) orhc-candidate-formats))
		      (format "%s: %s" (cdr (assoc "=key=" entry)) entry))
		  'orhc-bibtex-field-formatter
		  entry)
		 ;; this is the candidate that is returned, the entry a-list +
		 ;; file and position.
		 (append entry (list (cons "bibfile" (buffer-file-name))
				     (cons "position" (point)))))))))

	;; Now update the cache variables for hash and entries
	(if (assoc bibfile (cdr (assoc 'candidates orhc-bibtex-cache-data)))
	    (setf (cdr (assoc bibfile
			      (cdr (assoc 'candidates orhc-bibtex-cache-data))))
		  entries)
	  (cl-pushnew (cons bibfile entries)
		      (cdr (assoc 'candidates orhc-bibtex-cache-data))))
	(if (assoc bibfile (cdr (assoc 'hashes orhc-bibtex-cache-data)))
	    (setf (cdr (assoc
			bibfile
			(cdr (assoc 'hashes orhc-bibtex-cache-data))))
		  hash)
	  (cl-pushnew (cons bibfile hash)
		      (cdr (assoc 'hashes orhc-bibtex-cache-data))))

	;; And save it to disk for persistent use
	(with-temp-file orhc-bibtex-cache-file
	  (print orhc-bibtex-cache-data (current-buffer)))))
    (unless bibfile-open (kill-buffer (find-buffer-visiting bibfile)))))

(defun orhc-update-bibtex-cache ()
  "Conditionally update cache for all files in `org-ref-bibtex-files'.
Files that have the same hash as in the cache are not updated."
  (cl-loop for bibfile in org-ref-bibtex-files
	   unless (string= (with-temp-buffer
			     (insert-file-contents bibfile)
			     (secure-hash 'sha256 (current-buffer)))
			   (or (cdr
				(assoc bibfile
				       (cdr
					(assoc 'hashes orhc-bibtex-cache-data))))
			       ""))
	   do
	   (orhc-update-bibfile-cache bibfile)))


(defun orhc-helm-cite-describe-cache ()
  "Show what is in the cache."
  (interactive)
  (let ((hash-cache (cdr (assoc 'hashes orhc-bibtex-cache-data)))
	(candidates-cache (cdr (assoc 'candidates orhc-bibtex-cache-data))))
    (message "%s\n\n%s"
	     (mapconcat (lambda (h)
			  (format "%s - %s" (car h) (cdr h)))
			hash-cache "\n")
	     (mapconcat (lambda (c)
			  (format "%s - %s entries" (car c) (length (cdr c))))
			candidates-cache "\n"))))


(defun orhc-bibtex-candidates ()
  "Return the candidates from cache for files listed in `org-ref-bibtex-files'.
Update the cache if necessary."
  ;; this only does something when the cache is out of date
  (org-ref-save-all-bibtex-buffers)
  (orhc-update-bibtex-cache)
  (let ((candidates (cdr (assoc 'candidates orhc-bibtex-cache-data))))
    (apply 'append
	   (cl-loop for bibfile in org-ref-bibtex-files
		    collect (cdr (assoc bibfile candidates))))))


;; Now load files and update them if needed. We do this when you load the
;; library so they are available later.
(orhc-load-cache-file)
(orhc-update-bibtex-cache)


;;* org-ref bibtex formatted citation

(defun org-ref-format-bibtex-entry (entry)
  "Return a formatted citation for the bibtex entry at point.
Formats are from `org-ref-formatted-citation-formats'. The
variable `org-ref-formatted-citation-backend' determines the set
of format strings used."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((formats (cdr (assoc org-ref-formatted-citation-backend  org-ref-formatted-citation-formats)))
	   (format-string)
	   (ref))
      (if (null entry)
	  "!!! No entry found !!!"
	(setq format-string (or (cdr (assoc (downcase (bibtex-completion-get-value "=type=" entry)) formats))
				"${author}, /${title}/ (${year})."))
	(setq ref (s-format format-string 'bibtex-completion-apa-get-value entry))
	(replace-regexp-in-string "\\([.?!]\\)\\." "\\1" ref)))))


(defun org-ref-format-entry (key)
  "Returns a formatted bibtex entry for KEY."
  (let* ((bibtex-completion-bibliography (org-ref-find-bibliography)))
    (org-ref-format-bibtex-entry (ignore-errors (bibtex-completion-get-entry key)))))


(defun org-ref-format-bibtex-entry-at-point ()
  "Return a formatted citation for the bibtex entry at point."
  (save-excursion
    (bibtex-beginning-of-entry)
    (org-ref-format-bibtex-entry (bibtex-parse-entry t))))


;; ** using citeproc
(defun orhc-formatted-citation (entry)
  "Get a formatted string for ENTRY."
  (require 'unsrt)
  (let* ((adaptive-fill-function '(lambda () "    "))
	 (indent-tabs-mode nil)
	 (entry-type (downcase
		      (cdr (assoc "=type=" entry))))
	 (entry-styles (cdr (assoc 'entries bibliography-style)))
	 (entry-fields
	  (progn
	    (if (cdr (assoc (intern entry-type) entry-styles))
		(cdr (assoc (intern entry-type) entry-styles))
	      (warn "%s not found. Using default." entry-type)
	      (cdr (assoc 't entry-styles)))))
	 (funcs (mapcar
		 (lambda (field)
		   (if (fboundp (intern
				 (format "orcp-%s" field)))
		       (intern
			(format "orcp-%s" field))
		     ;; No formatter found. just get the data
		     `(lambda (entry)
			(orcp-get-entry-field
			 ,(symbol-name field) entry))))
		 entry-fields)))

    ;; this is the entry. We do this in a buffer to make it
    ;; easy to indent, fill, etc...
    (with-temp-buffer
      (insert (mapconcat (lambda (field-func)
			   (funcall field-func entry))
			 funcs
			 ""))
      (buffer-string))))

;; * Extract bibtex blocks from an org-file
;;;###autoload
(defun org-ref-extract-bibtex-blocks (bibfile)
  "Extract all bibtex blocks in buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u), which
will clobber the file."
  (interactive
   (list (read-file-name "Bibfile: " nil nil nil
			 (file-name-nondirectory
			  (concat (file-name-sans-extension
				   (buffer-file-name))
				  ".bib")))))

  (let ((contents ""))
    (when (and (file-exists-p bibfile)
	       (not current-prefix-arg))
      (setq contents (with-temp-buffer
		       (insert-file-contents bibfile)
		       (buffer-string))))

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#\\+BEGIN_SRC bibtex" nil t)
	(setq contents
	      (concat
	       contents
	       (org-element-property :value (org-element-at-point))))))

    (with-temp-file bibfile
      (insert contents))))

(defun org-ref-bibtex-key-from-doi (doi &optional bib)
  "Return a bibtex entry's key from a DOI.
BIB is an optional filename to get the entry from. Defaults to
the first entry of `org-ref-default-bibliography'."
  (let ((bibfile (if bib bib (car org-ref-default-bibliography))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name bibfile))
      (search-forward doi)
      (bibtex-beginning-of-entry)
      (cdr (assoc "=key=" (bibtex-parse-entry))))))

;;* The end
(provide 'org-ref-bibtex)

;;; org-ref-bibtex.el ends here
