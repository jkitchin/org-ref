;;; jmax-bibtex.el --- jmax-bibtex utilities

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

;; jmax-bibtex-generate-longtitles
;; jmax-bibtex-generate-shorttitles
;; jmax-stringify-journal-name :: replace a journal name with a string in
;; `jmax-bibtex-journal-abbreviations'
;; jmax-set-journal-string :: in a bibtex entry run this to replace the
;; journal with a string
;;
;; jmax-title-case-article :: title case the title in an article
;; jmax-sentence-case-article :: sentence case the title in an article.

;; jmax-replace-nonascii :: replace nonascii characters in a bibtex
;; entry. Replacements are in `jmax-nonascii-latex-replacements'.
;;
;; jmax-title-case-article
;; jmax-sentence-case-article
;;
;; jmax-bibtex-next-entry :: bound to M-n
;; jmax-bibtex-previous-entry :: bound to M-p
;;
;; Functions to act on a bibtex entry or file
;; jmax-bibtex-hydra/body gives a hydra menu to a lot of useful functions.
;; jmax-bibtex-new-entry/body gives a hydra menu to add new bibtex entries.
;; jmax-bibtex-file/body gives a hydra menu of actions for the bibtex file
;;
;; jmax-bibtex :: a deprecated menu of actions

(require 'hydra)
(require 'key-chord)

;;; Code:
;; * Custom variables
(defgroup jmax-bibtex nil
  "Customization group for jmax-bibtex.")


(defcustom jmax-bibtex-hydra-key-chord
  nil
  "key-chord to run `jmax-bibtex-hydra'.
I like \"jj\""
  :type 'string
  :group 'jmax-bibtex)


(defcustom jmax-bibtex-hydra-key-binding
  nil
  "key-binding to run `jmax-bibtex-hydra'.
I like \C-cj."
  :type 'string
  :group 'jmax-bibtex)

;; * Journal abbreviations
(defvar jmax-bibtex-journal-abbreviations
  '()
  "List of (string journal-full-name journal-abbreviation).  Find abbreviations at http://cassi.cas.org/search.jsp.")

(setq jmax-bibtex-journal-abbreviations
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


(defun jmax-bibtex-generate-longtitles ()
  "Generate longtitles.bib which are @string definitions.
The full journal names are in `jmax-bibtex-journal-abbreviations'."
  (interactive)
  (with-temp-file "longtitles.bib"
    (dolist (row jmax-bibtex-journal-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
		      (nth 0 row)
		      (nth 1 row))))))


(defun jmax-bibtex-generate-shorttitles ()
    "Generate shorttitles.bib which are @string definitions.
The abbreviated journal names in `jmax-bibtex-journal-abbreviations'."
  (interactive)
  (with-temp-file "shorttitles.bib"
    (dolist (row jmax-bibtex-journal-abbreviations)
      (insert (format "@string{%s=\"%s\"}\n"
		      (nth 0 row)
		      (nth 2 row))))))


(defun jmax-stringify-journal-name (&optional key start end)
  "Replace journal name in a bibtex entry with a string.
The strings are defined in
`jmax-bibtex-journal-abbreviations'.  The optional arguments KEY,
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
			jmax-bibtex-journal-abbreviations))
	   (abbrev-names (mapcar
			  (lambda (row)
			    (cons  (nth 2 row) (nth 0 row)))
			  jmax-bibtex-journal-abbreviations))
	   (journal (s-trim (bibtex-autokey-get-field "journal")))
	   (bstring (or
		     (cdr (assoc journal full-names))
		     (cdr (assoc journal abbrev-names)))))
      (when bstring
	(bibtex-set-field "journal" bstring t)
        (bibtex-fill-entry)))))

(defun jmax-helm-set-journal-string ()
  "Helm interface to set a journal string."
  (interactive)
  (bibtex-set-field
   "journal"
   (helm :sources `((name . "journal")
		    (candidates . ,(mapcar
				    (lambda (x)
				      (cons (format "%s | %s"  (nth 1 x) (nth 2 x))
					    (car x)))
				    jmax-bibtex-journal-abbreviations))
		    (action . (lambda (x) (identity x))))
	 :input (s-trim (bibtex-autokey-get-field "journal")))
   t)
  (bibtex-fill-entry)
  (bibtex-clean-entry))


(defun jmax-set-journal-string (full-journal-name)
  "Set a bibtex journal name to the string that represents FULL-JOURNAL-NAME.
This is defined in `jmax-bibtex-journal-abbreviations'."
  (interactive (list
		(ido-completing-read
		 "Journal: "
		 (mapcar
		  (lambda (x)
		    (nth 1 x))
		  jmax-bibtex-journal-abbreviations))))
  ;; construct data alist for the string lookup.
  (let ((alist (mapcar
		(lambda (x)
		  (cons (nth 1 x) (nth 0 x)))
		jmax-bibtex-journal-abbreviations)))
    (bibtex-set-field "journal" (cdr (assoc full-journal-name alist)) t)
    (bibtex-fill-entry)
    (bibtex-clean-entry)))

;; * Non-ascii character replacement
;; see https://github.com/fxcoudert/tools/blob/master/doi2bib for more replacements
(defvar jmax-nonascii-latex-replacements
  '()
  "Cons list of non-ascii characters and their LaTeX representations.")

(setq jmax-nonascii-latex-replacements
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
	("å" . "{\\\\aa}")
	("ö" . "{\\\\\"o}")
	("Á" . "{\\\\'A}")
	("á" . "{\\\\'a}")
	("í" . "{\\\\'i}")
	("ó" . "{\\\\'o}")
	("ó" . "{\\\\'o}")
	("ú" .  "{\\\\'u}")
	("ú" . "{\\\\'u}")
	("š" . "{\\\\v{s}}")
	("ř"  . "{\\\\v{r}}")
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
	("Ⅵ" . "VI")
	("Ⅲ" . "III")
	("Ⅴ" . "V")
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
	(" "  . " ")
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

(defun jmax-replace-nonascii ()
  "Hook function to replace non-ascii characters in a bibtex entry."

  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x) (car x)) jmax-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
	(replace-match (cdr (assoc char jmax-nonascii-latex-replacements))))
      (goto-char (point-min))))
  (save-buffer))

(add-hook 'org-ref-clean-bibtex-entry-hook 'jmax-replace-nonascii)

;; * Title case transformations
(defvar jmax-lower-case-words
  '("a" "an" "on" "and" "for"
    "the" "of" "in")
  "List of words to keep lowercase when changing case in a title.")


(defun jmax-title-case-article (&optional key start end)
  "Convert a bibtex entry article title to title-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles."
  (interactive)
  (bibtex-beginning-of-entry)

  (let* ((title (bibtex-autokey-get-field "title"))
	 (words (split-string title))
	 (start 0))
    (when
	(string= "article" (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
      (setq words (mapcar
		   (lambda (word)
		     (if (or
			  ;; match words containing {} or \ which are probably
			  ;; LaTeX or protected words
			  (string-match "\\$\\|{\\|}\\|\\\\" word)
			  ;; these words should not be capitalized, unless they
			  ;; are the first word
			  (-contains? jmax-lower-case-words (s-downcase word)))
			 word
		       (s-capitalize word)))
		   words))

      ;; Check if first word should be capitalized
      (when (-contains? jmax-lower-case-words (car words))
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

(add-hook 'org-ref-clean-bibtex-entry-hook 'jmax-title-case-article)


(defun jmax-sentence-case-article (&optional key start end)
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
	(string= "article" (downcase (cdr (assoc "=type=" (bibtex-parse-entry)))))
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
;;		(format "{%s}" (upcase char)))
		(format "%s" (upcase char)))
	  (setq start (match-end 1))))

      ;; this is defined in doi-utils
      (bibtex-set-field
       "title" title)

      ;; clean and refill entry so it looks nice
      (bibtex-clean-entry)
      (bibtex-fill-entry))))

;; * Navigation in bibtex file
(defun jmax-bibtex-next-entry (&optional n)
  "Jump to the beginning of the next bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing."
  (interactive "P")
  ;; Note if we start at the beginning of an entry, nothing
  ;; happens. We need to move forward a char, and call again.
  (when (= (point) (save-excursion
		     (bibtex-beginning-of-entry)))
    (forward-char)
    (jmax-bibtex-next-entry))

  ;; search forward for an entry
  (when
      (re-search-forward bibtex-entry-head nil t (and (numberp n) n))
    ;; go to beginning of the entry
    (bibtex-beginning-of-entry)))


(defun jmax-bibtex-previous-entry (&optional n)
  "Jump to beginning of the previous bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries back."
  (interactive "P")
  (bibtex-beginning-of-entry)
 (when
     (re-search-backward bibtex-entry-head nil t (and (numberp n) n))
   (bibtex-beginning-of-entry)))


(defun jmax-bibtex-mode-keys ()
  "Modify keymaps used by `bibtex-mode'."
  (local-set-key (kbd "M-n") 'jmax-bibtex-next-entry)
  (local-set-key (kbd "M-p") 'jmax-bibtex-previous-entry))

;; add to bibtex-mode-hook
(add-hook 'bibtex-mode-hook 'jmax-bibtex-mode-keys)

;; * Functions to act on an entry with a doi
(defun jmax-bibtex-entry-doi ()
  "Get doi from entry at point."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (reftex-get-bib-field "doi" (bibtex-parse-entry t))))


(defun jmax-bibtex-wos ()
  "Open bibtex entry in Web Of Science if there is a DOI."
  (interactive)
  (doi-utils-wos (jmax-bibtex-entry-doi)))


(defun jmax-bibtex-wos-citing ()
  "Open citing articles for bibtex entry in Web Of Science if there is a DOI."
  (interactive)
  (doi-utils-wos-citing (jmax-bibtex-entry-doi)))


(defun jmax-bibtex-wos-related ()
  "Open related articles for bibtex entry in Web Of Science if there is a DOI."
  (interactive)
  (doi-utils-wos-related (jmax-bibtex-entry-doi)))


(defun jmax-bibtex-wos-citing ()
  "Open citing articles for bibtex entry in Web Of Science if there is a DOI."
  (interactive)
  (doi-utils-wos-citing (jmax-bibtex-entry-doi)))


(defun jmax-bibtex-crossref ()
  "Open the bibtex entry in Crossref by its doi."
  (interactive)
  (doi-utils-crossref (jmax-bibtex-entry-doi)))


(defun jmax-bibtex-google-scholar ()
  "Open the bibtex entry at point in google-scholar by its doi."
  (interactive)
  (doi-utils-google-scholar (jmax-bibtex-entry-doi)))


(defun jmax-bibtex-pubmed ()
  "Open the bibtex entry at point in Pubmed by its doi."
  (interactive)
  (doi-utils-pubmed (jmax-bibtex-entry-doi)))


(defun jmax-bibtex-pdf (doi)
  "Open the pdf for the bibtex entry at point.
Thin wrapper to get `jmax-bibtex' to open pdf, because it calls
functions with a DOI argument."
  (interactive)
  (org-ref-open-bibtex-pdf))


;; * Hydra menus
;; ** Hydra menu for bibtex entries
;; hydra menu for actions on bibtex entries
(defhydra jmax-bibtex-hydra (:color blue)
   "
_p_: Open pdf     _y_: Copy key               _n_: New entry     _w_: WOS
_b_: Open url     _f_: Copy formatted entry   _o_: Copy entry    _c_: WOS citing
_r_: Refile entry _k_: Add keywords           _d_: delete entry  _a_: WOS related
_e_: Email entry  _K_: Edit keywords          _L_: clean entry   _P_: Pubmed
_U_: Update entry _N_: Open notes             _R_: Crossref      _g_: Google Scholar
_s_: Sort entry   _a_: Remove nonascii        _h_: helm-bibtex   _q_: quit
_u_: Update field _F_: file funcs
_T_: Title case
_S_: Sentence case
"
   ("p" org-ref-open-bibtex-pdf)
   ("P" jmax-bibtex-pubmed)
   ("w" jmax-bibtex-wos)
   ("c" jmax-bibtex-wos-citing)
   ("a" jmax-bibtex-wos-related)
   ("R" jmax-bibtex-crossref)
   ("g" jmax-bibtex-google-scholar)
   ("n" jmax-bibtex-new-entry/body)
   ("N" org-ref-open-bibtex-notes)
   ("o" bibtex-copy-entry-as-kill)
   ("d" bibtex-kill-entry)
   ("L" org-ref-clean-bibtex-entry)
   ("y" (kill-new  (bibtex-autokey-get-field "=key=")))
   ("f" bibtex-copy-summary-as-kill)
   ("k" helm-tag-bibtex-entry)
   ("K" (lambda ()
	  (interactive)
	  (org-ref-set-bibtex-keywords
	   (read-string "Keywords: "
			(bibtex-autokey-get-field "keywords"))
	   t)))
   ("b" org-ref-open-in-browser)
   ("r" (lambda () (interactive)
	  (bibtex-beginning-of-entry)
	  (bibtex-kill-entry)
	  (find-file (ido-completing-read
		      "Bibtex file: "
		      (f-entries "." (lambda (f) (f-ext? f "bib")))))
	  (goto-char (point-max))
	  (bibtex-yank)
	  (save-buffer)
	  (kill-buffer)))
   ("e" email-bibtex-entry)
   ("U" (doi-utils-update-bibtex-entry-from-doi (jmax-bibtex-entry-doi)))
   ("u" doi-utils-update-field)
   ("F" jmax-bibtex-file/body)
   ("h" helm-bibtex)
   ("a" jmax-replace-nonascii)
   ("s" org-ref-sort-bibtex-entry)
   ("T" jmax-title-case-article)
   ("S" jmax-sentence-case-article)
   ("q" nil))

;; create key-chord and key binding for hydra
(when jmax-bibtex-hydra-key-chord
  (key-chord-define-global
   jmax-bibtex-hydra-key-chord
   'jmax-bibtex-hydra/body))


(when jmax-bibtex-hydra-key-binding
  (global-set-key jmax-bibtex-hydra-key-binding 'jmax-bibtex-hydra/body))

;; ** Hydra menu for new bibtex entries
;; A hydra for adding new bibtex entries.
(defhydra jmax-bibtex-new-entry (:color blue)
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


;; ** Hydra menu of functions to act on a bibtex file.
(defhydra jmax-bibtex-file (:color blue)
  "Bibtex file functions: "
  ("v" bibtex-validate "Validate entries")
  ("s" bibtex-sort-buffer "Sort entries")
  ("r" bibtex-reformat "Reformat entries")
  ("c" bibtex-count-entries "Count entries")
  ("p" org-ref-build-full-bibliography "PDF bibliography"))


;; * DEPRECATED bibtex menu
(defvar jmax-bibtex-menu-funcs '()
 "Functions to run in doi menu.
Each entry is a list of (key menu-name function).  The function
must take one argument, the doi.  This is somewhat deprecated, as
I prefer the hydra interfaces above.")

(setq jmax-bibtex-menu-funcs
      '(("p" "df" jmax-bibtex-pdf)
	("C" "opy" (lambda (doi)
		     (kill-new (org-ref-bib-citation))
		     (bury-buffer)))
	("w" "os" doi-utils-wos)
	("c" "iting articles" doi-utils-wos-citing)
	("r" "elated articles" doi-utils-wos-related)
        ("s" "Google Scholar" doi-utils-google-scholar)
	("P" "Pubmed" doi-utils-pubmed)
        ("f" "CrossRef" doi-utils-crossref)))

(defun jmax-bibtex ()
  "Menu command to run in a bibtex entry.
Functions from `jmax-bibtex-menu-funcs'.  They all rely on the
entry having a doi."

  (interactive)
  ;; construct menu string as a message
  (message
   (concat
    (mapconcat
     (lambda (tup)
       (concat "[" (elt tup 0) "]"
	       (elt tup 1) " "))
     jmax-bibtex-menu-funcs "") ": "))
  (let* ((input (read-char-exclusive))
	 (choice (assoc
		  (char-to-string input) jmax-bibtex-menu-funcs)))
    (when choice
      (funcall
       (elt
	choice
	2)
       (jmax-bibtex-entry-doi)
       ))))

(defalias 'jb 'jmax-bibtex)

;; * The end
(provide 'jmax-bibtex)

;;; jmax-bibtex.el ends here
