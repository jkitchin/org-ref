;;; pdf-utils.el --- PDF utilities to download article from bibtex entry
;;;

;; Copyright (C) 2021  Lorenzo Martinico

;; Author: Lorenzo Martinico <lorenzo+git@martinico.me>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((org-ref))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to download PDF file from a bibtex entry,
;; if it includes a DOI. It includes a library of conversion function from
;; article URL to PDF URL for a variety of academic journals. The main function
;; in the package is pdf-utils-get-bibtex-entry, which should be called with the
;; cursor in a bibtex entry.

;;; Code:

(defvar org-ref-pdf-directory)

(eval-when-compile
  (require 'cl-lib))
(require 'bibtex)
(require 'dash)

(require 'url-http)
(require 'org-ref-utils)
(defcustom pdf-utils-download
  t
  "Try to download PDFs when adding bibtex entries when non-nil."
  :type 'boolean
  :group 'pdf-utils)

(defcustom pdf-utils-open-after-download
  nil
  "Open PDF after adding bibtex entries."
  :type 'boolean
  :group 'pdf-utils)

;; To fetch a PDF paper, given a URL to an academic journal website, we need to compute the url to
;; the pdf. We do this with a series of functions. Each function takes a single
;; argument, the redirect url. If it knows how to compute the pdf url it does,
;; and returns it. We store the functions in a variable:

(defvar pdf-utils-url-functions nil
  "Functions that return a url to a pdf from a redirect url.
Each function takes one argument, the redirect url.  The function
must return a pdf-url, or nil.")

(defvar *pdf-utils-waiting* t
  "Stores waiting state for url retrieval.")

;;** APS journals

(defun aps-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s*\\)://journals.aps.org" url)
    (replace-regexp-in-string "/abstract/" "/pdf/" url)))


;;** Science

(defun science-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://www.sciencemag.org" url)
    (concat url ".full.pdf")))


;;** Nature

(defun nature-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://www.nature.com" url)
    (concat url ".pdf")))


;;** Elsevier/ScienceDirect
;; You cannot compute these pdf links; they are embedded in the redirected pages.

;;** Wiley
;; Wiley have changed the url structure from
;; http://onlinelibrary.wiley.com/doi/10.1002/anie.201402680/abstract
;; http://onlinelibrary.wiley.com/doi/10.1002/anie.201402680/pdf
;; to
;; http://onlinelibrary.wiley.com/doi/abs/10.1002/anie.201402680
;; http://onlinelibrary.wiley.com/doi/pdf/10.1002/anie.201402680
;; Hence fewer steps are now required.

(defun wiley-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://onlinelibrary.wiley.com" url)
    (replace-regexp-in-string "doi/abs" "doi/pdf" url)))


(defun agu-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "https://agupubs.onlinelibrary.wiley.com"
		      url)
    (replace-regexp-in-string "/full/" "/pdfdirect/" url)))


;;** Springer

(defun springer-chapter-pdf-url (url)
  (when (string-match "^http\\(s?\\)://link.springer.com/chapter/" url)
    (replace-regexp-in-string "/chapter" "/content/pdf"
			      (concat url ".pdf"))))


(defun springer-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://link.springer.com" url)
    (replace-regexp-in-string "/article/" "/content/pdf/"
			      (concat url ".pdf"))))


;;** ACS
;; here is a typical url http://pubs.acs.org/doi/abs/10.1021/nl500037x
;; the pdf is found at http://pubs.acs.org/doi/pdf/10.1021/nl500037x

;; we just change /abs/ to /pdf/.

(defun acs-pdf-url-1 (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://pubs.acs.org/doi/abs/" url)
    (replace-regexp-in-string "/abs/" "/pdf/" url)))

;; 1/20/2016 I noticed this new pattern in pdf urls, where there is no abs in
;; the url
(defun acs-pdf-url-2 (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://pubs.acs.org/doi/" url)
    (replace-regexp-in-string "/doi/" "/doi/pdf/" url)))

;; 1/18/2019: It looks like they are using https now
(defun acs-pdf-url-3 (url)
  "Get url to the pdf from URL."
  (when (string-match "^https://pubs.acs.org/doi/" url)
    (replace-regexp-in-string "/doi/" "/doi/pdf/" url)))


;;** IOP

(defun iop-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://iopscience.iop.org" url)
    (concat url "/pdf")))

;;** JSTOR

(defun jstor-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://www.jstor.org" url)
    (concat (replace-regexp-in-string "/stable/" "/stable/pdfplus/" url) ".pdf")))


;;** AIP

(defun aip-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://scitation.aip.org" url)
    ;; get stuff after content
    (let (p1 p2 s p3)
      (setq p2 (replace-regexp-in-string
                "^http\\(s?\\)://scitation.aip.org/" "" url))
      (setq s (split-string p2 "/"))
      (setq p1 (mapconcat 'identity (-remove-at-indices '(0 6) s) "/"))
      (setq p3 (concat "/" (nth 0 s) (nth 1 s) "/" (nth 2 s) "/" (nth 3 s)))
      (format "http://scitation.aip.org/deliver/fulltext/%s.pdf?itemId=/%s&mimeType=pdf&containerItemId=%s"
              p1 p2 p3))))

;;** Taylor and Francis

(defun tandfonline-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://www.tandfonline.com" url)
    (replace-regexp-in-string "/abs/\\|/full/" "/pdf/" url)))

;;** ECS

(defun ecs-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://jes.ecsdl.org" url)
    (replace-regexp-in-string "\.abstract$" ".full.pdf" url)))

;; http://ecst.ecsdl.org/content/25/2/2769
;; http://ecst.ecsdl.org/content/25/2/2769.full.pdf


(defun ecst-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://ecst.ecsdl.org" url)
    (concat url ".full.pdf")))



;;** RSC

(defun rsc-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://pubs.rsc.org" url)
    (let ((url (downcase url)))
      (setq url (replace-regexp-in-string "articlelanding" "articlepdf" url))
      url)))

;;** Science Direct
(defun pdf-utils-get-science-direct-url (redirect-url)
  "Science direct hides the pdf url in html.  We get it out here.
REDIRECT-URL is where the pdf url will be in."
  (let ((first-url
         (with-current-buffer (url-retrieve-synchronously redirect-url)
           (goto-char (point-min))
           (when (re-search-forward "pdf_url\" content=\"\\([^\"]*\\)\"" nil t)
             (match-string-no-properties 1)))))
    (and first-url
         (with-current-buffer (url-retrieve-synchronously first-url)
           (goto-char (point-min))
           (when (re-search-forward "or click <a href=\"\\([^\"]*\\)\">" nil t)
             (match-string-no-properties 1))))))

(defun science-direct-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://www.sciencedirect.com" url)
    (pdf-utils-get-science-direct-url url)))

;; sometimes I get
;; http://linkinghub.elsevier.com/retrieve/pii/S0927025609004558
;; which actually redirect to
;; http://www.sciencedirect.com/science/article/pii/S0927025609004558
(defun linkinghub-elsevier-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match
	 "^https://linkinghub.elsevier.com/retrieve" url)
    (pdf-utils-get-science-direct-url
     (replace-regexp-in-string
      ;; change URL to science direct and use function to get pdf URL
      "https://linkinghub.elsevier.com/retrieve"
      "https://www.sciencedirect.com/science/article"
      url))))

;;** PNAS
;; http://www.pnas.org/content/early/2014/05/08/1319030111
;; http://www.pnas.org/content/early/2014/05/08/1319030111.full.pdf

;; with supporting info
;; http://www.pnas.org/content/early/2014/05/08/1319030111.full.pdf+html?with-ds=yes

(defun pnas-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://www.pnas.org" url)
    (concat url ".full.pdf?with-ds=yes")))


(defvar *pdf-utils-url* nil
  "Stores url to pdf download from a callback function.")

;;** Copernicus Publications
(defvar copernicus-journal-urls '(
                                  "^https://www.adv-geosci.net/"
                                  "^https://www.adv-radio-sci.net/"
                                  "^https://www.adv-sci-res.net/"
                                  "^https://www.adv-stat-clim-meteorol-oceanogr.net/"
                                  "^https://www.ann-geophys.net/"
                                  "^https://www.arch-anim-breed.net/"
                                  "^https://www.astra-proc.net/"
                                  "^https://www.atmos-chem-phys.net/"
                                  "^https://www.atmos-chem-phys-discuss.net/"
                                  "^https://www.atmos-meas-tech.net/"
                                  "^https://www.atmos-meas-tech-discuss.net/"
                                  "^https://www.biogeosciences.net/"
                                  "^https://www.biogeosciences-discuss.net/"
                                  "^https://www.clim-past.net/recent_papers.html"
                                  "^https://www.clim-past-discuss.net/"
                                  "^https://www.drink-water-eng-sci.net/"
                                  "^https://www.drink-water-eng-sci-discuss.net/"
                                  "^https://www.eg-quaternary-sci-j.net/"
                                  "^https://www.earth-surf-dynam.net/"
                                  "^https://www.earth-surf-dynam-discuss.net/"
                                  "^https://www.earth-syst-dynam.net/"
                                  "^https://www.earth-syst-dynam-discuss.net/"
                                  "^https://www.earth-syst-sci-data.net/"
                                  "^https://www.earth-syst-sci-data-discuss.net/"
                                  "^https://www.foss-rec.net/"
                                  "^https://www.geogr-helv.net/"
                                  "^https://www.geosci-instrum-method-data-syst.net/"
                                  "^https://www.geosci-instrum-method-data-syst-discuss.net/"
                                  "^https://www.geosci-model-dev.net/"
                                  "^https://www.geosci-model-dev-discuss.net/"
                                  "^https://www.hist-geo-space-sci.net/"
                                  "^https://www.hydrol-earth-syst-sci.net/"
                                  "^https://www.hydrol-earth-syst-sci-discuss.net/"
                                  "^https://www.j-sens-sens-syst.net/"
                                  "^https://www.mech-sci.net/"
                                  "^https://www.nat-hazards-earth-syst-sci.net/"
                                  "^https://www.nonlin-processes-geophys-discuss.net/"
                                  "^https://www.ocean-sci.net/"
                                  "^https://www.ocean-sci-discuss.net/"
                                  "^https://www.primate-biol.net/"
                                  "^https://www.proc-iahs.net/"
                                  "^https://www.sci-dril.net/"
                                  "^https://www.soil-journal.net/"
                                  "^https://www.soil-discuss.net/"
                                  "^https://www.solid-earth.net/"
                                  "^https://www.solid-earth-discuss.net/"
                                  "^https://www.stephan-mueller-spec-publ-ser.net/"
                                  "^https://www.the-cryosphere.net/"
                                  "^https://www.the-cryosphere-discuss.net/"
                                  "^https://www.web-ecol.net/"
                                  "^https://www.wind-energ-sci.net/"
                                  "^https://www.wind-energ-sci-discuss.net/"
                                  )
  "List of Copernicus URLs.")

(defun pdf-utils-get-copernicus-pdf-url (redirect-url)
  "Copernicus hides the pdf url in html.  We get it out here.
REDIRECT-URL is where the pdf url will be in."
  (setq *pdf-utils-waiting* t)
  (url-retrieve
   redirect-url
   (lambda (status)
     (goto-char (point-min))
     (re-search-forward "citation_pdf_url\" content=\"\\([^\"]*\\)\"" nil t)

     (setq *pdf-utils-url* (match-string 1)
	   *pdf-utils-waiting* nil)))
  (while *pdf-utils-waiting* (sleep-for 0.1))
  *pdf-utils-url*)

(defun copernicus-pdf-url (url)
  "Get url to the pdf from URL."

  (car (cl-loop for copurl in copernicus-journal-urls
	        when (string-match copurl url)
	        collect
	        (progn (pdf-utils-get-copernicus-pdf-url url)
	               *pdf-utils-url*))))


;;** Sage
(defun sage-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://pss.sagepub.com" url)
    (concat url ".full.pdf")))


;;** Journal of Neuroscience
(defun jneurosci-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://www.jneurosci.org" url)
    (concat url ".full.pdf")))

;;** Generic .full.pdf
(defun generic-full-pdf-url (url)
  (let ((pdf (concat url ".full.pdf")))
    (when (url-http-file-exists-p pdf)
      pdf)))

;;** IEEE
;; 10.1109/re.2014.6912247
;; http(s)://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6912247
;; http(s)://ieeexplore.ieee.org/ielx7/6903646/6912234/06912247.pdf
;; http(s)://ieeexplore.ieee.org/iel7/6903646/6912234/06912247.pdf?arnumber=6912247
;; <meta name="citation_pdf_url" content="http(s)://ieeexplore.ieee.org/iel7/6903646/6912234/06912247.pdf?arnumber=6912247">
;; <frame src="http(s)://ieeexplore.ieee.org/ielx7/6903646/6912234/06912247.pdf?tp=&arnumber=6912247&isnumber=6912234" frameborder=0 />
(defun ieee-pdf-url (url)
  "Get a url to the pdf from URL for IEEE urls."
  (when (string-match "^https?://ieeexplore.ieee.org" url)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "<meta name=\"citation_pdf_url\" content=\"\\([[:ascii:]]*?\\)\">" nil t)
	(let ((framed-url (match-string 1)))
          (with-current-buffer (url-retrieve-synchronously framed-url)
            (goto-char (point-min))
            (when (re-search-forward "<frame src=\"\\(http[[:ascii:]]*?\\)\"" nil t)
              (match-string 1))))))))

;; At least some IEEE papers need the following new pdf-link parsing
;; Example: 10.1109/35.667413
(defun ieee2-pdf-url (url)
  "Get a url to the pdf from URL for IEEE urls."
  (when (string-match "^https?://ieeexplore.ieee.org" url)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "\"pdfUrl\":\"\\([[:ascii:]]*?\\)\"" nil t)
	(let ((framed-url (match-string 1)))
          (with-current-buffer (url-retrieve-synchronously (concat "http://ieeexplore.ieee.org" framed-url))
            (goto-char (point-min))
            (when (re-search-forward "<frame src=\"\\(http[[:ascii:]]*?\\)\"" nil t)
              (match-string 1))))))))

;; Another try to get the ieee pdf
;; <iframe src="http(s)://ieeexplore.ieee.org/ielx5/8/4538127/04538164.pdf?tp=&arnumber=4538164&isnumber=4538127" frameborder=0>
(defun ieee3-pdf-url (url)
  "Get a url to the pdf from URL for IEEE urls."
  (when (string-match "^https?://ieeexplore.ieee.org" url)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (re-search-forward "\"pdfUrl\":\"\\([[:ascii:]]*?\\)\"" nil t)
	(let ((framed-url (match-string 1)))
          (with-current-buffer (url-retrieve-synchronously (concat "http://ieeexplore.ieee.org" framed-url))
            (goto-char (point-min))
            (when (re-search-forward "<iframe src=\"\\(http[[:ascii:]]*?\\)\"" nil t)
              (match-string 1))))))))

;; ACM Digital Library
;; https://dl.acm.org/doi/10.1145/1368088.1368132
(defun acm-pdf-url (url)
  "Get a url to the pdf from URL for ACM urls."
  (when (string-match "^https?://dl.acm.org" url)
    (replace-regexp-in-string "doi" "doi/pdf" url )))

;;** Optical Society of America (OSA)
(defun osa-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^https://www.osapublishing.org" url)
    (replace-regexp-in-string "abstract.cfm" "viewmedia.cfm" url )))



;;** ASME Biomechanical Journal

(defun asme-biomechanical-pdf-url (url)
  "Typical URL:  http://biomechanical.asmedigitalcollection.asme.org/article.aspx?articleid=1427237

On this page the pdf might be here:     <meta name=\"citation_author\" content=\"Dalong Li\" /><meta name=\"citation_author_email\" content=\"dal40@pitt.edu\" /><meta name=\"citation_author\" content=\"Anne M. Robertson\" /><meta name=\"citation_author_email\" content=\"rbertson@pitt.edu\" /><meta name=\"citation_title\" content=\"A Structural Multi-Mechanism Damage Model for Cerebral Arterial Tissue\" /><meta name=\"citation_firstpage\" content=\"101013\" /><meta name=\"citation_doi\" content=\"10.1115/1.3202559\" /><meta name=\"citation_keyword\" content=\"Mechanisms\" /><meta name=\"citation_keyword\" content=\"Biological tissues\" /><meta name=\"citation_keyword\" content=\"Stress\" /><meta name=\"citation_keyword\" content=\"Fibers\" /><meta name=\"citation_journal_title\" content=\"Journal of Biomechanical Engineering\" /><meta name=\"citation_journal_abbrev\" content=\"J Biomech Eng\" /><meta name=\"citation_volume\" content=\"131\" /><meta name=\"citation_issue\" content=\"10\" /><meta name=\"citation_publication_date\" content=\"2009/10/01\" /><meta name=\"citation_issn\" content=\"0148-0731\" /><meta name=\"citation_publisher\" content=\"American Society of Mechanical Engineers\" /><meta name=\"citation_pdf_url\" content=\"http://biomechanical.asmedigitalcollection.asme.org/data/journals/jbendy/27048/101013_1.pdf\" />

It is in the citation_pdf_url.

It would be better to parse this, but here I just use a regexp.
"

  (when (string-match "^http\\(s?\\)://biomechanical.asmedigitalcollection.asme.org" url)
    (setq *pdf-utils-waiting* 0)
    (url-retrieve
     url
     (lambda (status)
       (goto-char (point-min))
       (re-search-forward "citation_pdf_url\" content=\"\\(.*\\)\"" nil t)
       (message-box (match-string 1))
       (setq *pdf-utils-url* (match-string 1)
	     *pdf-utils-waiting* nil)))
    (while (and *pdf-utils-waiting* (< *pdf-utils-waiting* 5))
      (setq *pdf-utils-waiting* (+ *pdf-utils-waiting* 0.1))
      (sleep-for 0.1))
    *pdf-utils-url*))


;; Society for Industrial and Applied Mathematics (SIAM)
(defun siam-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s?\\)://epubs.siam.org" url)
    (replace-regexp-in-string "/doi/" "/doi/pdf/" url )))

;; PLOS journals
;; https://plos.org/
(defun plos-pdf-url (url)
  "Get url to the pdf from URL."
  (when (string-match "^http\\(s*\\)://journals.plos.org" url)
    (concat (replace-regexp-in-string (regexp-quote "/article?id=") "/article/file?id=" url) "&type=printable")))


;;** Add all functions

(setq pdf-utils-url-functions
      (list
       'aps-pdf-url
       'science-pdf-url
       'nature-pdf-url
       'wiley-pdf-url
       'springer-chapter-pdf-url
       'springer-pdf-url
       'acs-pdf-url-1
       'acs-pdf-url-2
       'acs-pdf-url-3
       'iop-pdf-url
       'jstor-pdf-url
       'aip-pdf-url
       'science-direct-pdf-url
       'linkinghub-elsevier-pdf-url
       'tandfonline-pdf-url
       'ecs-pdf-url
       'ecst-pdf-url
       'rsc-pdf-url
       'pnas-pdf-url
       'copernicus-pdf-url
       'sage-pdf-url
       'jneurosci-pdf-url
       'ieee-pdf-url
       'ieee2-pdf-url
       'ieee3-pdf-url
       'acm-pdf-url
       'osa-pdf-url
       'asme-biomechanical-pdf-url
       'siam-pdf-url
       'agu-pdf-url
       'plos-pdf-url
       'generic-full-pdf-url))

;;** Get the pdf url for a doi
;;
(defun pdf-utils-get-pdf-url (url)
  "Return a url to a pdf from a journal URL
Loops through the functions in `doi-utils-pdf-url-functions'
until one is found."
  (catch 'pdf-url
    (dolist (func pdf-utils-url-functions)
      (let ((this-pdf-url (funcall func url)))
        (when this-pdf-url
          (throw 'pdf-url this-pdf-url))))))

;;;###autoload
(defun pdf-utils-get-bibtex-entry (&optional arg)
  "Download pdf for entry at point if the pdf does not already exist locally.
The entry must have a doi. The pdf will be saved
to `org-ref-pdf-directory', by the name %s.pdf where %s is the
bibtex label.  Files will not be overwritten.  The pdf will be
checked to make sure it is a pdf, and not some html failure
page. You must have permission to access the pdf. We open the pdf
at the end if `pdf-utils-open-after-download' is non-nil.

With one prefix ARG, directly get the pdf from a file (through
`read-file-name') instead of looking up a DOI. With a double
prefix ARG, directly get the pdf from an open buffer (through
`read-buffer-to-switch') instead. These two alternative methods
work even if the entry has no DOI, and the pdf file is not
checked."
  (interactive "P")
  (save-excursion
    (bibtex-beginning-of-entry)
    (let ( ;; get doi, removing http://dx.doi.org/ if it is there.
          (doi (replace-regexp-in-string
                "https?://\\(dx.\\)?.doi.org/" ""
                (bibtex-autokey-get-field "doi")))
          (key (cdr (assoc "=key=" (bibtex-parse-entry))))
          (pdf-url)
          (pdf-file))

      (setq pdf-file (concat
		      (if org-ref-pdf-directory
			  (file-name-as-directory org-ref-pdf-directory)
			(read-directory-name "PDF directory: " "."))
		      key ".pdf"))
      ;; now get file if needed.
      (unless (file-exists-p pdf-file)
	(cond
	 ((and (not arg)
	       doi
	       (setq pdf-url (doi-utils-get-pdf-url doi)))
	  (url-copy-file pdf-url pdf-file)
	  ;; now check if we got a pdf
          (if (org-ref-pdf-p pdf-file)
              (message "%s saved" pdf-file)
            (delete-file pdf-file)
            (message "No pdf was downloaded.")
            (browse-url pdf-url)))
	 ((equal arg '(4))
	  (copy-file (expand-file-name (read-file-name "Pdf file: " nil nil t))
		     pdf-file))
	 ((equal arg '(16))
	  (with-current-buffer (read-buffer-to-switch "Pdf buffer: ")
	    (write-file pdf-file)))
	 (t
	  (message "We don't have a recipe for this journal.")))
	(when (and pdf-utils-open-after-download (file-exists-p pdf-file))
	  (org-open-file pdf-file))))))


;; * Convenience

(defun pdf-utils-toggle-download ()
  "Toggle the setting of `pdf-utils-download'.
I find this useful when downloading the pdfs slows down adding a
lot of references; then you just toggle it off."
  (interactive)
  (message "Setting pdf-utils-download to %s"
	   (setq pdf-utils-download (not pdf-utils-download))))

(provide 'pdf-utils)
;;; pdf-utils.el ends here
