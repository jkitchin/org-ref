;;; org-ref-ivy-cite.el --- Use ivy for completion in org-ref  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

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

;;

(declare-function 'org-ref-insert-key-at-point "org-ref-core.el")
(declare-function 'org-ref-find-bibliography "org-ref-core.el")
(declare-function 'org-ref-get-labels "org-ref-core.el")
(declare-function 'org-ref-get-bibtex-key-and-file "org-ref-core.el")


;;; Code:
(require 'ivy)
(require 'org-ref-bibtex)
(require 'org-ref-citeproc)
(require 'bibtex-completion)

;; This lets you customize how the completion for ivy is displayed. The default
;; is in the minibuffer. You may like to see something more like a popup though.
(defcustom org-ref-ivy-display-function nil
  "ivy function to display completion with.
Set to `ivy-display-function-overlay' to get popups at point."
  :type 'function
  :group 'org-ref)

(when org-ref-ivy-display-function
  (add-to-list 'ivy-display-functions-alist
	       `(org-ref-ivy-insert-cite-link . ,org-ref-ivy-display-function))
  (add-to-list 'ivy-display-functions-alist
	       `(org-ref-ivy-insert-label-link . ,org-ref-ivy-display-function))
  (add-to-list 'ivy-display-functions-alist
	       `(org-ref-ivy-insert-ref-link . ,org-ref-ivy-display-function)))


(defvar org-ref-cite-types)
(defvar org-ref-show-citation-on-enter)

(defvar org-ref-ivy-cite-marked-candidates '()
  "Holds entries marked in `org-ref-ivy-insert-cite-link'.")

;;;###autoload
(defun org-ref-ivy-cite-completion ()
  "Use ivy for completion."
  (interactive)
  ;; Define core functions for org-ref
  (setq org-ref-insert-link-function 'org-ref-insert-link
	org-ref-insert-cite-function 'org-ref-ivy-insert-cite-link
	org-ref-insert-label-function 'org-ref-ivy-insert-label-link
	org-ref-insert-ref-function 'org-ref-ivy-insert-ref-link
	org-ref-cite-onclick-function (lambda (_) (org-ref-cite-hydra/body))))

(org-ref-ivy-cite-completion)

(define-key org-mode-map
  (kbd org-ref-insert-cite-key)
  org-ref-insert-link-function)


(defun or-looking-forward-cite ()
  "Return if point is in the position before a citation."
  (save-excursion
    (forward-char)
    (-contains? org-ref-cite-types
		(org-element-property
		 :type
		 (org-element-context)))))


(defun or-looking-back-cite ()
  "Return if point is in the position after a citation."
  (save-excursion
    (forward-char -1)
    (-contains? org-ref-cite-types
		(org-element-property
		 :type
		 (org-element-context)))))


(defun or-ivy-bibtex-insert-cite (entry)
  "Insert a citation for ENTRY.
If `org-ref-ivy-cite-marked-candidates' is non-nil then they are added instead of ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (with-ivy-window
   (if org-ref-ivy-cite-marked-candidates
       (cl-loop for entry in org-ref-ivy-cite-marked-candidates
	        do
	        (if ivy-current-prefix-arg
		    (let ((org-ref-default-citation-link (ivy-read "Type: " org-ref-cite-types)))
		      (org-ref-insert-key-at-point (list (cdr (assoc "=key=" entry)))))
		  (org-ref-insert-key-at-point (list (cdr (assoc "=key=" entry))))))
     (if ivy-current-prefix-arg
	 (let ((org-ref-default-citation-link (ivy-read "Type: " org-ref-cite-types)))
	   (org-ref-insert-key-at-point (list (cdr (assoc "=key=" entry)))))
       (org-ref-insert-key-at-point (list (cdr (assoc "=key=" entry))))))))


(defun or-ivy-bibtex-open-pdf (entry)
  "Open the pdf associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (with-ivy-window
    (let ((pdf (expand-file-name
		(format "%s.pdf"
			(cdr (assoc "=key=" entry)))
		org-ref-pdf-directory)))
      (if (file-exists-p pdf)
	  (org-open-file pdf)
	(message "No pdf found for %s" (cdr (assoc "=key=" entry)))))))


(defun or-ivy-bibtex-open-notes (entry)
  "Open the notes associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (with-ivy-window
    (org-ref-open-notes-at-point
     (cdr (assoc "=key=" entry)))))


(defun or-ivy-bibtex-open-entry (entry)
  "Open the bibtex file at ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (find-file (cdr (assoc "bibfile" entry)))
  (goto-char (cdr (assoc "position" entry)))
  (bibtex-beginning-of-entry))


(defun or-ivy-bibtex-copy-entry (entry)
  "Copy selected bibtex ENTRY to the clipboard."
  (with-temp-buffer
    (save-window-excursion
      (or-ivy-bibtex-open-entry entry)
      (bibtex-copy-entry-as-kill))
    (bibtex-yank)
    (kill-region (point-min) (point-max))))


(defun or-ivy-bibtex-open-url (entry)
  "Open the URL associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (let ((url (cdr (assoc "url" entry))))
    (if url
	(browse-url url)
      (message "No url found for %s" (cdr (assoc "=key=" entry))))))


(defun or-ivy-bibtex-open-doi (entry)
  "Open the DOI associated with ENTRY.
ENTRY is selected from `orhc-bibtex-candidates'."
  (let ((doi (cdr (assoc "doi" entry))))
    (if doi
	(browse-url (format "http://dx.doi.org/%s" doi))
      (message "No doi found for %s" (cdr (assoc "=key=" entry))))))


(defun or-ivy-bibtex-set-keywords (entry)
  "Prompt for keywords, and put them on the selected ENTRY."
  (let ((keywords (read-string "Keyword(s) comma-separated: " ))
	entry-keywords)
    (save-window-excursion
      (or-ivy-bibtex-open-entry entry)
      (setq entry-keywords (bibtex-autokey-get-field "keywords"))
      (bibtex-set-field
       "keywords"
       (if (> (length entry-keywords) 0)
	   (concat entry-keywords ", " keywords)
	 keywords)))))


(defun or-ivy-bibtex-email-entry (entry)
  "Insert selected ENTRY and attach pdf file to an email.
Create email unless called from an email."
  (with-ivy-window
    (let ((goto-to nil))
      (unless (memq major-mode '(message-mode mu4e-compose-mode))
	(setq goto-to t)
	(compose-mail)
	(message-goto-body))
      (save-window-excursion
	(or-ivy-bibtex-open-entry entry)
	(bibtex-copy-entry-as-kill))
      (insert (pop bibtex-entry-kill-ring))
      (insert "\n")
      (let ((pdf (expand-file-name
		  (format "%s.pdf"
			  (cdr (assoc "=key=" entry)))
		  org-ref-pdf-directory)))
	(if (file-exists-p pdf)
	    (mml-attach-file pdf)))
      (when goto-to
	(message-goto-to)))))


(defun or-ivy-bibtex-formatted-citation (entry)
  "Return string containing formatted citations for ENTRY.
This uses a citeproc library."
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Style: " '("unsrt" "author-year")
	      :action 'load-library
	      :require-match t
	      :preselect "unsrt"
	      :caller 'or-ivy-formatted-citation)
    (format "%s\n\n" (orhc-formatted-citation entry))))


(defun or-ivy-bibtex-insert-formatted-citation (entry)
  "Insert formatted citations at point for selected entries."
  (with-ivy-window
    (insert (mapconcat
	     'identity
	     (cl-loop for entry in (or org-ref-ivy-cite-marked-candidates (list entry))
		      collect (org-ref-format-bibtex-entry entry))
	     "\n\n"))))


(defun or-ivy-bibtex-copy-formatted-citation (entry)
  "Copy formatted citation to clipboard for ENTRY."
  (kill-new (org-ref-format-entry (cdr (assoc "=key=" entry)))))


(defun or-ivy-bibtex-add-entry (_)
  "Open a bibliography file and move point to the end, in order
to add a new bibtex entry. The arg is selected from
`orhc-bibtex-candidates' but ignored."
  (ivy-read "bibtex file: " org-ref-bibtex-files
	    :require-match t
	    :action 'find-file
	    :caller 'or-ivy-bibtex-add-entry)
  (widen)
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n")))


(defvar org-ref-ivy-cite-actions
  '(("b" or-ivy-bibtex-open-entry "Open bibtex entry")
    ("B" or-ivy-bibtex-copy-entry "Copy bibtex entry")
    ("p" or-ivy-bibtex-open-pdf "Open pdf")
    ("n" or-ivy-bibtex-open-notes "Open notes")
    ("u" or-ivy-bibtex-open-url "Open url")
    ("d" or-ivy-bibtex-open-doi "Open doi")
    ("k" or-ivy-bibtex-set-keywords "Add keywords")
    ("e" or-ivy-bibtex-email-entry "Email entry")
    ("f" or-ivy-bibtex-insert-formatted-citation "Insert formatted citation")
    ("F" or-ivy-bibtex-copy-formatted-citation "Copy formatted citation")
    ("a" or-ivy-bibtex-add-entry "Add bibtex entry"))
  "List of additional actions for `org-ref-ivy-insert-cite-link' (the default action being to insert a citation).")

(defvar org-ref-ivy-cite-re-builder 'ivy--regex-ignore-order
  "Regex builder to use in `org-ref-ivy-insert-cite-link'. Can be set to nil to use Ivy's default).")

(defun org-ref-swap (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)

(defun org-ref-ivy-current ()
  (if (boundp 'ivy--current)
      ivy--current
    (ivy-state-current ivy-last)))

(defun org-ref-ivy-move-up ()
  "Move ivy candidate up and update candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last)
        (org-ref-swap ivy--index (1- ivy--index) (ivy-state-collection ivy-last)))
  (setf (ivy-state-preselect ivy-last) (org-ref-ivy-current))
  (ivy--reset-state ivy-last))

(defun org-ref-ivy-move-down ()
  "Move ivy candidate down."
  (interactive)
  (setf (ivy-state-collection ivy-last)
        (org-ref-swap ivy--index (1+ ivy--index) (ivy-state-collection ivy-last)))
  (setf (ivy-state-preselect ivy-last) (org-ref-ivy-current))
  (ivy--reset-state ivy-last))

(defun org-ref-ivy-sort-year-ascending ()
  "Sort entries by year in ascending order."
  (interactive)
  (setf (ivy-state-collection ivy-last)
	(cl-sort (copy-sequence (ivy-state-collection ivy-last))
		 (lambda (a b)
		   (let ((y1 (string-to-number (or (cdr (assoc "year" a)) "0")))
			 (y2 (string-to-number (or (cdr (assoc "year" b)) "0"))))
		     (< y1 y2)))))
  (setf (ivy-state-preselect ivy-last) (org-ref-ivy-current))
  (ivy--reset-state ivy-last))

(defun org-ref-ivy-sort-year-descending ()
  "sort entries by year in descending order."
  (interactive)
  (setf (ivy-state-collection ivy-last)
	(cl-sort (copy-sequence (ivy-state-collection ivy-last))
		 (lambda (a b)
		   (let ((y1 (string-to-number (or (cdr (assoc "year" a)) "0")))
			 (y2 (string-to-number (or (cdr (assoc "year" b)) "0"))))
		     (> y1 y2)))))
  (setf (ivy-state-preselect ivy-last) (org-ref-ivy-current))
  (ivy--reset-state ivy-last))

;; * marking candidates

(defun org-ref-ivy-mark-candidate ()
  "Add current candidate to `org-ref-ivy-cite-marked-candidates'.
If candidate is already in, remove it."
  (interactive)
  (let ((cand (or (assoc (org-ref-ivy-current) (ivy-state-collection ivy-last))
		  (org-ref-ivy-current))))
    (if (-contains? org-ref-ivy-cite-marked-candidates cand)
	;; remove it from the marked list
	(setq org-ref-ivy-cite-marked-candidates
	      (-remove-item cand org-ref-ivy-cite-marked-candidates))

      ;; add to list
      (setq org-ref-ivy-cite-marked-candidates
	    (append org-ref-ivy-cite-marked-candidates (list cand)))))

  (ivy-next-line))


(defun org-ref-ivy-show-marked-candidates ()
  "Show marked candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last) org-ref-ivy-cite-marked-candidates)
  (setf (ivy-state-preselect ivy-last) (org-ref-ivy-current))
  (ivy--reset-state ivy-last))


(defun org-ref-ivy-show-all ()
  "Show all the candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last)
	(orhc-bibtex-candidates))
  (ivy--reset-state ivy-last))

;; * org-ref-cite keymap

(defvar org-ref-ivy-cite-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<SPC>") 'org-ref-ivy-mark-candidate)
    (define-key map (kbd "C-,") 'org-ref-ivy-show-marked-candidates)
    (define-key map (kbd "C-.") 'org-ref-ivy-show-all)
    (define-key map (kbd "C-<up>") 'org-ref-ivy-move-up)
    (define-key map (kbd "C-<down>") 'org-ref-ivy-move-down)
    (define-key map (kbd "C-y") 'org-ref-ivy-sort-year-ascending)
    (define-key map (kbd "C-M-y") 'org-ref-ivy-sort-year-descending)
    (define-key map (kbd "C-k") (lambda ()
				  (interactive)
				  (beginning-of-line)
				  (kill-visual-line)
				  (setf (ivy-state-collection ivy-last)
					(orhc-bibtex-candidates))
				  (setf (ivy-state-preselect ivy-last) (org-ref-ivy-current))
				  (ivy--reset-state ivy-last)))
    (define-key map (kbd "C-<return>")
      (lambda ()
	"Apply action and move to next/previous candidate."
	(interactive)
	(ivy-call)
	(ivy-next-line)))
    ;; (define-key ivy-minibuffer-map (kbd "M-<return>")
    ;;   (lambda ()
    ;; 	"Apply default action to all marked candidates."
    ;; 	(interactive)
    ;; 	(mapc (ivy--get-action ivy-last)
    ;; 	      org-ref-ivy-cite-marked-candidates)
    ;; 	(ivy-exit-with-action (function (lambda (_) nil)))))
    map)
  "A key map for `org-ref-ivy-insert-cite-link'.")


(ivy-set-actions
 'org-ref-ivy-insert-cite-link
 org-ref-ivy-cite-actions)


(defun org-ref-ivy-insert-cite-link (&optional arg)
  "ivy function for interacting with bibtex.
Uses `org-ref-find-bibliography' for bibtex sources, unless a
prefix ARG is used, which uses `org-ref-default-bibliography'."
  (interactive "P")
  (setq org-ref-bibtex-files (if arg
				 org-ref-default-bibliography
			       (org-ref-find-bibliography)))
  (setq org-ref-ivy-cite-marked-candidates '())

  (ivy-read "Open: " (orhc-bibtex-candidates)
	    :require-match t
	    :keymap org-ref-ivy-cite-keymap
	    :re-builder org-ref-ivy-cite-re-builder
	    :action 'or-ivy-bibtex-insert-cite
	    :caller 'org-ref-ivy-insert-cite-link))


(defun org-ref-ivy-cite-transformer (s)
  "Make entry red if it is marked."
  (let* ((fill-column (frame-width))
	 (fill-prefix "   ")
	 (wrapped-s (with-temp-buffer
		      (insert s)
		      (fill-paragraph)
		      (buffer-string))))
    (if (-contains?
	 (if (listp (car org-ref-ivy-cite-marked-candidates))
	     (mapcar 'car org-ref-ivy-cite-marked-candidates)
	   org-ref-ivy-cite-marked-candidates)
	 s)
	(propertize wrapped-s 'face 'font-lock-warning-face)
      (propertize wrapped-s 'face nil))))

(ivy-set-display-transformer
 'org-ref-ivy-insert-cite-link
 'org-ref-ivy-cite-transformer )


(defun org-ref-ivy-insert-label-link ()
  "Insert a label with ivy."
  (interactive)
  (insert
   (concat (if (not (looking-back "label:" 6)) "label:" "")
	   (ivy-read "label: " (org-ref-get-labels)
		     :caller 'org-ref-ivy-insert-label-link))))


(defun org-ref-ivy-insert-ref-link ()
  "Insert a ref link with ivy.
Use a prefix arg to select the ref type."
  (interactive)
  (let ((label (ivy-read "label: " (org-ref-get-labels) :require-match t
			 :caller 'org-ref-ivy-insert-ref-link)))
    (cond
     ;; from a colon insert
     ((looking-back ":" 1)
      (insert label))
     ;; non-default
     (ivy-current-prefix-arg
      (insert
       (ivy-read "type: " org-ref-ref-types)
       ":"
       label))
     ;; default
     (t
      (insert
       (or (when (looking-at "$") " ") "")
       (concat (org-ref-infer-ref-type label)
	       ":"
	       label))))))

(require 'hydra)
(setq hydra-is-helpful t)

(defhydra org-ref-cite-hydra (:color blue :hint nil)
  "
_p_: Open pdf     _w_: WOS          _g_: Google Scholar _K_: Copy citation to clipboard
_u_: Open url     _r_: WOS related  _P_: Pubmed         _k_: Copy key to clipboard
_n_: Open notes   _c_: WOS citing   _C_: Crossref       _f_: Copy formatted entry
_o_: Open entry   _e_: Email entry  ^ ^                 _q_: quit
_i_: Insert cite  _h_: change type
"
  ("o" org-ref-open-citation-at-point nil)
  ("p" (funcall org-ref-open-pdf-function) nil)
  ("n" org-ref-open-notes-at-point nil)
  ("u" org-ref-open-url-at-point nil)
  ("w" org-ref-wos-at-point nil)
  ("r" org-ref-wos-related-at-point nil)
  ("c" org-ref-wos-citing-at-point nil)
  ("g" org-ref-google-scholar-at-point nil)
  ("P" org-ref-pubmed-at-point nil)
  ("C" org-ref-crossref-at-point nil)
  ("K" org-ref-copy-entry-as-summary nil)
  ("k" (progn
	 (kill-new
	  (car (org-ref-get-bibtex-key-and-file))))
   nil)
  ("f" (kill-new
	(org-ref-format-entry (org-ref-get-bibtex-key-under-cursor)))
   nil)

  ("e" (kill-new (save-excursion
		   (org-ref-open-citation-at-point)
		   (org-ref-email-bibtex-entry)))
   nil)
  ("i" (funcall org-ref-insert-cite-function))
  ("h" org-ref-change-cite-type)
  ("q" nil))


(defun org-ref-ivy-onclick-actions ()
  "An alternate click function that uses ivy for action selection.
Each action is taken from `org-ref-ivy-cite-actions'. Each action
should act on a bibtex entry that matches the key in
`orhc-bibtex-candidates'. Set `org-ref-cite-onclick-function' to
this function to use it."
  (interactive)
  (ivy-read
   "action: "
   (cl-loop for i from 0
	    for (_ func s) in
	    org-ref-ivy-cite-actions
	    collect (cons (format "%2s. %s" i s) func))
   :action (lambda (f)
	     (let* ((key (car (org-ref-get-bibtex-key-and-file)))
		    (entry (cdr (elt (orhc-bibtex-candidates)
				     (-elem-index
				      key
				      (cl-loop for entry in (orhc-bibtex-candidates)
					       collect (cdr (assoc "=key=" entry ))))))))
	       (funcall f entry)))))


;; * org-ref-ivy-set-keywords
(defvar org-ref-ivy-set-keywords-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<SPC>") 'org-ref-ivy-mark-candidate)
    (define-key map (kbd "C-,") 'org-ref-ivy-show-marked-candidates)
    (define-key map (kbd "C-.") 'org-ref-ivy-show-all)
    (define-key map (kbd "C-<up>") 'org-ref-ivy-move-up)
    (define-key map (kbd "C-<down>") 'org-ref-ivy-move-down)
    map)
  "A key map for `org-ref-ivy-set-keywords'.")

(defun org-ref-ivy-set-keywords ()
  "Add keywords to bibtex entries selected by org-ref-ivy."
  (interactive)
  (setq org-ref-ivy-cite-marked-candidates '())
  (ivy-read "Keywords: " (org-ref-bibtex-keywords)
	    :keymap org-ref-ivy-set-keywords-keymap
	    :caller 'org-ref-ivy-set-keywords
	    :action (lambda (key)
		      (org-ref-set-bibtex-keywords
		       (mapconcat
			'identity
			(or org-ref-ivy-cite-marked-candidates (list key))
			", ")))))

(ivy-set-display-transformer
 'org-ref-ivy-set-keywords
 'org-ref-ivy-cite-transformer)

(provide 'org-ref-ivy-cite)
;;; org-ref-ivy-cite.el ends here
