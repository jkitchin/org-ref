;;; org-ref-reftex.el --- org-ref completion setup with reftex  -*- lexical-binding: t; -*-

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

;;; Commentary: This is a bare-bones completion engine using only org-mode and
;;; vanilla Emacs functions. It is not being further developed.

;;

;;; Code:
(require 'reftex)
(require 'reftex-cite)
(require 'org-ref-utils)

(declare-function 'org-ref-find-bibliography "org-ref-core.el")
(declare-function 'org-ref-get-bibtex-key-and-file "org-ref-core.el")
(declare-function 'org-ref-bib-citation "org-ref-core.el")

(defvar org-ref-cite-types)
(defvar org-ref-open-notes-function)
(defvar org-ref-get-pdf-filename-function)
(defvar org-ref-open-pdf-function)

;;;###autoload
(defun org-ref-reftex-completion ()
  "Use reftex and org-mode for completion."
  (interactive)
  ;; Define core functions for org-ref
  (setq org-ref-insert-link-function 'org-ref-insert-cite-link
	org-ref-insert-cite-function 'org-ref-insert-cite-link
	org-ref-insert-label-function 'org-insert-link
	org-ref-insert-ref-function 'org-insert-link
	org-ref-cite-onclick-function 'org-ref-cite-onclick-minibuffer-menu)
  (message "reftex completion in org-ref loaded."))

(org-ref-reftex-completion)
(define-key org-mode-map
  (kbd org-ref-insert-cite-key)
  org-ref-insert-link-function)

;; Messages in the minbuffer conflict with the minibuffer menu. So we turn them
;; off.
(setq org-ref-show-citation-on-enter nil)

;;* org-mode / reftex setup
(defun org-mode-reftex-setup ()
  "Setup `org-mode' and reftex for `org-ref'."
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (global-auto-revert-mode t))
  (make-local-variable 'reftex-cite-format)
  (setq reftex-cite-format 'org))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)


(eval-after-load 'reftex-vars
  '(progn
     (add-to-list 'reftex-cite-format-builtin
                  '(org "Org-mode citation"
                        ((?\C-m . "cite:%l")     ; default
                         (?d . ",%l")            ; for appending
                         (?a . "autocite:%l")
                         (?t . "citet:%l")
                         (?T . "citet*:%l")
                         (?p . "citep:%l")
                         (?P . "citep*:%l")
                         (?h . "citeauthor:%l")
                         (?H . "citeauthor*:%l")
                         (?y . "citeyear:%l")
                         (?x . "citetext:%l")
                         (?n . "nocite:%l"))))))


(defun org-ref-insert-cite-link (alternative-cite)
  "Insert a default citation link using reftex.
If you are on a link, it appends to the end of the link,
otherwise, a new link is inserted.  Use a prefix
arg (ALTERNATIVE-CITE) to get a menu of citation types."
  (interactive "P")
  (org-ref-find-bibliography)
  (let* ((object (org-element-context))
         (link-string-end (org-element-property :end object)))

    (if (not alternative-cite)

        (cond
         ;; case where we are in a link
         ((and (equal (org-element-type object) 'link)
               (-contains? org-ref-cite-types
			   (org-element-property :type object)))
          (goto-char link-string-end)
          ;; sometimes there are spaces at the end of the link
          ;; this code moves point pack until no spaces are there
          (skip-chars-backward " ")
          (insert (concat "," (mapconcat
			       'identity
			       (reftex-citation t ?a) ","))))

         ;; We are next to a link, and we want to append
         ((save-excursion
            (backward-char)
            (and (equal (org-element-type (org-element-context)) 'link)
                 (-contains? org-ref-cite-types
                             (org-element-property
			      :type (org-element-context)))))
          (skip-chars-backward " ")
          (insert (concat "," (mapconcat
			       'identity
			       (reftex-citation t ?a) ","))))

         ;; insert fresh link
         (t
          (insert
           (concat org-ref-default-citation-link
                   ":"
                   (mapconcat 'identity (reftex-citation t) ",")))))

      ;; you pressed a C-u so we run this code
      (reftex-citation))))


;;;###autoload
(defun org-ref-open-notes-from-reftex ()
  "Call reftex, and open notes for selected entry."
  (interactive)
  ;; now look for entry in the notes file
  (if  org-ref-bibliography-notes
      (find-file-other-window org-ref-bibliography-notes)
    (error "Org-ref-bib-bibliography-notes is not set to anything"))

  (org-open-link-from-string
   (format "[[#%s]]" (first (reftex-citation t))))
  (funcall org-ref-open-notes-function))

(defalias 'ornr 'org-ref-open-notes-from-reftex)

;;*** Minibuffer menu
;;;###autoload
(defun org-ref-cite-onclick-minibuffer-menu (&optional _link-string)
  "Action when a cite link is clicked on.
Provides a menu of context sensitive actions.  If the bibtex entry
has a pdf, you get an option to open it.  If there is a doi, you
get a lot of options.  LINK-STRING is used by the link function."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key))
         (bibfile (cdr results))
         (url (save-excursion
                (with-temp-buffer
                  (insert-file-contents bibfile)
                  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                  (bibtex-search-entry key)
                  (bibtex-autokey-get-field "url"))))
         (doi (save-excursion
                (with-temp-buffer
                  (insert-file-contents bibfile)
                  (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                  (bibtex-search-entry key)
                  ;; I like this better than bibtex-url which does not always find
                  ;; the urls
                  (bibtex-autokey-get-field "doi")))))

    (when (string= "" doi) (setq doi nil))
    (when (string= "" url) (setq url nil))
    (setq org-ref-cite-menu-funcs '())

    ;; open action
    (when
        bibfile
      (add-to-list
       'org-ref-cite-menu-funcs
       '("o" "pen" org-ref-open-citation-at-point)))

    ;; pdf
    (when (file-exists-p pdf-file)
      (add-to-list
       'org-ref-cite-menu-funcs
       `("p" "df" ,org-ref-open-pdf-function) t))

    ;; notes
    (add-to-list
     'org-ref-cite-menu-funcs
     '("n" "otes" org-ref-open-notes-at-point) t)

    ;; url
    (when (or url doi)
      (add-to-list
       'org-ref-cite-menu-funcs
       '("u" "rl" org-ref-open-url-at-point) t))

    ;; doi funcs
    (when doi
      (add-to-list
       'org-ref-cite-menu-funcs
       '("w" "os" org-ref-wos-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("c" "iting" org-ref-wos-citing-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("r" "elated" org-ref-wos-related-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("g" "oogle scholar" org-ref-google-scholar-at-point) t)

      (add-to-list
       'org-ref-cite-menu-funcs
       '("P" "ubmed" org-ref-pubmed-at-point) t))

    ;; add user functions
    (dolist (tup org-ref-user-cite-menu-funcs)
      (add-to-list
       'org-ref-cite-menu-funcs
       tup t))

    ;; finally quit
    (add-to-list
     'org-ref-cite-menu-funcs
     '("q" "uit" (lambda ())) t)

    ;; now we make a menu
    ;; construct menu string as a message
    (message
     (concat
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (bibfile (cdr results)))
        (save-excursion
          (with-temp-buffer
            (insert-file-contents bibfile)
            (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
            (bibtex-search-entry key)
            (org-ref-bib-citation))))
      "\n"
      (mapconcat
       (lambda (tup)
         (concat "[" (elt tup 0) "]"
                 (elt tup 1) " "))
       org-ref-cite-menu-funcs "")))
    ;; get the input
    (let* ((input (read-char-exclusive))
           (choice (assoc
                    (char-to-string input) org-ref-cite-menu-funcs)))
      ;; now run the function (2nd element in choice)
      (when choice
        (funcall
         (elt
          choice
          2))))))


(provide 'org-ref-reftex)
;;; org-ref-reftex.el ends here
