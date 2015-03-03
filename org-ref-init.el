;;; org-ref-init.el --- setup bibliography, cite, ref and label org-mode links.

;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
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

;;; Version: 0.1

;;; Commentary:
;;
;; Lisp code to setup bibliography cite, ref and label org-mode links.
;; also sets up reftex and helm for org-mode citations. The links are
;; clickable and do things that are useful. You should really read
;; org-ref.org for details.
;;
;; Package-Requires: ((dash) (helm) (helm-bibtex))
(require 'cl)

;; org path for loadable org-files
(defvar org-ref-org-load-path
  nil
  "List of directories to find org-files that `org-babel-load-file' can load code from.")

;; https://github.com/jkitchin/org-ref
(defun org-ref-org-require (feature)
  "Load a FEATURE from an org-file.
FEATURE is a symbol, and it is loaded from an org-file by the name of FEATURE.org, that is in the `org-load-path'.  The FEATURE is loaded from `org-babel-load-file'."
  (let ((org-file (concat (symbol-name feature) ".org"))
	(path))

    ;; find the org-file
    (catch 'result
      (loop for dir in org-ref-org-load-path do
	    (when (file-exists-p
		   (setq path
			 (expand-file-name
			  org-file
			  dir)))
	      (throw 'result path))))
    (let ((default-directory (file-name-directory path)))
      (org-babel-load-file path))))

;; remember this directory
(defconst org-ref-dir (file-name-directory (or load-file-name
					       (buffer-file-name)))
    "Directory where org-ref is installed.")

;; for loading org-files
(add-to-list 'org-ref-org-load-path org-ref-dir)
;; for loading emacs-lisp files
(add-to-list 'load-path org-ref-dir)

(org-ref-org-require 'org-ref)
(org-ref-org-require 'doi-utils)
(org-ref-org-require 'pubmed)
(require 'jmax-bibtex)

(provide 'org-ref-init)

;;; org-ref-init.el ends here
