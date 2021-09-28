;;; org-ref-zotero.el --- Zotero utilities for org-mode        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; This file was contributed by Mohammad Pedramfar <https://github.com/mpedramfar>

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
;; this library provides functions to connect to an instance of the Zotero
;; translation server and get bibtex entries from urls.

;;; Code:
(require 'url)
(require 'org-ref-core)
(require 'f)


(defun zot-translate-get-json (url)
  "Get citation data of URL in Zotero JSON format, using Zotero
translation server."
  (let*
      ((url-request-method "POST")
       (url-request-extra-headers '(("Content-Type" . "text/plain")))
       (url-request-data url)
       (response-buffer (url-retrieve-synchronously "http://127.0.0.1:1969/web"))
       (output (with-current-buffer response-buffer
                 (goto-char (point-min))
                 (search-forward "\n\n")
                 (delete-region (point-min) (point))
                 (buffer-string))))
    (kill-buffer response-buffer)
    (if (equal output "URL not provided")
        (user-error "URL not provided")
      output)))


(defun zot-translate-get-bibtex-from-json (json)
  "Convert Zotero JSON format to bibtex, using Zotero translation
server."
  (let*
      ((url-request-method "POST")
       (url-request-extra-headers '(("Content-Type" . "application/json")))
       (url-request-data json)
       (response-buffer
        (url-retrieve-synchronously "http://127.0.0.1:1969/export?format=bibtex"))
       (output (with-current-buffer response-buffer
                 (goto-char (point-min))
                 (search-forward "\n\n")
                 (delete-region (point-min) (point))
                 (buffer-string))))
    (kill-buffer response-buffer)
    (if (equal output "Bad Request")
        (user-error "Bad Request")
      output)))


(defun zot-translate-get-bibtex (url)
  "Get bibtex data for URL using Zotero translation server."
  (zot-translate-get-bibtex-from-json (zot-translate-get-json url)))


(defun zot-translate-add-bibtex-entry (url bibfile)
  "Get bibtex entry for URL using Zotero translation server. Then
add it to BIBFILE"
  (interactive
   (list (read-string
          "url: "
          (ignore-errors (current-kill 0 t)))
         ;;  now get the bibfile to add it to
         (completing-read
          "Bibfile: "
          (append (f-entries "." (lambda (f) (f-ext? f "bib")))
                  org-ref-default-bibliography))))
  (save-window-excursion
    (find-file bibfile)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (insert (zot-translate-get-bibtex url))
    (org-ref-clean-bibtex-entry)
    (goto-char (point-max))
    (when (not (looking-at "^")) (insert "\n"))
    (save-buffer)))


;;* The end
(provide 'org-ref-zotero)

;;; org-ref-zotero.el ends here
