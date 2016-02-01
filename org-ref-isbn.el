;;; org-ref-isbn.el --- utilities for generating bibtex entries from an ISBN  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords: convenience

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

;; Provides functions to download bibtex entries from isbn numbers.

;;; Code:
(require 'f)
(require 'org)
(require 'org-ref)

;; byte-compile
(defvar-local url-http-end-of-headers nil)

;;* ISBN utility

;; I found this on the web. It can be handy, but the bibtex entry has a lot of stuff in it.

;;;###autoload
(defun isbn-to-bibtex-lead (isbn)
  "Search lead.to for ISBN bibtex entry.
You have to copy the entry if it is on the page to your bibtex
file."
  (interactive "sISBN: ")
  (browse-url
   (format
    "http://lead.to/amazon/en/?key=%s+&si=all&op=bt&bn=&so=sa&ht=us"
    isbn)))

;; Here we get isbn metadata and build a bibtex entry.
;; http://xisbn.worldcat.org/xisbnadmin/doc/api.htm#getmetadata

;;;###autoload
(defun isbn-to-bibtex (isbn bibfile)
  "Get bibtex entry for ISBN and insert it into BIBFILE.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from worldcat."
  (interactive
   (list
    (read-string
     "ISBN: "
     ;; now set initial input
     (cond
      ;; If region is active and it starts with a number, we use it
      ((and  (region-active-p)
             (s-match "^[0-9]" (buffer-substring (region-beginning) (region-end))))
       (buffer-substring (region-beginning) (region-end)))
      ;; if first entry in kill ring starts with a number assume it is an isbn
      ;; and use it as the guess
      ((if (s-match "^[0-9]" (car kill-ring))
           (car kill-ring)))
      ;; type or paste it in
      (t
       nil)))
    (ido-completing-read
     "Bibfile: "
     (append (f-entries "." (lambda (f) (f-ext? f "bib")))
             org-ref-default-bibliography))))

  (let* ((results (with-current-buffer
                      (url-retrieve-synchronously
                       (format
                        "http://xisbn.worldcat.org/webservices/xid/isbn/%s?method=getMetadata&format=json&fl=*"
                        isbn))
                    (json-read-from-string
                     (buffer-substring url-http-end-of-headers (point-max)))))
         (status (cdr (nth 1 results)))
         (metadata (aref (cdar results) 0))
         (new-entry)
         (new-key))

    ;; check if we got something
    (unless (string= "ok" status)
      (error "Status is %s" status))

    ;; construct an alphabetically sorted bibtex entry. I assume ISBN numbers go
    ;; with book entries.
    (setq new-entry
          (concat "\n@book{,\n"
                  (mapconcat
                   'identity
                   (cl-loop for field in (-sort 'string-lessp (mapcar 'car metadata))
                            collect
                            (format "  %s={%s}," field (cdr (assoc field metadata))))
                   "\n")
                  "\n}\n"))

    ;; build entry in temp buffer to get the key so we can check for duplicates
    (setq new-entry (with-temp-buffer
                      (insert new-entry)
                      (org-ref-clean-bibtex-entry)
                      (setq new-key (bibtex-key-in-head))
                      (buffer-string)))
    (find-file bibfile)
    (goto-char (point-min))
    (when (search-forward new-key nil t)
      (beep)
      (setq new-key (read-string
                     (format  "%s already exists. Enter new key (C-g to cancel): " new-key)
                     new-key)))
    (goto-char (point-max))
    (insert new-entry)
    ;; set key. It is simplest to just replace it, even if it is the same.
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
        (delete-region (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
    (insert new-key)
    (bibtex-fill-entry)
    (save-buffer)))

(provide 'org-ref-isbn)
;;; org-ref-isbn.el ends here
