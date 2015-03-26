;;; wos.el --- WEb of Science functions              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  John Kitchin

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

;; Adds a new org-mode link for a search in Web of Science.


;;; Code:
(org-add-link-type
 "wos-search"
 (lambda (path)
   (browse-url
    (format  "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
             (s-join "+"
              (split-string path)))))
 ;; formatting function. Assume html
 (lambda (link desc format)
   (format "<a href=\"%s\">%s</a>"
           (format  "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
             (s-join "+"
              (split-string link)))
           (format "wos:%s" link)
           )))


(defun wos-search ()
  "Open the word at point or selection in Web of Science."
  ;; the url was derived from this page: http://wokinfo.com/webtools/searchbox/
  (interactive)
  (browse-url
   (format "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
    (if (region-active-p)
	(mapconcat 'identity (split-string
			      (buffer-substring (region-beginning)
						(region-end))) "+")
      (thing-at-point 'word)))))


(defun wos ()
  "Open Web of Science search page in a browser."
  (interactive)
  (browse-url "http://apps.webofknowledge.com"))

(provide 'wos)
;;; wos.el ends here
