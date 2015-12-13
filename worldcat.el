;;; worldcat.el --- Worldcat helpers

;; https://www.worldcat.org/wcpa/content/affiliate/default.jsp


;; https://www.worldcat.org/affiliate/tools?atype=text

;; Status ACTIVE
;; Environment Sandbox
;; Expiration Date 06/27/2016
;; Registry ID 128807
;; Override Institutions
;; Redirect URI
;; Services
;; WorldCat Search API (wcapi)
;; View API Documentation
;; Sandbox Info: Requests limited to 100/day


;;; Commentary:
;;

;;; Code:

(defun worldcat-query-all (query)
  "Open browser to Worldcat QUERY."
  (browse-url
   (format
    "http://www.worldcat.org/search?qt=worldcat_org_all&q=%s"
    query)))

(provide 'worldcat)

;;; worldcat.el ends here
