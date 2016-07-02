;;; org-ref-ivy.el --- org-ref with ivy completion -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 0.8.1
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((dash "2.11.0") (ivy "0.8.0") (hydra "0.13.2") (key-chord "0") (s "1.10.0") (f "0.18.0") (parsebib "0")  (emacs "24.4"))

;; This file is not currently part of GNU Emacs.

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
(setq org-ref-completion-library 'org-ref-ivy-cite)

(require 'org-ref-ivy-cite)

(org-ref-ivy-cite-completion)

(provide 'org-ref-ivy)

;;; org-ref-ivy.el ends here
