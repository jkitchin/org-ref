;;; org-ref-ivy.el --- org-ref with ivy completion -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 1.0
;; Keywords: org-mode, cite, ref, label

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

(require 'org-ref-core)
(require 'org-ref-bibliography-links)
(require 'org-ref-citation-links)
(require 'org-ref-ref-links)
(require 'org-ref-label-link)
(require 'org-ref-misc-links)
(require 'org-ref-utils)
(require 'org-ref-bibtex)
(require 'bibtex-utils)

(setq org-ref-insert-link-function 'org-ref-insert-link
      org-ref-insert-cite-function 'org-ref-cite-insert
      org-ref-insert-label-function nil
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)

(provide 'org-ref-ivy)

;;; org-ref-ivy.el ends here
