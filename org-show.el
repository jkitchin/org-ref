
;;; org-show.el --- Summary
;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Contributions from Sacha Chua.
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

;;; Commentary:
;; A simple mode for presenting org-files as slide-shows

(defvar org-show-presentation-file nil "File containing the presentation.")
(defvar org-show-slide-tag "slide" "Tag that marks slides.")
(defvar org-show-slide-tag-regexp (concat ":" (regexp-quote org-show-slide-tag) ":"))
(defvar org-show-latex-scale 4.0 "scale for latex preview")

(defvar org-show-original-latex-scale
  (plist-get org-format-latex-options :scale)
  "Original scale for latex preview, so we can reset it.")

(defvar org-show-text-scale 4 "scale for text in presentation")
(defvar org-show-current-slide-number 1 "holds current slide number")

(defvar org-show-mogrify-p (executable-find "mogrify"))

(defvar org-show-tags-column -60 "column position to move tags to in slide mode")
(defvar org-show-original-tags-column org-tags-column "Save value so we can change back to it")

(when org-show-mogrify-p (require 'eimp))

(require 'easymenu)

(defvar org-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [next] 'org-show-next-slide)
    (define-key map [prior] 'org-show-previous-slide)
    
    (define-key map [f5] 'org-show-start-slideshow)
    (define-key map [f6] 'org-show-execute-slide)
    (define-key map (kbd "C--") 'org-show-decrease-text-size)
    (define-key map (kbd "C-=") 'org-show-increase-text-size)
    (define-key map (kbd "\e\eg") 'org-show-goto-slide)
    (define-key map (kbd "\e\et") 'org-show-toc)
    (define-key map (kbd "\e\eq") 'org-show-stop-slideshow)
    map)
  "Keymap for org-show-mode.")

(easy-menu-define my-menu org-show-mode-map "My own menu"
  '("org-show"
    ["Start slide show" org-show-start-slideshow t]
    ["Next slide" org-show-next-slide t]
    ["Previous slide" org-show-previous-slide t]
    ["Open this slide" org-show-open-slide t]
    ["Goto slide" org-show-goto-slide t]
    ["Table of contents" org-show-toc t]
    ["Stop slide show"  org-show-stop-slideshow t]
))


(define-minor-mode org-show-mode
  "Minor mode for org-show

\\{org-show-mode-map}"
  :lighter " org-show"
  :global t
  :keymap org-show-mode-map)

(defvar org-show-temp-images '() "list of temporary images")

(defun org-show-execute-slide ()
  "Process slide at point.
  If it contains an Emacs Lisp source block, evaluate it.
  If it contains an image, view it in a split buffer
  Else, focus on that buffer.
  Hide all drawers."
  (interactive)
  (setq org-show-presentation-file (expand-file-name (buffer-name)))
  (delete-other-windows)  

  ;; make sure nothing is folded. This seems to be necessary to
  ;; prevent an error on narrowing then trying to make latex fragments
  ;; I think.
  (org-cycle '(64))

  (org-narrow-to-subtree)
  (visual-line-mode 1)
  (let ((heading-text (nth 4 (org-heading-components)))
        (org-format-latex-options (plist-put org-format-latex-options :scale org-show-latex-scale)))

    (set-frame-name (format "%-180s%15s%s" heading-text "slide " (cdr (assoc heading-text org-show-slide-titles))))

    ;; preview equations in the current subtree
    (org-preview-latex-fragment '(4))
    (message "") ; clear minibuffer
    (cond

     ;; view images if there is one. WE only do this this for the first one.
     ((and (goto-char (point-min))
           (re-search-forward "\\[\\[\\(.*\\.\\(jpg\\|gif\\|png\\)\\)" nil t))
      
      (unless (file-exists-p "org-show-images")
	(make-directory "org-show-images"))
      
      (let* ((png-file (match-string 1))
	     (temp-png (expand-file-name (concat "org-show-images/" (secure-hash 'sha1
					    (with-temp-buffer
					      (insert-file-contents png-file)
					      (buffer-string))) ".png"))))

        (add-to-list 'org-show-temp-images temp-png)
	(unless (file-exists-p temp-png)
	  (copy-file png-file temp-png t))
      
	(split-window-right)      
      
	(other-window 1)
	(find-file temp-png)
        (when org-show-mogrify-p
          (eimp-fit-image-width-to-window nil)))
                  
      (other-window 1) ; back to slide
      (goto-char (point-min))
      (text-scale-set org-show-text-scale)
      (org-display-inline-images)
      (org-cycle-hide-drawers t)
      (org-show-subtree))

     ;; find and execute source code blocks.
     ;; you can either have images, or code. Not both.
     ;; Only code blocks of type emacs-lisp-slide are used.
     ((and (goto-char (point-min))
           (re-search-forward "#\\+begin_src emacs-lisp-slide" nil t))
      (let ((info (org-babel-get-src-block-info)))
        (unwind-protect
            (eval (read (concat "(progn " (nth 1 info) ")"))))))

     ;; plain text slides
     (t
      (switch-to-buffer (current-buffer))
      (text-scale-set org-show-text-scale)
      (org-show-subtree)
      (org-cycle-hide-drawers t)
      (org-display-inline-images)
      (delete-other-windows)))))

(defun org-show-next-slide ()
  "Goto next slide in presentation"
  (interactive)
  (find-file org-show-presentation-file)
  (widen)
  (if (<= (+ org-show-current-slide-number 1) (length org-show-slide-titles))
      (progn
	(setq org-show-current-slide-number (+ org-show-current-slide-number 1))
	(org-show-goto-slide org-show-current-slide-number))
    (org-show-goto-slide org-show-current-slide-number)
    (message "This is the end. My only friend the end.  Jim Morrison.")))

(defun org-show-previous-slide ()
  "Goto previous slide in the list"
  (interactive)
  (find-file org-show-presentation-file)
  (widen)
  (if (> (- org-show-current-slide-number 1) 0)
      (progn
	(setq org-show-current-slide-number (- org-show-current-slide-number 1))
	(org-show-goto-slide org-show-current-slide-number))
    (org-show-goto-slide org-show-current-slide-number)
    (message "Once upon a time...")))

(defun org-show-open-slide ()
 "Start show at this slide"
 (setq org-show-presentation-file (expand-file-name (buffer-name))) 
 (org-show-initialize)
 (let ((n (cdr (assoc (nth 4 (org-heading-components)) org-show-slide-titles))))
   (setq org-show-current-slide-number n)
   (org-show-goto-slide n)))

(defvar org-show-slide-list '() "List of slide numbers and markers to each slide")
(defvar org-show-slide-titles '() "List of titles and slide numbers for each slide")

(defun org-show-initialize ()
  ;; make slide lists for future navigation. rerun this if you change slide order
  (setq  org-show-slide-titles '()
         org-show-temp-images '()
         org-show-slide-list '())
     
  (let ((n 0))
    (org-map-entries
     (lambda ()
       (when (string-match-p ":slide:" (or (nth 5 (org-heading-components)) ""))
	 (setq n (+ n 1))
         
	 (add-to-list 'org-show-slide-titles 
		      (cons (nth 4 (org-heading-components)) n) t)

	 (add-to-list 'org-show-slide-list 
		      (cons n (set-marker (make-marker) (point))) t))))))

(defun org-show-start-slideshow ()
  "Start the slide show, at the beginning"
  (interactive)
    
  (setq org-show-presentation-file (expand-file-name (buffer-name)))
  (beginning-of-buffer)
  (setq org-tags-column org-show-tags-column)
  (org-set-tags-command '(4) t)

  (org-show-initialize)
  ;; hide slide tags
  (save-excursion
    (while (re-search-forward ":slide:" nil t)
      (overlay-put
       (make-overlay (match-beginning 0)(match-end 0))
       'invisible 'slide)))
  (add-to-invisibility-spec 'slide)
  (beginning-of-buffer)
  (delete-other-windows)
  (org-show-mode 1)
  (setq org-show-current-slide-number 1)
  (org-show-goto-slide 1))

(defun org-show-stop-slideshow ()
  (interactive)
  ;; make slide tag visible again
  (remove-from-invisibility-spec 'slide)

  ;; reset latex scale
  (plist-put org-format-latex-options :scale org-show-original-latex-scale)

  ;; clean up temp images
  (mapcar (lambda (x)
	    (let ((bname (file-name-nondirectory x)))
	      (when (get-buffer bname)
                (set-buffer bname) 
                (save-buffer)
		(kill-buffer bname)))

	    (when (file-exists-p x)
	      (delete-file x)))
	  org-show-temp-images)
  (setq org-show-temp-images '())

  ;; ;; clean up miscellaneous buffers
  (when (get-buffer "*Animation*") (kill-buffer "*Animation*"))

  (when org-show-presentation-file (find-file org-show-presentation-file))
  (widen)
  (text-scale-set 0)
  (delete-other-windows)
  (setq org-show-presentation-file nil)
  (setq org-show-current-slide-number 1)
  (set-frame-name (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))))
  (setq org-tags-column org-show-original-tags-column)
  (org-set-tags-command '(4) t)

  (org-show-mode -1))

(defalias 'stop 'org-show-stop-slideshow)

(defun org-show-goto-slide (n)
 "Goto slide N"
 (interactive "nSlide number: ")
 (message "Going to slide %s" n)
 (find-file org-show-presentation-file)
 (setq org-show-current-slide-number n)
 (widen)
 (goto-char (cdr (assoc n org-show-slide-list)))
 (org-show-execute-slide))

(defun org-show-toc ()
  (interactive)
  (let ((links) (c-b (buffer-name)) (n))
    (save-excursion
      (widen)
      (mapcar
       (lambda (x)
	 (setq n (car x))
	 (goto-char (cdr x))
	 (add-to-list
	  'links
	  (format " [[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s)(org-show-execute-slide))][%2s %s]]\n\n"
		  (marker-buffer (cdr x))
		  (marker-position (cdr x))
		  (car x)
		  (nth 4 (org-heading-components))) t))
	      org-show-slide-list))
    
    (switch-to-buffer "*List of Slides*")
    (org-mode)
    (erase-buffer)
    
    (insert (mapconcat 'identity links ""))
  
    ;(setq buffer-read-only t)
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))))

(require 'animate)

(defun org-show-animate (strings)
  "Animate STRINGS in an *Animation* buffer"
  (switch-to-buffer (get-buffer-create
                     (or animation-buffer-name
                         "*Animation*")))
  (erase-buffer)
  (text-scale-set 6)
  (let* ((vpos (/ (- 20
		     1 ;; For the mode-line
		     (1- (length strings)) 
		     (length strings))
		  2))
	 (width 43)
	 hpos)
    (while strings
      (setq hpos (/ (- width (length (car strings))) 2))
      (when (> 0 hpos) (setq hpos 0))
      (when (> 0 vpos) (setq vpos 0))
      (animate-string (car strings) vpos hpos)
      (setq vpos (1+ vpos))
      (setq strings (cdr strings)))))

(defun org-show-increase-text-size (&optional arg)
  "Increase text size. Bound to \\[org-show-increase-text-size].

With prefix ARG, set `org-show-text-scale' so subsquent slides are the same text size."
  (interactive "P")
  (text-scale-increase 1.5)
  (when arg
    (setq org-show-text-scale (* org-show-text-scale 1.5))))

(defun org-show-decrease-text-size (&optional arg)
  "Increase text size. Bound to \\[org-show-decrease-text-size].

With prefix ARG, set `org-show-text-scale' so subsquent slides are the same text size."
  (interactive "P")
  (text-scale-decrease 1.5)
  (when arg
    (setq org-show-text-scale (/ org-show-text-scale 1.5)))
)

(provide 'org-show)
