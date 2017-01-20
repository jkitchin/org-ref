;;; org-ert.el --- org+ert integration


;;; Commentary:
;; 

(require 'ert)
(require 'lispy)
;;; Code:

(defun org-src-test-p ()
  "Return if the src block at point is a :test and not ignored."
  (and (assoc :test (nth 2 (org-babel-get-src-block-info)))
       (not (string= "ignore" (cdr (assoc :test (nth 2 (org-babel-get-src-block-info))))))))


(defun org-goto-next-test-block ()
  "Move point the next :test block."
  (catch 'test
    (while (ignore-errors (org-babel-next-src-block))
      (when (org-src-test-p)
	(throw 'test t)))))


(defun org-babel-goto-nth-test-block (n)
  "Move point to the Nth test block."
  (goto-char (point-min))
  (loop for i from 1 to n do (org-goto-next-test-block))
  (recenter-top-bottom 1))


(defun org-babel-get-results (&optional run)
  "Get the results from the current src block.
Use a prefix arg RUN to run the block first.
Returns the result unless an error comes up, and then it returns 'error."
  (interactive "P")
  (condition-case err
      (progn
	(when run
	  (org-babel-execute-maybe))
	(goto-char (org-babel-where-is-src-block-result))
	(org-babel-read-result))
    (error 'error)))


(defun org-ert-run-tests ()
  (interactive)
  (save-excursion
    (let ((results '())
	  (i 0))
      (while (org-goto-next-test-block)
	(incf i)
	(org-babel-remove-result)
	(save-restriction
	  (org-narrow-to-block)
	  (setq
	   results
	   (append results
		   (list (list
			  (format "[[elisp:(org-babel-goto-nth-test-block %s)][%s]]"
				  i
				  (or (nth 4 (org-babel-get-src-block-info))
				      (format "test-%s" i)))
			  (org-babel-get-results t)))))))
      results)))


;;;###autoload
(defun org-ert-tangle (fname)
  "Tangle :test blocks out to FNAME."
  (interactive
   (list (read-file-name "test file: " "."
			 (concat (file-name-base (buffer-file-name)) ".el"))))
  (save-excursion
    (goto-char (point-min))
    (let ((test-string "")
	  (i 0))
      (while (org-goto-next-test-block)
	(incf i)
	(setq test-string
	      (concat test-string
		      (format "(ert-deftest %s ()\n%s)\n\n"
			      (or (nth 4 (org-babel-get-src-block-info))
				  (format "test-%s" i)) 
			      (nth 1 (org-babel-get-src-block-info))))))
      (with-temp-file fname
	(insert test-string)
	(lispy--indent-region (point-min) (point-max))))))




(defconst scimax-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where the scimax is installed.")



(defun org-ert-tangle-tests ()
  (let ((org (expand-file-name "test/all-org-test.org"
			       (file-name-directory (locate-library "org-ref"))))
	(el (expand-file-name "test/all-org-test.el"
			     (file-name-directory (locate-library "org-ref")))))
  (find-file org)
  (org-ert-tangle el)))

(provide 'org-ert)

;;; org-ert.el ends here

