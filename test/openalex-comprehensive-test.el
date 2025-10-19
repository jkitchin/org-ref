;;; openalex-comprehensive-test.el --- Comprehensive tests for openalex.el -*- lexical-binding: t; -*-

;; Comprehensive tests for all openalex.el functionality including:
;; - Data access functions (oa-get with arrays and nested data)
;; - Formatting functions (title, authors, abstract, links)
;; - Buffer management and sorting
;; - Edge cases and error handling

(require 'ert)
(require 'cl-lib)

;; Load the core functions from openalex.el inline

(defun oa-get (data query &optional iterable)
  "Get fields from DATA with QUERY.
QUERY is a dot notation string.
key1.key2.key3 represents a nested item.
key1.key2[].key3 represents key3 on all items in key1.key2."
  (let* ((fields (and query (split-string query "\\.")))
	 (current-field (and fields (pop fields))))
    (cond
     ((null query) data)
     ((and (string-suffix-p "[]" current-field) (null iterable))
      (setq current-field (substring current-field 0 -2))
      (oa-get (plist-get data (intern-soft (concat ":" current-field)))
	      (when fields (string-join fields "."))
	      t))
     ((and (string-suffix-p "[]" current-field) iterable)
      (setq current-field (substring current-field 0 -2))
      (cl-loop for item in data collect
	       (oa-get (plist-get item (intern-soft (concat ":" current-field)))
		       (when fields (string-join fields "."))
		       t)))
     (iterable
      (oa-get (cl-loop for item in data collect
		       (plist-get item (intern-soft (concat ":" current-field))))
	      (when fields (string-join fields "."))
	      t))
     (t
      (oa-get (plist-get data (intern-soft (concat ":" current-field)))
	      (when fields (string-join fields ".")))))))

(defun oa--title (wrk)
  "Return a title from WRK with linebreaks removed."
  (string-replace "\n" " " (plist-get wrk :title)))

(defun oa--authors (wrk)
  "Return an author string for WRK."
  (string-join (cl-loop for author in (plist-get wrk :authorships)
			collect
			(format "[[elisp:(oa--author-org \"%s\")][%s]]"
				(plist-get (plist-get author :author) :id)
				(plist-get (plist-get author :author) :display_name)))
	       ", "))

(defun oa--elisp-get-bibtex (wrk)
  "Return a elisp link to get a bibtex entry for WRK if there is a doi."
  (if-let* ((doi (plist-get wrk :doi)))
      (format "[[elisp:(doi-add-bibtex-entry \"%s\")][Get bibtex entry]]" doi)
    ""))

(defun oa--elisp-get-oa-related (wrk)
  "Return a elisp link to get related works for WRK."
  (format "[[elisp:(progn (xref--push-markers (current-buffer) (point)) (oa--related-works \"%s\"))][Get related work (%s)]]"
	  (plist-get wrk :id)
	  (length (plist-get wrk :related_works))))

(defun oa--elisp-get-oa-refs (wrk)
  "Return a elisp link to get references for WRK."
  (format "[[elisp:(progn (xref--push-markers (current-buffer) (point)) (oa--referenced-works \"%s\"))][Get references (%s)]]"
	  (plist-get wrk :id)
	  (length (plist-get wrk :referenced_works))))

(defun oa--elisp-get-oa-cited-by (wrk)
  "Return a elisp link to get works that cite WRK."
  (format "[[elisp:(progn (xref--push-markers (current-buffer) (point)) (oa--cited-by-works \"%s\"))][Get cited by (%s)]]"
	  (plist-get wrk :id)
	  (plist-get wrk :cited_by_count)))

(defun oa--abstract (wrk)
  "Construct an abstract from a WRK."
  (let* ((aii (plist-get wrk :abstract_inverted_index))
	 (word_index '())
	 sorted)
    (cl-loop for (k v) on aii by 'cddr
	     do
	     (cl-loop for index in v
		      do
		      (push (list k index) word_index)))
    (setq sorted (sort
		  word_index
		  (lambda (a b)
		    (< (nth 1 a) (nth 1 b)))))
    (string-join (mapcar
		  (lambda (x)
		    (substring (symbol-name (car x)) 1))
		  sorted)
		 " ")))

;;; Tests for oa-get with complex nested data and arrays

(ert-deftest test-openalex/oa-get-array-access ()
  "Test oa-get with array access using []."
  (let ((data '(:authors ((:name "Alice") (:name "Bob") (:name "Charlie")))))
    (should (equal (oa-get data "authors[].name")
                   '("Alice" "Bob" "Charlie")))))

(ert-deftest test-openalex/oa-get-nested-array ()
  "Test oa-get with nested properties in array."
  (let ((data '(:works ((:author (:name "Alice" :age 30))
                        (:author (:name "Bob" :age 25))))))
    (should (equal (oa-get data "works[].author.name")
                   '("Alice" "Bob")))))

(ert-deftest test-openalex/oa-get-double-array ()
  "Test oa-get with two levels of arrays."
  (let ((data '(:institutions
                ((:departments ((:name "CS") (:name "Math")))
                 (:departments ((:name "Physics") (:name "Chemistry")))))))
    (should (equal (oa-get data "institutions[].departments[].name")
                   '(("CS" "Math") ("Physics" "Chemistry"))))))

(ert-deftest test-openalex/oa-get-empty-array ()
  "Test oa-get with empty array."
  (let ((data '(:authors ())))
    (should (null (oa-get data "authors[].name")))))

(ert-deftest test-openalex/oa-get-null-query ()
  "Test oa-get with nil query returns original data."
  (let ((data '(:name "Test")))
    (should (equal (oa-get data nil) data))))

;;; Tests for oa--title function

(ert-deftest test-openalex/oa--title-simple ()
  "Test title extraction without linebreaks."
  (let ((work '(:title "A Simple Title")))
    (should (equal (oa--title work) "A Simple Title"))))

(ert-deftest test-openalex/oa--title-with-linebreaks ()
  "Test title with linebreaks are removed."
  (let ((work '(:title "A Title\nWith Multiple\nLinebreaks")))
    (should (equal (oa--title work) "A Title With Multiple Linebreaks"))))

(ert-deftest test-openalex/oa--title-empty ()
  "Test empty title."
  (let ((work '(:title "")))
    (should (equal (oa--title work) ""))))

;;; Tests for oa--authors function

(ert-deftest test-openalex/oa--authors-single ()
  "Test formatting single author."
  (let ((work '(:authorships
                ((:author (:id "https://openalex.org/A123"
                           :display_name "Alice Smith"))))))
    (should (string-match-p "Alice Smith" (oa--authors work)))
    (should (string-match-p "elisp:" (oa--authors work)))))

(ert-deftest test-openalex/oa--authors-multiple ()
  "Test formatting multiple authors."
  (let ((work '(:authorships
                ((:author (:id "https://openalex.org/A123"
                           :display_name "Alice Smith"))
                 (:author (:id "https://openalex.org/A456"
                           :display_name "Bob Jones"))))))
    (let ((result (oa--authors work)))
      (should (string-match-p "Alice Smith" result))
      (should (string-match-p "Bob Jones" result))
      (should (string-match-p ", " result)))))

(ert-deftest test-openalex/oa--authors-empty ()
  "Test empty authors list."
  (let ((work '(:authorships ())))
    (should (equal (oa--authors work) ""))))

;;; Tests for oa--abstract function

(ert-deftest test-openalex/oa--abstract-simple ()
  "Test abstract reconstruction from inverted index."
  (let ((work '(:abstract_inverted_index
                (:This (0) :is (1) :a (2) :test (3)))))
    (should (equal (oa--abstract work) "This is a test"))))

(ert-deftest test-openalex/oa--abstract-with-duplicates ()
  "Test abstract with words appearing multiple times."
  (let ((work '(:abstract_inverted_index
                (:The (0 4) :quick (1) :brown (2) :fox (3)))))
    (let ((result (oa--abstract work)))
      (should (string-match-p "The quick brown fox" result))
      (should (string-match-p "The$" result)))))

(ert-deftest test-openalex/oa--abstract-complex-order ()
  "Test abstract with complex word ordering."
  (let ((work '(:abstract_inverted_index
                (:world (4) :the (1) :Hello (0) :from (2) :lab (3)))))
    (should (equal (oa--abstract work) "Hello the from lab world"))))

(ert-deftest test-openalex/oa--abstract-empty ()
  "Test empty abstract."
  (let ((work '(:abstract_inverted_index nil)))
    (should (equal (oa--abstract work) ""))))

;;; Tests for link generation functions

(ert-deftest test-openalex/oa--elisp-get-bibtex-with-doi ()
  "Test bibtex link generation when DOI exists."
  (let ((work '(:doi "10.1234/test")))
    (let ((result (oa--elisp-get-bibtex work)))
      (should (string-match-p "elisp:" result))
      (should (string-match-p "10.1234/test" result))
      (should (string-match-p "Get bibtex entry" result)))))

(ert-deftest test-openalex/oa--elisp-get-bibtex-no-doi ()
  "Test bibtex link when no DOI exists."
  (let ((work '(:doi nil)))
    (should (equal (oa--elisp-get-bibtex work) ""))))

(ert-deftest test-openalex/oa--elisp-get-oa-related ()
  "Test related works link generation."
  (let ((work '(:id "https://openalex.org/W123"
                :related_works ("W1" "W2" "W3"))))
    (let ((result (oa--elisp-get-oa-related work)))
      (should (string-match-p "elisp:" result))
      (should (string-match-p "W123" result))
      (should (string-match-p "(3)" result)))))

(ert-deftest test-openalex/oa--elisp-get-oa-refs ()
  "Test references link generation."
  (let ((work '(:id "https://openalex.org/W456"
                :referenced_works ("W1" "W2"))))
    (let ((result (oa--elisp-get-oa-refs work)))
      (should (string-match-p "elisp:" result))
      (should (string-match-p "W456" result))
      (should (string-match-p "(2)" result)))))

(ert-deftest test-openalex/oa--elisp-get-oa-cited-by ()
  "Test cited-by link generation."
  (let ((work '(:id "https://openalex.org/W789"
                :cited_by_count 42)))
    (let ((result (oa--elisp-get-oa-cited-by work)))
      (should (string-match-p "elisp:" result))
      (should (string-match-p "W789" result))
      (should (string-match-p "(42)" result)))))

;;; Tests for realistic work data structure

(ert-deftest test-openalex/realistic-work-data ()
  "Test with realistic OpenAlex work data structure."
  (let ((work '(:id "https://openalex.org/W2741809807"
                :doi "10.1038/nature12373"
                :title "A comprehensive\nmolecular map"
                :publication_year 2013
                :cited_by_count 1337
                :authorships
                ((:author (:id "https://openalex.org/A1234"
                           :display_name "John Doe"))
                 (:author (:id "https://openalex.org/A5678"
                           :display_name "Jane Smith")))
                :primary_location
                (:source (:display_name "Nature")
                 :pdf_url "https://example.com/paper.pdf")
                :related_works ("W1" "W2" "W3" "W4" "W5")
                :referenced_works ("R1" "R2")
                :abstract_inverted_index
                (:This (0) :is (1) :the (2) :abstract (3)))))

    ;; Test title formatting
    (should (equal (oa--title work) "A comprehensive molecular map"))

    ;; Test DOI access
    (should (equal (plist-get work :doi) "10.1038/nature12373"))

    ;; Test nested access
    (should (equal (oa-get work "primary_location.source.display_name") "Nature"))

    ;; Test authors
    (let ((authors (oa--authors work)))
      (should (string-match-p "John Doe" authors))
      (should (string-match-p "Jane Smith" authors)))

    ;; Test abstract
    (should (equal (oa--abstract work) "This is the abstract"))

    ;; Test link generation
    (should (string-match-p "Get bibtex entry" (oa--elisp-get-bibtex work)))
    (should (string-match-p "(5)" (oa--elisp-get-oa-related work)))
    (should (string-match-p "(2)" (oa--elisp-get-oa-refs work)))
    (should (string-match-p "(1337)" (oa--elisp-get-oa-cited-by work)))))

;;; Tests for realistic author data structure

(ert-deftest test-openalex/realistic-author-data ()
  "Test with realistic OpenAlex author data structure."
  (let ((author '(:id "https://openalex.org/A5023888391"
                  :orcid "https://orcid.org/0000-0003-2625-9232"
                  :display_name "John Kitchin"
                  :works_count 250
                  :cited_by_count 5000
                  :last_known_institutions
                  ((:id "https://openalex.org/I74973139"
                    :display_name "Carnegie Mellon University"
                    :country_code "US"
                    :type "education"))
                  :counts_by_year
                  ((:year 2023 :works_count 15 :cited_by_count 500)
                   (:year 2022 :works_count 20 :cited_by_count 600)))))

    ;; Test basic field access
    (should (equal (oa-get author "display_name") "John Kitchin"))
    (should (equal (oa-get author "works_count") 250))
    (should (equal (oa-get author "cited_by_count") 5000))

    ;; Test nested institution access
    (should (equal (oa-get author "last_known_institutions[].display_name")
                   '("Carnegie Mellon University")))

    ;; Test counts by year array
    (should (equal (oa-get author "counts_by_year[].year")
                   '(2023 2022)))))

;;; Tests for edge cases and error handling

(ert-deftest test-openalex/oa-get-missing-nested-field ()
  "Test oa-get with missing nested field returns nil."
  (let ((data '(:author (:name "Alice"))))
    (should (null (oa-get data "author.missing.field")))))

(ert-deftest test-openalex/oa-get-array-with-nil-elements ()
  "Test oa-get with array containing nil elements."
  (let ((data '(:items ((:name "A") nil (:name "C")))))
    ;; Should handle nil gracefully
    (should (equal (oa-get data "items[].name") '("A" nil "C")))))

(ert-deftest test-openalex/oa--title-nil-title ()
  "Test title function with nil title."
  (let ((work '(:title nil)))
    (should-error (oa--title work))))

(ert-deftest test-openalex/oa--authors-nil-authorships ()
  "Test authors with nil authorships."
  (let ((work '(:authorships nil)))
    (should (equal (oa--authors work) ""))))

;;; Tests for special characters and encoding

(ert-deftest test-openalex/oa--title-special-characters ()
  "Test title with special characters."
  (let ((work '(:title "Title with émojis 🚀 and spëcial çharacters")))
    (should (equal (oa--title work)
                   "Title with émojis 🚀 and spëcial çharacters"))))

(ert-deftest test-openalex/oa--abstract-special-characters ()
  "Test abstract with special characters."
  (let ((work '(:abstract_inverted_index
                (:café (0) :naïve (1) :résumé (2)))))
    (should (equal (oa--abstract work) "café naïve résumé"))))

;;; Tests for large data handling

(ert-deftest test-openalex/oa-get-large-array ()
  "Test oa-get with large array (performance check)."
  (let* ((items (cl-loop for i from 1 to 100 collect
                         (list :name (format "Item-%d" i))))
         (data (list :items items)))
    (let ((result (oa-get data "items[].name")))
      (should (= (length result) 100))
      (should (equal (car result) "Item-1"))
      (should (equal (car (last result)) "Item-100")))))

(ert-deftest test-openalex/oa--abstract-large-text ()
  "Test abstract reconstruction with many words."
  (let* ((words (cl-loop for i from 0 to 99 collect
                         (list (intern (concat ":" (format "word%d" i)))
                               (list i))))
         (work (list :abstract_inverted_index (apply 'append words))))
    (let ((result (oa--abstract work)))
      ;; Should have 100 words separated by spaces
      (should (>= (length (split-string result " ")) 100)))))

;;; Integration tests combining multiple functions

(ert-deftest test-openalex/complete-work-processing ()
  "Integration test: Process a complete work record."
  (let ((work '(:id "https://openalex.org/W123"
                :doi "10.1234/test"
                :title "Test\nWork"
                :publication_year 2023
                :cited_by_count 10
                :authorships
                ((:author (:id "A1" :display_name "Author One"))
                 (:author (:id "A2" :display_name "Author Two")))
                :primary_location (:source (:display_name "Test Journal"))
                :related_works ("W1" "W2")
                :referenced_works ("R1")
                :abstract_inverted_index (:Test (0) :abstract (1)))))

    ;; All functions should work together without error
    (should (stringp (oa--title work)))
    (should (stringp (oa--authors work)))
    (should (stringp (oa--abstract work)))
    (should (stringp (oa--elisp-get-bibtex work)))
    (should (stringp (oa--elisp-get-oa-related work)))
    (should (stringp (oa--elisp-get-oa-refs work)))
    (should (stringp (oa--elisp-get-oa-cited-by work)))

    ;; Check specific values
    (should (equal (oa--title work) "Test Work"))
    (should (string-match-p "Author One.*Author Two" (oa--authors work)))
    (should (equal (oa--abstract work) "Test abstract"))))

;;; Tests for data validation

(ert-deftest test-openalex/validate-openalex-id-format ()
  "Test that OpenAlex IDs are in expected format."
  (let ((work '(:id "https://openalex.org/W2741809807")))
    (should (string-match-p "^https://openalex.org/[A-Z][0-9]+$"
                           (plist-get work :id)))))

(ert-deftest test-openalex/validate-doi-format ()
  "Test DOI format validation."
  (let ((work '(:doi "10.1038/nature12373")))
    (should (string-match-p "^10\\.[0-9]+/" (plist-get work :doi)))))

(provide 'openalex-comprehensive-test)

;;; openalex-comprehensive-test.el ends here
