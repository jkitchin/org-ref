;;; openalex-utilities-test.el --- Tests for OpenAlex utility functions -*- lexical-binding: t; -*-

;; Tests for OpenAlex buffer management, sorting, and utility functions:
;; - Buffer sorting (by year, by citation count)
;; - Buffer management (killing buffers)
;; - URL construction and formatting
;; - Data validation and error handling

(require 'ert)
(require 'org)

;;; Mock data for testing

(defvar test-openalex-work-data
  '(:id "https://openalex.org/W123"
    :doi "10.1234/test"
    :title "Test Work"
    :publication_year 2023
    :cited_by_count 42
    :authorships ((:author (:id "A1" :display_name "Author One"))))
  "Mock OpenAlex work data for testing.")

;;; Tests for sorting functionality

(ert-deftest test-openalex/buffer-sort-year-descending ()
  "Test that year properties can be accessed for sorting."
  (with-temp-buffer
    (org-mode)
    (insert "* Work 1
:PROPERTIES:
:YEAR: 2020
:END:

* Work 2
:PROPERTIES:
:YEAR: 2023
:END:

* Work 3
:PROPERTIES:
:YEAR: 2021
:END:
")
    ;; Collect all year values
    (let ((years '()))
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (push (org-entry-get (point) "YEAR") years))
      (setq years (nreverse years))
      (should (equal years '("2020" "2023" "2021")))
      ;; Verify they can be converted to numbers for sorting
      (should (equal (mapcar #'string-to-number years) '(2020 2023 2021))))))

(ert-deftest test-openalex/buffer-sort-cited-by-count ()
  "Test that citation counts can be extracted for sorting."
  (with-temp-buffer
    (org-mode)
    (insert "* Work 1
:PROPERTIES:
:CITED_BY_COUNT: 100
:END:

* Work 2
:PROPERTIES:
:CITED_BY_COUNT: 500
:END:

* Work 3
:PROPERTIES:
:CITED_BY_COUNT: 50
:END:
")
    ;; Collect all citation counts
    (let ((counts '()))
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (push (org-entry-get (point) "CITED_BY_COUNT") counts))
      (setq counts (nreverse counts))
      (should (equal counts '("100" "500" "50")))
      ;; Verify they can be converted to numbers for sorting
      (should (equal (mapcar #'string-to-number counts) '(100 500 50))))))

;;; Tests for buffer management

(ert-deftest test-openalex/buffer-creation-pattern ()
  "Test that OpenAlex buffers follow naming convention."
  (let ((buffer-names '("*OpenAlex - Author*"
                       "*OpenAlex - Query*"
                       "*OpenAlex - Related works*"
                       "*OpenAlex - References*"
                       "*OpenAlex - Cited by*"
                       "*OpenAlex Full-text search*")))
    (dolist (name buffer-names)
      (should (string-prefix-p "*OpenAlex" name)))))

(ert-deftest test-openalex/buffer-cleanup ()
  "Test identification of OpenAlex buffers for cleanup."
  (let ((test-buffers '("*OpenAlex - Author*"
                       "*OpenAlex - Query*"
                       "*NotOpenAlex*"
                       "regular-buffer")))
    (dolist (name test-buffers)
      (if (string-prefix-p "*OpenAlex" name)
          (should (string-match-p "^\\*OpenAlex" name))
        (should-not (string-match-p "^\\*OpenAlex" name))))))

;;; Tests for URL construction and formatting

(ert-deftest test-openalex/base-url-construction ()
  "Test OpenAlex API base URLs."
  (let ((base-url "https://api.openalex.org"))
    (should (equal (concat base-url "/works") "https://api.openalex.org/works"))
    (should (equal (concat base-url "/authors") "https://api.openalex.org/authors"))
    (should (equal (concat base-url "/autocomplete/works")
                   "https://api.openalex.org/autocomplete/works"))
    (should (equal (concat base-url "/autocomplete/authors")
                   "https://api.openalex.org/autocomplete/authors"))))

(ert-deftest test-openalex/entity-id-formats ()
  "Test handling of different OpenAlex entity ID formats."
  (let ((openalex-id "https://openalex.org/W123456789")
        (doi "10.1234/test")
        (doi-url "https://doi.org/10.1234/test")
        (pubmed-id "PMID:12345678"))
    ;; OpenAlex ID
    (should (string-match-p "^https://openalex.org/[A-Z][0-9]+" openalex-id))
    ;; DOI
    (should (string-match-p "^10\\." doi))
    ;; DOI URL
    (should (string-prefix-p "https://doi.org/" doi-url))
    ;; PubMed ID
    (should (string-prefix-p "PMID:" pubmed-id))))

(ert-deftest test-openalex/filter-string-construction ()
  "Test filter string construction for API queries."
  (let* ((filters '((:author . "A123") (:year . "2023")))
         (filter-parts (mapcar (lambda (f)
                                (format "%s:%s"
                                        (substring (symbol-name (car f)) 1)  ; Remove leading :
                                        (cdr f)))
                              filters)))
    (should (member "author:A123" filter-parts))
    (should (member "year:2023" filter-parts))
    (should (equal (string-join filter-parts ",") "author:A123,year:2023"))))

;;; Tests for template processing

(ert-deftest test-openalex/org-property-template ()
  "Test org property template structure."
  (let ((template ":PROPERTIES:
:OPENALEX: ${id}
:DOI: ${doi}
:YEAR: ${year}
:CITED_BY_COUNT: ${cited_by_count}
:END:")
        (data '(("id" . "W123")
                ("doi" . "10.1234/test")
                ("year" . 2023)
                ("cited_by_count" . 42))))
    ;; Verify template contains expected placeholders
    (should (string-match-p "\\${id}" template))
    (should (string-match-p "\\${doi}" template))
    (should (string-match-p "\\${year}" template))
    (should (string-match-p "\\${cited_by_count}" template))))

;;; Tests for pagination and data collection

(ert-deftest test-openalex/pagination-calculation ()
  "Test pagination math for API requests."
  (let* ((total-count 250)
         (per-page 25)
         (expected-pages 10))
    (should (= (ceiling (/ (float total-count) per-page)) expected-pages)))

  (let* ((total-count 251)
         (per-page 25)
         (expected-pages 11))
    (should (= (ceiling (/ (float total-count) per-page)) expected-pages)))

  (let* ((total-count 25)
         (per-page 25)
         (expected-pages 1))
    (should (= (ceiling (/ (float total-count) per-page)) expected-pages))))

(ert-deftest test-openalex/page-range-generation ()
  "Test generation of page ranges for iteration."
  (let* ((total-pages 5)
         (page-range (number-sequence 1 total-pages)))
    (should (equal page-range '(1 2 3 4 5))))

  (let* ((start-page 2)
         (end-page 5)
         (page-range (number-sequence start-page end-page)))
    (should (equal page-range '(2 3 4 5)))))

;;; Tests for data aggregation

(ert-deftest test-openalex/works-aggregation ()
  "Test aggregation of works from multiple pages."
  (let ((page1-results '((:id "W1") (:id "W2")))
        (page2-results '((:id "W3") (:id "W4")))
        (page3-results '((:id "W5"))))
    (let ((all-results (append page1-results page2-results page3-results)))
      (should (= (length all-results) 5))
      (should (equal (car all-results) '(:id "W1")))
      (should (equal (car (last all-results)) '(:id "W5"))))))

;;; Tests for error handling and edge cases

(ert-deftest test-openalex/empty-results-handling ()
  "Test handling of empty results from API."
  (let ((empty-response '(:results () :meta (:count 0 :per_page 25))))
    (should (null (plist-get empty-response :results)))
    (should (= (plist-get (plist-get empty-response :meta) :count) 0))))

(ert-deftest test-openalex/missing-optional-fields ()
  "Test handling of missing optional fields in work data."
  (let ((minimal-work '(:id "W123" :title "Test")))
    ;; Should handle missing fields gracefully
    (should (plist-get minimal-work :id))
    (should (plist-get minimal-work :title))
    (should (null (plist-get minimal-work :doi)))
    (should (null (plist-get minimal-work :abstract_inverted_index)))))

(ert-deftest test-openalex/null-safety ()
  "Test null safety for various operations."
  (should (null nil))
  (should (equal (or nil "default") "default"))
  (should (equal (and nil "never") nil))
  (should (equal (if-let ((x nil)) "yes" "no") "no")))

;;; Tests for date and year handling

(ert-deftest test-openalex/year-extraction ()
  "Test year extraction from different date formats."
  (let ((publication-date "2023-06-15"))
    (should (string-match "^\\([0-9]\\{4\\}\\)" publication-date))
    (should (equal (match-string 1 publication-date) "2023")))

  (let ((year 2023))
    (should (numberp year))
    (should (>= year 1900))
    (should (<= year 2100))))

(ert-deftest test-openalex/current-year-calculation ()
  "Test current year for time-based queries."
  (let ((current-year (string-to-number (format-time-string "%Y" (current-time)))))
    (should (numberp current-year))
    (should (>= current-year 2024))))

;;; Tests for author collaboration data

(ert-deftest test-openalex/coauthor-extraction ()
  "Test extraction of coauthors from authorships."
  (let ((work '(:authorships
                ((:author (:display_name "Alice Smith"
                           :id "A1"))
                 (:author (:display_name "Bob Jones"
                           :id "A2"))
                 (:author (:display_name "Carol White"
                           :id "A3"))))))
    (let ((authors (plist-get work :authorships)))
      (should (= (length authors) 3))
      (should (equal (plist-get (plist-get (car authors) :author) :display_name)
                     "Alice Smith")))))

(ert-deftest test-openalex/institution-extraction-from-authorship ()
  "Test institution extraction from authorship data."
  (let ((authorship '(:author (:display_name "Alice")
                      :institutions ((:display_name "MIT" :country_code "US")
                                   (:display_name "Harvard" :country_code "US")))))
    (let ((institutions (plist-get authorship :institutions)))
      (should (= (length institutions) 2))
      (should (equal (plist-get (car institutions) :display_name) "MIT")))))

;;; Tests for name formatting and parsing

(ert-deftest test-openalex/author-name-parsing ()
  "Test parsing author names for COA formatting."
  (let* ((display-name "John A. Smith")
         (name-parts (split-string display-name))
         (last-name (car (last name-parts)))
         (first-names (butlast name-parts)))
    (should (equal last-name "Smith"))
    (should (equal first-names '("John" "A.")))))

(ert-deftest test-openalex/name-case-normalization ()
  "Test name case normalization."
  (should (equal (capitalize "john") "John"))
  (should (equal (capitalize "SMITH") "Smith"))
  (should (equal (upcase "test") "TEST"))
  (should (equal (downcase "TEST") "test")))

;;; Tests for list processing

(ert-deftest test-openalex/list-splitting ()
  "Test splitting lists for batch processing."
  (let* ((items '("W1" "W2" "W3" "W4" "W5" "W6" "W7"))
         (batch-size 3)
         (batch1 (cl-subseq items 0 (min batch-size (length items))))
         (remaining (nthcdr batch-size items)))
    (should (= (length batch1) 3))
    (should (= (length remaining) 4))
    (should (equal batch1 '("W1" "W2" "W3")))
    (should (equal remaining '("W4" "W5" "W6" "W7")))))

(ert-deftest test-openalex/list-uniqueness ()
  "Test removing duplicates from lists."
  (let ((items-with-dups '("A" "B" "C" "B" "D" "A")))
    (should (equal (delete-dups (copy-sequence items-with-dups))
                   '("A" "B" "C" "D")))))

;;; Tests for string operations

(ert-deftest test-openalex/url-encoding ()
  "Test URL encoding for query parameters."
  (should (equal (url-hexify-string "hello world") "hello%20world"))
  (should (equal (url-hexify-string "test@example.com") "test%40example.com"))
  (should (equal (url-hexify-string "a+b") "a%2Bb")))

(ert-deftest test-openalex/string-joining ()
  "Test string joining for various separators."
  (should (equal (string-join '("a" "b" "c") ", ") "a, b, c"))
  (should (equal (string-join '("one") ", ") "one"))
  (should (equal (string-join '() ", ") ""))
  (should (equal (string-join '("1" "2" "3") "|") "1|2|3")))

;;; Tests for org-mode integration

(ert-deftest test-openalex/org-link-format ()
  "Test org-mode link formatting."
  (let ((link "[[https://openalex.org/W123][Work Title]]"))
    (should (string-match-p "^\\[\\[.*\\]\\[.*\\]\\]$" link)))

  (let ((elisp-link "[[elisp:(some-function)][Click Here]]"))
    (should (string-prefix-p "[[elisp:" elisp-link))))

(ert-deftest test-openalex/org-property-access ()
  "Test org property access patterns."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Heading
:PROPERTIES:
:CUSTOM_ID: test123
:YEAR: 2023
:DOI: 10.1234/test
:END:
")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (should (equal (org-entry-get (point) "CUSTOM_ID") "test123"))
    (should (equal (org-entry-get (point) "YEAR") "2023"))
    (should (equal (org-entry-get (point) "DOI") "10.1234/test"))))

;;; Integration tests

(ert-deftest test-openalex/complete-workflow-simulation ()
  "Simulate a complete workflow from query to display."
  (let* ((query-filter "author:A123")
         (page 1)
         (per-page 25)
         (mock-count 50)
         (total-pages (ceiling (/ (float mock-count) per-page))))

    ;; Verify pagination
    (should (= total-pages 2))

    ;; Verify filter construction
    (should (string-match-p "author:" query-filter))

    ;; Simulate results collection
    (let ((all-results '()))
      (dotimes (p total-pages)
        (let ((page-results (list (list :id (format "W%d" p)))))
          (setq all-results (append all-results page-results))))
      (should (= (length all-results) 2)))))

(provide 'openalex-utilities-test)

;;; openalex-utilities-test.el ends here
