(ns clojure-algorithms.web-programming.search-books-by-isbn-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.search-books-by-isbn
             :refer [normalize-olid olid-url valid-isbn? summarize-book-data
                     get-openlibrary-data]]))

(deftest test-normalize-olid
  (is (= "isbn/0140328726" (normalize-olid "isbn/0140328726")))
  (is (= "authors/OL34184A" (normalize-olid "/authors/OL34184A")))
  (is (= "isbn/0140328726" (normalize-olid "  isbn/0140328726/ ")))
  (is (thrown? IllegalArgumentException (normalize-olid "nope")))
  (is (thrown? IllegalArgumentException (normalize-olid "a/b/c"))))

(deftest test-olid-url
  (is (= "https://openlibrary.org/isbn/0140328726.json"
         (olid-url "isbn/0140328726"))))

(deftest test-valid-isbn?
  (is (true? (valid-isbn? "0140328726")))
  (is (true? (valid-isbn? "9780140328721")))
  (is (false? (valid-isbn? "123")))
  (is (false? (valid-isbn? "abcdefghij"))))

(deftest test-summarize-book-data
  (is (= {"Title" "T" "Publish date" "1999" "Authors" "A U Thor"
          "Number of pages" 96 "ISBN (10)" "0140328726" "ISBN (13)" "x"}
         (summarize-book-data {"title" "T" "publish_date" "1999"
                               "authors" [{"key" "/authors/OL1A"}]
                               "number_of_pages" 96 "isbn_10" ["0140328726"]
                               "isbn_13" ["x"]}
                              (fn [_] "A U Thor")))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (get-openlibrary-data "isbn/0140328726"))))
