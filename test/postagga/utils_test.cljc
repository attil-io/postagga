;;Copyright (c) 2017 Rafik Naccache <rafik@fekr.tech>
;;Distributed under the MIT License
(ns postagga.utils-test
  (:require [clojure.test :refer :all]
            [postagga.tools :refer :all]))

(deftest get-column-test
  (testing "Get Column")
  (is (= {} (get-column nil 0)))
  (is (= {} (get-column {} 0)))
  (is (= {[0 0] 1} (get-column {[0 0] 1} 0)))
  (is (= {[0 0] 1 [1 0] 2} (get-column {[0 0] 1 [1 0] 2} 0)))
  (is (= {} (get-column {[0 0] 1 [1 0] 2} 1)))
  (is (= {[0 1] 3 [1 1] 4} (get-column {[0 0] 1 [1 0] 2 [0 1] 3 [1 1] 4} 1)))
  (is (= {[0 0] 1 [2 0] 2} (get-column {[0 0] 1 [2 0] 2} 0))))

(deftest get-row-test
  (testing "Get Row")
  (is (= {} (get-row nil 0)))
  (is (= {} (get-row {} 0)))
  (is (= {[0 0] 1} (get-row {[0 0] 1} 0)))
  (is (= {[0 0] 1 [0 1] 2} (get-row {[0 0] 1 [0 1] 2} 0)))
  (is (= {} (get-row {[0 0] 1 [0 1] 2} 1)))
  (is (= {[1 0] 2 [1 1] 4} (get-row {[0 0] 1 [1 0] 2 [0 1] 3 [1 1] 4} 1)))
  (is (= {[0 0] 1 [0 2] 2} (get-row {[0 0] 1 [0 2] 2} 0))))

(deftest arg-max-test
  (testing "Arg Max")
  (is (= nil (arg-max {})))
  (is (= 0 (arg-max {0 0}))) 
  (is (= 2 (arg-max {0 1 2 3})))) 

(deftest bigrams-test
  (testing "Bigrams")
  (is (= #{} (bigrams nil)))
  (is (= #{} (bigrams "")))
  (is (= #{} (bigrams "a")))
  (is (= #{"ab"} (bigrams "ab")))
  (is (= #{"ab" "bc"} (bigrams "abc"))))

(deftest similarity-test
  (testing "Similarity")
  (is (= 0 (similarity nil nil)))  ; TODO: should this really be the case
  (is (= 0 (similarity "" "")))    ; TODO: should this really be the case
  (is (= 0 (similarity "" nil)))
  (is (= 0 (similarity "a" nil)))
  (is (= 0 (similarity "a" "")))
  (is (= 0 (similarity "a" "a")))   ; TODO: should this really be the case???
  (is (= 1 (similarity "ab" "ab")))
  (is (= 1/2 (similarity "ab" "abc")))
  (is (= 1/3 (similarity "ab" "abcd")))
  (is (= 0 (similarity "ab" "cd"))))

(deftest are-close-within-test
  (testing "Are Close Within?")
  (is (= false (are-close-within? 1/4 "ab" "cd")))  
  (is (= true (are-close-within? 0 "ab" "cd")))  
  (is (= false (are-close-within? 3/4 "abc" "bcd")))  
  (is (= true (are-close-within? 1/4 "abc" "bcd")))  
  (is (= true (are-close-within? 1 "abc" "abc"))))
 
(deftest find-first-test
  (testing "Find first")
  (is (= nil (find-first #(= % 2) nil )))
  (is (= nil (find-first #(= % 2) [1] )))
  (is (= 2 (find-first #(= % 2) [1 2 3 4] )))
  (is (= 5 (find-first #(= 0 (mod % 5)) [5 10] )))) 

