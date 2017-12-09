;;Copyright (c) 2017 Rafik Naccache <rafik@fekr.tech>
;;Distributed under the MIT License
(ns postagga.tagger-test
  (:require [clojure.test :refer :all]
            [postagga.tagger :refer :all]))

(deftest viterbi-test
  (testing "Viterbi")
  (let [states #{"Healthy" "Fever"}
        init-probs {"Healthy" 0.6 "Fever" 0.4} 
        transitions  {["Healthy" "Healthy"] 0.7 ["Healthy" "Fever"]  0.3 ["Fever" "Healthy"] 0.4 ["Fever" "Fever"] 0.6} 
        emissions {["Healthy" "Ok"] 0.5 ["Healthy" "Cold"] 0.4 ["Healthy" "Dizzy"] 0.1 ["Fever" "Ok"] 0.1 ["Fever" "Cold"] 0.4 ["Fever" "Dizzy"] 0.6}
        model {:states states :init-probs init-probs :transitions transitions :emissions emissions}]
     (is (= ["Healthy" "Healthy" "Fever"] (viterbi model ["Ok" "Cold" "Dizzy"])))
     (is (= ["Healthy" "Healthy" "Healthy"] (viterbi (assoc-in model [:transitions] {["Healthy" "Healthy"] 0.9 ["Healthy" "Fever"]  0.1 ["Fever" "Healthy"] 0.4 ["Fever" "Fever"] 0.6}) ["Ok" "Dizzy" "Ok"]))))) 

