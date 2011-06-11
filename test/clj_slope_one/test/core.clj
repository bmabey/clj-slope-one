(ns clj-slope-one.test.core
  (:use clj-slope-one.core :reload)
  (:use clojure.test midje.sweet))

(deftest map-vals-test
  (fact
   (map-vals #(inc %2) {:foo 1 :bar 2})) => {:foo 2 :bar 3})

(deftest map-nested-vals-test
  (fact
   (map-nested-vals (fn [keys v] (inc v)) {:foo {:bar 2}})) => {:foo {:bar 3}})

(def data
  {"John" {"Item A" 5 "Item B" 3 "Item C" 2}
   "Mark" {"Item A" 3 "Item B" 4 "Item C" nil}
   "Lucy" {"Item A" nil "Item B" 2 "Item C" 5}})

(deftest slope-one-test
  (facts
   (let [trained-model (train data)]
     (:differences trained-model) =>  {"Item A" {"Item B" 0.5 "Item C" 3}
                                       "Item B" {"Item A" -0.5 "Item C" -1}
                                       "Item C" {"Item A" -3   "Item B" 1}}
     (:freqs trained-model) =>  {"Item A" {"Item B" 2 "Item C" 1}
                                 "Item B" {"Item A" 2 "Item C" 2}
                                 "Item C" {"Item A" 1 "Item B" 2}})
   ;; Item A and B diff: ((5 - 3) + (3 - 4)) / 2 = (2 + (-1)) /  2  = 0.5
   ;; Item B and A diff: ((3 - 5) + (4 - 3)) / 2 = (-2 + 1) /  2  = -0.5
   ;; Item B and C diff: ((3 - 2) + (2 - 5)) / 2 = (1 + (-3)) /  2  = -1.0
   ;; ....
   ))
