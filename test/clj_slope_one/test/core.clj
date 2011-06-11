(ns clj-slope-one.test.core
  (:use clj-slope-one.core :reload)
  (:use clojure.test midje.sweet))

(deftest update-test
  (facts
   (update {} :foo (fnil inc 0)) => {:foo 1}
   (update {:bar 3} :bar (fnil + 0) 10) => {:bar 13}))

(def data
  {"John" {"Item A" 5 "Item B" 3 "Item C" 2}
   "Mark" {"Item A" 3 "Item B" 4 "Item C" nil}
   "Lucy" {"Item A" nil "Item B" 2 "Item C" 5}})

(deftest differences-test
  (fact
   (differences data) => {"Item A" {"Item B" 0.5 "Item C" 3}
                          "Item B" {"Item A" -0.5 "Item C" -1}
                          "Item C" {"Item A" -3   "Item B" 1}}
   ;; Item A and B diff: ((5 - 3) + (3 - 4)) / 2 = (2 + (-1)) /  2  = 0.5
   ;; Item B and A diff: ((3 - 5) + (4 - 3)) / 2 = (-2 + 1) /  2  = -0.5
   ;; Item B and C diff: ((3 - 2) + (2 - 5)) / 2 = (1 + (-3)) /  2  = -1.0
   ;; ....
   ))
