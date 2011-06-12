(ns clj-slope-one.test.utils
  (:use clj-slope-one.utils :reload)
  (:use clojure.test midje.sweet))

(deftest map-vals-test
  (fact
   (map-vals #(inc %2) {:foo 1 :bar 2})) => {:foo 2 :bar 3})

(deftest map-nested-vals-test
  (fact
   (map-nested-vals (fn [keys v] (inc v)) {:foo {:bar 2}})) => {:foo {:bar 3}})
