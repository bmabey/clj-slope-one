(ns clj-slope-one.test.core
  (:use [clj-slope-one.core])
  (:use clojure.test midje.sweet))

(def data
  {"John" {"Item A" 5 "Item B" 3 "Item C" 2}
   "Mark" {"Item A" 3 "Item B" 4 "Item C" nil}
   "Lucy" {"Item A" nil "Item B" 2 "Item C" 5}})

(defn flatten-initial
  "Flattens the first level of a potentially nested sequential structure."
  [coll]
  (reduce
   (fn [so-far x]
     (apply conj so-far x))
   []
   coll))


(defn update
  "'Updates' the value at k in the map m using thr provided function f.  The supplied args will be
    applied to f along with the current value (which may be nil, so fnil can be useful)."
  [m k f & args]
  (assoc m k (apply f (get m k) args)))

(deftest update-test
  (facts
   (update {} :foo 0 inc) => {:foo 1}
   (update {:bar 3} :bar 0 + 10) => {:bar 13}))

(defn differences [data]
  (let [item-pair-diffs (flatten-initial
                         (for [[user preferences] data]
                           (for [[i u_i] preferences
                                 [j u_j] preferences
                                 :when (and (not= i j) u_i u_j)]
                             [[i j] (- u_i u_j)])))
        [freqs diffs] (reduce (fn [[freqs-so-far diffs-so-far] [item-pair diff]]
                                [(update freqs-so-far item-pair (fnil inc 0))
                                 (update diffs-so-far item-pair (fnil + 0) diff)])
                              [{} {}]
                              item-pair-diffs)]
    (reduce (fn [so-far [item-pair diff]]
              (assoc-in so-far item-pair (/ diff (get freqs item-pair)))) {} diffs)))

(differences data)

(deftest diff-test
  (fact
   (differences data) => {"Item A" {"Item B" 0.5 "Item C" 3}
                          "Item B" {"Item A" -0.5 "Item C" -1}
                          "Item C" {"Item A" -3   "Item B" 1}}
   ;; Item A and B diff: ((5 - 3) + (3 - 4)) / 2 = (2 + (-1)) /  2  = 0.5
   ;; Item A and B diff: ((3 - 5) + (4 - 3)) / 2 = (-2 + 1) /  2  = -0.5
   ;; Item B and C diff: ((3 - 2) + (2 - 5)) / 2 = (1 + (-3)) /  2  = -1.0
   ;; ....
   ))
