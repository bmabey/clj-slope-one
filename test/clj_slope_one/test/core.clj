(ns clj-slope-one.test.core
  (:use clj-slope-one.core :reload)
  (:use clojure.test midje.sweet))

(def data
  {"John" {"Item A" 5 "Item B" 3 "Item C" 2 "Item D" nil}
   "Mark" {"Item A" 3 "Item B" 4 "Item C" nil "Item D" 4}
   "Lucy" {"Item A" nil "Item B" 2 "Item C" 5 "Item D" 3}
   "Fred" {"Item A" 4 "Item B" nil "Item C" 3 "Item D" nil}})

(deftest slope-one-test
  (facts
   (let [trained-model (train data)]
     (:differences trained-model) =>   {"Item A" {"Item D" -1, "Item C" 2, "Item B" 1/2}
                                        "Item C" {"Item D" 2, "Item B" 1, "Item A" -2}
                                        "Item B" {"Item D" -1/2, "Item C" -1, "Item A" -1/2}
                                        "Item D" {"Item C" -2, "Item B" 1/2, "Item A" 1}}
     (:freqs trained-model) => {"Item A" {"Item D" 1, "Item C" 2, "Item B" 2}
                                "Item B" {"Item D" 2, "Item C" 2, "Item A" 2}
                                "Item C" {"Item D" 1, "Item B" 2, "Item A" 2}
                                "Item D" {"Item C" 1, "Item B" 2, "Item A" 1}}
       "a single prediction can be issued for a specified item"
       (predict trained-model {"Item A" 2} "Item B") => (/ 3 2)
       "a list of items can be mapped to predictions"
       (predictions trained-model {"Item A" 2} ["Item B" "Item C"]) => (just {"Item B" (/ 3 2)
                                                                             "Item C" 0})
       "when no items are specified all unrated items are predicted on"
       (keys (predictions trained-model {"Item B" 3})) => (just "Item A" "Item C" "Item D"))))

;; TODO: handle case where the item hasn't been rated by anymore (probably return an average
;; or simply don't have a prediction/reccomendation unless content can be taken into account)

;; TODO: cap the predictions to a specified allowable range (e.g. 0-5)
