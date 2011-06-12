(ns clj-slope-one.core
  (use clj-slope-one.utils))

(defn train [data]
  (let [item-pair-diffs (flatten-initial
                         (for [[user preferences] data]
                           (for [[i u_i] preferences
                                 [j u_j] preferences
                                 :when (and (not= i j) u_i u_j)]
                             [[i j] (- u_i u_j)])))
        [freqs diffs] (reduce (fn [[freqs-so-far diffs-so-far] [item-pair diff]]
                                [(update-in freqs-so-far item-pair (fnil inc 0))
                                 (update-in diffs-so-far item-pair (fnil + 0) diff)])
                              [{} {}]
                              item-pair-diffs)]
    {:freqs freqs
     :differences (map-nested-vals (fn [item-pair diff] (/ diff (get-in freqs item-pair))) diffs)}))

(defn known-items [model]
  (-> model :differences keys))

(defn predict [{:keys [differences freqs] :as model} preferences j]
  (/ (apply +
            (for [[i rating] preferences :when (not= i j)]
              (* (+ (get-in differences [j i]) rating) (get-in freqs [j i]))))
     (apply + (for [[i _] preferences :when (not= i j)] (get-in freqs [j i])))))

(defn predictions
  ([model preferences]
     (predictions model preferences (filter #(not (contains? preferences %)) (known-items model))))
  ([model preferences items]
     (mapmap (partial predict model preferences) items)))
