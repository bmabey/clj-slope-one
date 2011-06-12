(ns clj-slope-one.core)

;; Util fns...

(defn flatten-initial
  "Flattens the first level of a potentially nested sequential structure."
  [coll]
  (reduce
   (fn [so-far x]
     (apply conj so-far x))
   []
   coll))

(defn map-vals
  "Maps the vals in m using f, where f receives k v and returns new v."
  [f m]
  (persistent!
    (reduce (fn [m [k v]] (assoc! m k (f k v)))
            (transient m) m)))

(defn map-nested-vals
  "Maps the nested vals of a nested map m with f, where f receives the keys and value.
   Currently only works with maps of a single layer of nesting. Example:

   (map-nested-vals (fn [keys val] (inc val)) {:foo {:bar 2}}) => {:foo {:bar 3}}"
  [f m]
  (map-vals (fn [k1 inner-map]
              (map-vals (fn [k2 val] (f [k1 k2] val)) inner-map)) m))


;; nice pattern from: http://tech.puredanger.com/2010/09/24/meet-my-little-friend-mapmap/
(defn mapmap
  "Apply kf and vf to a sequence, s, and produce a map of (kf %) to (vf %)."
  ([vf s]
     (mapmap identity vf s))
  ([kf vf s]
     (zipmap (map kf s)
              (map vf s))))

;; Main algorithm

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
