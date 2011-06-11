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


(defn update
  "'Updates' the value at k in the map m using thr provided function f.  The supplied args will be
    applied to f along with the current value (which may be nil, so fnil is useful)."
  [m k f & args]
  (assoc m k (apply f (get m k) args)))

;; Main algorithm

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
