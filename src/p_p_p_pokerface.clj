(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {\T 10 \J 11 \Q 12 \K 13 \A 14} card-char (first card)]
    (if (Character/isDigit card-char)
      (Integer/valueOf (str card-char))
      (get values card-char))))

(defn suit [card]
  (str (second card)))

(defn freq-of-x [fun hand]
  (frequencies (map fun hand)))

(defn freq-of-rank [hand]
  (freq-of-x rank hand))

(defn n-of-a-kind? [n hand]
  (not (empty? (filter #(= (second %) n) (freq-of-rank hand)))))

(defn pair? [hand]
 (n-of-a-kind? 2 hand)) 

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 1 (count (apply hash-set (map suit hand)))))

(defn full-house? [hand]
  (and (= 2 (count (apply hash-set (map rank hand)))) (pair? hand)))

(defn two-pairs? [hand]
  (and (= 2 (get (frequencies
                  (seq (vals (freq-of-rank hand)))) 2))))

(defn straight? [hand]
  (not (nil? (some #{(sort (map rank hand))}
        (cons '(2 3 4 5 14) (partition 5 1 (range 2 15)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
