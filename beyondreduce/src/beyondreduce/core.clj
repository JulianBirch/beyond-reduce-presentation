(ns beyondreduce.core)

(def mylist {:head 1 :tail {:head 2 :tail {:head 3 :tail :nil}}})

(defn head-map [f cell]
  (if (= :nil cell)
    :nil
    (update cell :head f)))

(defn tail-map [f cell]
  (if (= :nil cell)
    :nil
    (update cell :tail f)))

(defn cata [map-recurse map-value l]
  (let [cata2 (partial cata map-recurse map-value)]
    (->> l
         (map-recurse cata2)
         map-value)))

(defn ana [map-recurse map-value l]
  (let [ana2 (partial ana map-recurse map-value)]
    (->> l
         map-value
         (map-recurse ana2))))

(defn next-value [v]
  (if (> v 3)
    :nil
    {:head v :tail (inc v)}))

(ana tail-map next-value 0)

(defn evalop [val]
  (if (map? val)
    (let [{:keys [op l r]} val]
      (case op
        :add (+ l r)
        :mul (* l r)
        :neg (- l)
        :sub (- l r)))
    val))

(def complex {:op :add
              :l {:op :sub :l 3 :r 6}
              :r {:op :mul :l {:op :neg :l 2} :r 9}})

(defn tree-map [f cell]
  (if (map? cell)
    (let [c2 (update cell :l f)]
      (if (= (:op cell) :neg)
        c2
        (update c2 :r f)))
    cell))

(cata tree-map evalop complex)

(defn search [val]
  (if (map? val)
    (let [{:keys [op l r]} val]
      (if (= op :neg)
        l
        (concat l r)))
    [val]))

(cata tree-map search complex)

(defn next-level [cell]
  (if (map? cell)
    (let [new-items (->> cell
                         :head
                         (mapcat (juxt :l :r))
                         (filter identity))
          tail (if (empty? new-items)
                   :nil
                   {:head new-items :tail :nil})]
      (assoc cell :tail tail))
    cell))

(defn non-maps [{:keys [head tail] :as opts}]
  (filter #(not (map? %)) (concat head tail)))

(cata tail-map non-maps
  (ana tail-map next-level {:head [complex] :tail :nil}))
; (3 6 9 2)

(defn hylo [map-recurse map-before map-after l]
  (let [hylo2 (partial hylo map-recurse map-before map-after)]
    (->> l
         map-before
         (map-recurse hylo2)
         map-after)))

(hylo tail-map next-level non-maps
      {:head [complex] :tail :nil})

(defn sort-2 [[x y]]
  (if (< x y) [x y] [y x]))

(defn explode [x]
  (let [c (count x)]
    (case c
          0 x
          1 x
          2 (sort-2 x)
          (let [[l r] (split-at (/ c 2) x)]
            {:l l :r r}))))

; Adapted from code by Alexei Sholik
(defn riffle [{:keys [l r]}]
  (loop [l l r r result []]
    (let [lhead (first l), rhead (first r)]
      (cond
        (nil? lhead)     (concat result r)
        (nil? rhead)     (concat result l)
        (<= lhead rhead) (recur (rest l) r (conj result lhead))
        :else            (recur l (rest r) (conj result rhead))))))

(defn collapse [x]
  (if (map? x)
    (riffle x)
    x))

(defn binary-map [f x]
  (if (map? x)
    (-> x
      (update :l f)
      (update :r f))
    x))

(hylo binary-map explode collapse [9 3 6 7 0 1 8 2 4 5])
