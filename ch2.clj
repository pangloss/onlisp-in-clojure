(defn list+ [lst n]
  (map (fn [x] (+ x n)) lst))

; p. 22
(defn count-instances [obj lsts]
  (letfn [(instances-in [lst]
                        (if (coll? lst)
                          (+ (if (= (first lst) obj) 1 0)
                             (instances-in (next lst)))
                          0))]
         (map instances-in lsts)))
(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))

; p. 23
(defn our-length
  ([lst acc]
    (if lst
      (recur (next lst) (inc acc))
      acc))
  ([lst] (our-length lst 0)))

(our-length '(a b c))
