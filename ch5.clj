;p. 67
(defn fif
  ([f-if then]
   (fn [x] (when (f-if x) (then x))))
  ([f-if then else]
   (fn [x] (if (f-if x)
             (then x)
             (else x)))))

#_(map (fif slave owner employer) people)

; Function intersection
(defn fint [f & fns]
  (if fns
    (let [chain (apply fint fns)]
      (fn [x] (and (f x) (chain x))))
    f))

#_(filter (fint signed sealed delivered) docs)

; Function union
(defn fun [f & fns]
  (if fns
    (let [chain (apply fint fns)]
      (fn [x] (or (f x) (chain x))))
    f))

#_(filter (fun friends acquaintances relatives) people)


(defn lrec
  ([rec] (lrec rec nil))
  ([rec base]
   (fn self [lst]
       (if lst
         (rec (first lst) #(self (next lst)))
         (if (fn? base) (base) base)))))

(def lrec-length (lrec (fn [x f] (inc (f))) 0))
; equivalent to:
(defn our-length [lst]
  (if lst
    (inc (our-length (next lst)))
    0))

;
(lrec (fn [x f] (and odd? x) (f)) true)

(defn ttrav
  ([rec] (ttrav rec identity))
  ([rec base]
   (fn self [tree]
       (if (list? tree)
         (rec (self (first tree))
              (when (next tree)
                (self (next tree))))
         (if (fn? base) (base tree) base)))))

(def our-copy-tree (ttrav cons))
(def count-leaves (ttrav (fn [l r] (+ l (or r 1))) 1))
(def our-flatten (ttrav concat list))

(our-copy-tree '((1 2 3) (4 (5 6))))
(our-flatten '((1 2 3) (4 (5 6))))
(count-leaves '((1 2 3) (4 (5 6))))

(defn trec
  ([rec] (trec rec identity))
  ([rec base]
   (fn self [tree]
       (if (list? tree)
         (rec tree #(self (first tree)) #(when (next tree) (self (next tree))))
         (if (fn? base) (base tree) base)))))

(def our-flatten2 (trec (fn [o l r] (concat (l) (r))) list))
;
; my variation... rfind-if was the result of (rfind-if odd?)
(defn rfind-if [pred] (trec (fn [o l r] (or (l) (r))) #(and (pred %) %)))
(def rfind-odd (rfind-if odd?))
(rfind-odd '((2 2 2) (4 (5 6))))
