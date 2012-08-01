; remove-if-not -> select (!)o
; mapcan -> mapcat

; find2 seems equvalent to 'some except the style of its return.
(defn find2 [f [x & more :as lst]]
  (when lst
    (let [val (f x)]
      (if val
        [x val]
        (recur f more)))))
(find2 odd? (range))

; p. 45
(def last1 last)
(defn single [lst] (and (first lst) (not (next lst))))
(defn append1 [lst obj] (concat lst (list obj)))
(defn mklist [obj] (if (sequential? obj) obj (list obj)))

(append1 '(1 2) 3)
(= (mklist 1) (mklist '(1)))

(defn longer [x y]
  (if (and (sequential? x) (sequential? y))
    (loop [x (next x) y (next y)]
      (and x (or (not y) (recur (next x) (next y)))))
    (> (count x) (count y))))
(longer [3 1 2] [1 2])

; stock filter is better - this is not a lazy seq. I'm writing to the
; book's spec not the optimum.
(defn filter-map [f lst]
  (loop [acc () [x & more] lst]
    (let [acc (if-let [val (f x)] (cons val acc) acc)]
      (if more
        (recur acc more)
        (reverse acc)))))
(filter-map #(when (even? %) (* % %)) (range 10))


(defn group [source n]
  (when (zero? n) (throw (Exception. "zero length")))
  (when source
    (letfn [(rec [source acc]
                 (let [more (drop n source)]
                   (if (empty? more)
                     (reverse (cons source acc))
                     (recur more (cons (take n source) acc)))))]
           (rec source nil))))
(= (group '(a b c d e f g) 2)
   (partition-all 2 '(a b c d e f g)))

(flatten '(a (b c) ((d e) f)))
; built-in flatten uses tree-seq which is much more elegant.
; the example in the book is *not* fully tail recursive either.
; the example seems to be missing a 'reverse' which I have added.
(defn flatten1 [x]
  (letfn [
    (rec [x acc]
         (cond
           (nil? x) acc
           (symbol? x) (cons x acc)
           :default (recur (next x) (rec (first x) acc))))]
         (reverse (rec x nil))))
(flatten1 '(a (b c) ((d e) f)))

; again, not tail-recursive
(defn prune [test tree]
  (letfn [(rec [[x & more :as tree] acc]
               (cond
                 (nil? tree) (reverse acc)
                 (sequential? x) (recur more (cons (rec x nil) acc))
                 :default (recur more
                                 (if (test x)
                                   acc
                                   (cons x acc)))))]
    (rec tree nil)))
(prune even? '(1 2 (3 (4 5) 6) 7 8 (9)))

;p. 50
; find2 defined above
;
; clojure doesn't have member, so here's a definition:
(defn member
  ([x lst] (member x lst :test =))
  ([x lst _ test]
   (let [r (drop-while #(not (test x %)) lst)]
     (when (not (empty? r)) r))))
(member 'c '(a b c d))
(member 'e '(a b c d))

(defn before
  ([x y lst] (before x y lst :test =))
  ([x y [a & more :as lst] _ test]
   (and lst
        (cond (test y a) nil
              (test x a) lst
              :default (recur x y more :test test)))))
(before 'b 'd '(a b c d))

(defn after
  ([x y lst] (after x y lst :test =))
  ([x y lst _ test]
   (member x
           (before y x lst :test test )
           :test test )))
(after 'a 'b '(b a d))
(after 'a 'b '(a))

(defn duplicate
  ([obj lst] (duplicate obj lst :test =))
  ([obj lst _ test]
   (member obj (next (member obj lst :test test)) :test test)))
(duplicate 'a '(b a d a x))

; split-if is not quite the same as partition-by. This only returns 2 parts and does
; not continue once the fn is true. It's definition is also
; broken and it's not referred to again in the book. Skipping.
(partition-by #(> % 4) (range 1 11))

(defn most
  ([f [a & more :as lst]]
   (when lst (most f more a (f a))))
  ([f [a & more :as lst] winner n]
   (if lst
     (let [fa (f a)]
       (if (< n fa)
         (recur f more a fa)
         (recur f more winner n)))
     [winner n])))

#_(apply max-key count '((a b) (a b c) (a) (e f g)))
(most count '((a b) (a b c) (a) (e f g)))
(most count nil)

(do
  (defn best [f lst]
    (when lst
      (loop [wins (first lst) [a & lst] (next lst)]
        (let [wins (if (f a wins) a wins)]
          (if lst
            (recur wins lst)
            wins)))))
  (best > '(1 2 3 4 5 3)))

(do
  (defn mostn [f lst]
    (when lst
      (letfn [(new-results [item lst n]
                           (if lst (build-results lst (list item) n) [(list item) n]))
              (build-results [[item & lst] result n]
                             (let [score (f item)]
                               (cond (> score n) (new-results item lst score)
                                     (= score n) (recur lst (cons item result) n)
                                     lst (recur lst result n)
                                     :default [result n])))]
             (new-results (first lst) (next lst) (f (first lst))))))
  (mostn count '((a b) (a b c) (a) (e f g) (a b c d) (a b x y)))
  )

; This seems like a very useful tool so I made it lazy and renamed the
; original.
(defn map->> [f start test-fn succ-fn]
  (loop [i start result nil]
    (if (test-fn i)
      (reverse result)
      (recur (succ-fn i) (cons (f i) result)))))

; Cool! The lazy version is actually much better and doesn't require a
; final reverse.
(defn map-> [f start test-fn succ-fn]
  (letfn [(go [i]
              (lazy-seq (when (not (test-fn i))
                          (cons (f i) (go (succ-fn i))))))]
         (go start)))

(take 5 (map--> identity 5 #(> % 20) inc))

(defn mapa-b
  ([f a b] (mapa-b f a b 1))
  ([f a b step]
    (map-> f a #(> % b) #(+ % step))))

(mapa-b inc -2 0 0.5)

(defn map0-n [f n] (mapa-b f 0 n))
(map0-n inc 5)

; I think these are functionally equivalent (mapcat is more capable)
(def mappend mapcat)

; Skipping mapcars and rmapcar.
; Skipping readlist, prompt and break-loop.
;
(def mkstr str)
(let [pi (. Math PI)]
  (mkstr pi " pieces of " 'pi))

