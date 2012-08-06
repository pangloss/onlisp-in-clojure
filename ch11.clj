(defn condlet-clause [vars cl bodfn]
  `[~(first cl)
     ; ensure that all gensym vars are in scope
     (let ~(vec (mapcat (fn [[v s]] [s nil]) vars))
       ; bind the ones defined in this branch
       (let ~(condlet-binds vars cl)
         ; execute the body
         (~bodfn ~@(map (fn [[v s]] s) vars))))])

;(condlet-clause `([a ~(gensym 'a)] [b ~(gensym 'b)])
;                [true `(a 1 b 2)]
;                (fn [y z] (println (str "got" y z))))

(defn condlet-binds [vars cl]
  (let [var-map (apply hash-map (flatten vars))]
    (vec (mapcat (fn [[bindname bindvalue :as bindform]]
                            (if (sequential? bindform)
                              [(var-map bindname) bindvalue]))
                        (partition 2 (second cl))))))

(defn getvars [clauses]
  (->> clauses
       (partition 2)
       (map second)
       (mapcat #(partition 2 %))
       (map first)
       distinct
       vec))

;(getvars ['no ['a 1 'b 2] 'yes ['c 9]]) ;=> [a b c]

; it may have been better to have used an array-map here...
(defn varsyms [clauses]
  (map #(vector % (gensym %)) (getvars clauses)))

(defmacro condlet [clauses & body]
  (let [bodfn (gensym 'bodfn)
        vars (varsyms clauses)]
    `(let [~bodfn (fn ~(getvars clauses) ~@body)]
       (cond ~@(mapcat #(condlet-clause vars % bodfn) (partition 2 clauses))))))

(condlet [(= 1 2) [x (do (print 'a) 'a)
                   y (do (print 'b) 'b)]
          (= 1 1) [y (do (print 'c) 'c)
                   x (do (print 'd) 'd)]
          true    [x (do (print 'e) 'e)
                   z (do (print 'f) 'f)]]
         (list "wow" x y z))


; Wow. It's tricky to simultaneously port from CL to Clojure and also
; change the definition/usage to be more clojure-like.
;
; This was especially tricky because it's hard for me to fully
; understand the CL code that I'm working from and if I misunderstand
; then I introduce all sorts of confusion. That's why I've broken my
; implementation up into multiple helpers. I needed to understand what
; was happening to my forms at a level more granular than I may need to
; if I were just defining my own macros.


(def ^:dynamic *db* (atom {}))

; this seems easier with clojure!
; Could have used try / finally structure, too...
(defmacro with-db [db & body]
  `(binding [*db* ~db]
     ~@body))

(with-db *db* (swap! *db* assoc :y 1))
(with-db (atom {:new true}) (swap! *db* assoc :x 1))

; p. 152
;
(defmacro in [obj & choices]
  (let [insym (gensym 'insym)]
    `(let [~insym ~obj]
       (or ~@(map (fn [c] `(= ~insym ~c))
                  choices)))))

(defmacro inq [obj & args]
  `(in ~obj ~@(map (fn [c] `'~c) args)))


(defmacro in-if [fn & choices]
  (let [fnsym `fnsym#]
    `(let [~fnsym ~fn]
       (or ~@(map #(list fnsym %)
                  choices)))))

(defn >casex [g [key & more]]
  (cond (list? key) `((inq ~g ~@key) (do ~@more))
        (in key true :else :default) `(:default (do ~@more))
        :default (throw (Exception. "bad >case clause"))))
; Didn't bother making this one more idiomatic because it seems
; relatively useless.
(defmacro >case [expr & clauses]
  (let [g `g#]
    `(let [~g ~expr]
       (cond ~@(mapcat #(>casex g %)
                       clauses)))))

(in * + - *)
(inq 'a a b c)
; insym is defined so that the 1st argument isn't executed repeatedly.
(in (first [* +]) * -)
(in-if #(= 'x %) 'a 'b 'x)
(>case 'blog
  [(frog bog) "no"]
  [(blog dog) "yes"])



; while macro is in clojure.core
(defmacro till [test & body]
  `(while (not ~test) ~@body))

(let [x (atom 0)]
  (till (> @x 10)
    (print ".")
    (swap! x inc)))

; c style for not worth the bother.

; do-tuples is a less flexible version of partition:
(partition 2 1 '(a b c d))

; do-tuples-c can be replicated by adding n-1 elements from the front to
; the back of the list: (partition-style args)
(defn partition-c [n ss coll]
  (partition n ss (concat coll (take (dec n) coll))))

;
(defmacro do-tuples-o [parms source & body]
  (when parms
    (let [src (gensym)]
      `(let [~src ~source]
         (map (fn ~parms ~@body)
              ~@(map (fn [n] `(drop ~n ~src))
                     (range (count parms))))))))

; does the same as PG's but in 1/5 the lines...still prefer my
; partition-c though!
(defmacro do-tuples-c [parms source & body]
  `(let [src# ~source]
     (do-tuples-o ~parms
                  (concat src# (take ~(dec (count parms)) src#))
                  ~@body)))

; mvdo* is another one that clojure has out of the box. Not directly,
; but because loop and for, etc have destructuring these macros aren't
; needed. I'm studying them but won't build them because they're not
; used in the book again anyhow.
;
; skipping mvsetq since we don't mutate values and multi value returns
; aren't special.

; shuffle is almost the same as interleave but interleave drops the 4
; here and interleave supports more than 2 lists.
; (interleave '(a b c) '( 1 2 3 4))
;
(defn our-shuffle [x y]
  (cond (nil? x) y
        (nil? y) x
        :default (list* (first x) (first y) (our-shuffle (next x) (next y)))))
(our-shuffle '(a b c) '( 1 2 3 4))
