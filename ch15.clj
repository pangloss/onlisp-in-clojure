(declare rbuild build-call build-compose)


(defmacro fn-by [& expr] (rbuild expr))

(defn rbuild [expr]
  (if (or (symbol? expr) (#{ 'fn 'fn* } (first expr)) )
    expr
    (if (= (first expr) 'compose)
      (build-compose (next expr))
      (build-call expr))))

(defn build-call [[op & fns]]
  (let [g (gensym 'bcall) h (gensym) i (gensym)]
    `(fn ([~g] (~op ~@(map (fn [f] `(~(rbuild f) ~g)) fns)))
         ([~g ~h] (~op ~@(map (fn [f] `(~(rbuild f) ~g ~h)) fns)))
         ([~g ~h ~i] (~op ~@(map (fn [f] `(~(rbuild f) ~g ~h ~i)) fns))))))


(defn build-compose [fns]
  (let [g (gensym 'bcomp)]
    `(fn [& ~g]
         ~(letfn [(rec [fns]
                       (if fns
                         `(~(rbuild (first fns)) ~(rec (next fns)))
                         g))]
                 (rec fns)))))

; PG's macro:
((fn-by vector list (vector list (compose list list vector list)) list list) 1)

; Built-in clojure function composition:
((juxt list (comp (juxt list (comp list list vector list))) list list) 1)

; The built-in juxt always returns a vector, so if I wanted to use any
; other function to the results, the PG method is more flexible. OTOH,
; the clojure method correctly handles multiple arguments at the
; composed fn entry point(s).


; from ch5.clj
(defn lrec
  ([rec] (lrec rec nil))
  ([rec base]
   (fn self [lst]
       (if lst
         (rec (first lst) #(self (next lst)))
         (if (fn? base) (base) base)))))

; binds are [value recur-fn], in the book, anamorphic [it rec] are used.
(defmacro alrec
  ([binds rec]
   `(lrec (fn ~binds ~rec)))
  ([binds rec base]
   `(lrec (fn ~binds ~rec) ~base)))

(defmacro on-cdrs [binds rec base & lsts]
  `((alrec ~binds ~rec (fn [] ~base)) ~@lsts))

(defn our-copy-list [lst]
  (on-cdrs [it rec]
           (cons it (rec)) nil lst))

(defn our-find-if [fn lst]
  (on-cdrs [it rec]
           (if (fn it) it (rec)) nil lst))

(defn our-some [fn lst]
  (on-cdrs [it rec]
           (or (fn it) (rec)) nil lst))

(defn our-length [lst]
  (on-cdrs [_ rec] (inc (rec)) 0 lst))

((on-cdrs [_ rec] (inc (rec)) 0 lst) '(a b c))
((fn rec [lst] (if lst (inc (rec (next lst))) 0)) '(a b c))

(defn maxmin [args]
  (when args
    (on-cdrs [it rec]
             (let [[mx mn] (rec)]
               [(max mx it) (min mn it)])
             [(first args) (first args)]
             (next args))))

