; argument capture bug was caught by the clojure compiler :)
(defmacro our-for [[var start stop] & body]
 `(loop [~var ~start limit ~stop]
    ~@body
    (when (< ~var limit) (recur (inc ~var) limit))))

; clojure accepted the debugged version:
(defmacro our-for [[var start stop] & body]
 `(loop [~var ~start limit# ~stop]
    ~@body
    (when (< ~var limit#) (recur (inc ~var) limit#))))

(our-for [x 1 5] (print x))
(our-for [limit 1 5] (print limit))
(let [limit 5]
  (our-for [i 1 10]
    (when (> i limit) (print i))))


; 9.2... This macro changes w which it has no unique owership of.
; Poor form to cause side effects like this.
(def w nil)
(defmacro gripe [warning]
  `(do
     (def w (cons w (list ~warning)))
     nil))

(do (gripe "sample < 2")
  w)

