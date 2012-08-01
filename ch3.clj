; p. 30
(defn good-reverse
  ([lst] (good-reverse lst nil))
  ([lst acc]
   (if lst
     (recur (next lst) (cons (first lst) acc))
     acc)))

(good-reverse '(1 2 3 4))

; p. 32
(defn powers [x] [x (Math/sqrt x) (* x x)])
(let [[base root square] (powers 4)]
  (list base root square))

; p. 33
; "So the rule should be: a given invocation can safely modify what it uniquely owns."
; this does not hold anymore. In multithreaded programs even this isn't
; safe.
