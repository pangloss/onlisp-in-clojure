(defmacro nil! [var] `(def ~var nil))

(require 'clojure.pprint)

(let [a 1 b 2 c 3]
  (pprint [
           `(a b ~c ('~(+ a b c)) (+ a b) 'c '((~a ~b)))
           'a
           `a
   ])
  nil
  )

(defmacro nif [expr pos zero neg]
  `(cond
     (< 0 ~expr) ~pos
     (= 0 ~expr) ~zero
     (< ~expr 0) ~neg))

(map #(nif % 'p 'z 'n) [0 2.5 -8])
`(hello there)

#_(defmacro memq [x choices] `(member ~x ~choices :test eq))

(defmacro wwhile [test & body]
          `(loop []
             ~@body
             (when (~test) (recur))))

(defmacro mac [expr] `(clojure.pprint/pprint (macroexpand-1 '~expr)))

(defmacro our-dolist [[var list & result] & body]
  `(do
     (for [~var ~list] ~@body)
     ~(if result)
     (let [~var nil] ~result)))
(our-dolist (x '(a b c)) (print x))

(let [op +]
  (defmacro addd [a b] `(~op ~a ~b)))
(addd 1 2)

(defn make-initforms [bindforms]
  (mapcat (fn [b] (if (list? b)
                    (list (first b) (second b))
                    (list b nil)))
          bindforms))

; (make-initform ((w 3) (x 1 (inc x)) (y 2 (inc y)) (z)))

(defn make-recurform [bindforms]
  (map (fn [b] (if (list? b)
                              (last b)
                              b))
                    bindforms))

; (make-recurform ((w 3) (x 1 (inc x)) (y 2 (inc y)) (z)))

; I can't make this work... I don't know how to (or if it's possible to)
; delay the execution of make-initforms so that it can be spliced into a
; vector, or to execute make-initform in time that loop sees that it is
; being given a vector (by the macro)...
;   ...entire day later...
; HOLY SHIT. I read the helper functions for this macro as macros
; themselves but they are just normal functions. OOPS!
;
; I guess the idea that a macro could call a function I just defined
; seemed strange so I dismissed the idea that the helpers should be
; regular functions early on and debugged on that assumption and the
; idea that I could get a second pass from the macroexpander if I needed
; it. Learned a lot I think.
(defmacro our-do [bindforms [test & result] & body]
  ; make-initformsis being called during the creation of the macro
  ; instead of when it is executed???
  `(loop ~(vec (make-initforms bindforms))
     (if ~test
       (do
         ~@result)
       (do
         ~@body
         (recur ~@(make-recurform bindforms))))))

; At least it works.
(our-do ((w 3) (x 1 (inc x)) (y 2 (inc y)) (z)) ; set up 4 loop variables
        ((> x 10) (println z) y)                ; code to run & return when exiting loop
  (println (str w "-> " x " -> " y " & " z)))   ; loop body

; clarity over efficiency in macros. They never expand at critical times in
; typical programs.
(defmacro our-and [& args]
  (case (count args)
    0 true
    1 (first args)
    `(if ~(first args) (our-and ~@(rest args)))))
