(defmacro abbrev [short long]
 `(defmacro ~short [& args#]
    `(~~long ~@args#)))

(defmacro abbrevs [& names]
  `(do
     ~@(map (fn [[a b]]
                `(abbrev ~a ~b))
            (partition 2 names))))

;another case of clojure doing it better:
;
; Instead of an anaphoric macro a+ to make this cleaner:
(defn mass-cost [menu-price] (a+ menu-price (* it 0.05) (* it 3)))
; we've got -> or ->> :
(defn mass-cost [menu-price] (-> menu-price (* 0.05) (* 3)))

; anaphoric macros could be useful if the argument needs to be placed in
; various spots in the methods, though in my experience that's
; suprisingly unlikely. Anyway, here's a pretty clean way to do just
; that without introducing anaphorics:
(-> 3 (* 2) (* 2) (#(str "the result is " % "!")))

; umm actually on closer reading, the a+ macro is a weird combination of
; actions because its result adds up each intermediate result, a sort of
; subtotal macro...

(declare a+expand alist alist-expand)

; subtotal on chained functions
(defmacro a+ [& args]
  (a+expand args nil))

; Note worth remembering: if I use `sym# instead of (gensym), the same
; symbol name is reused in subsequent recursions and breaks the
; algorithm.
;
; MORAL: use gensym only when doing recursion!
(defn a+expand [args syms]
  (if args
    (let [it 'it sym (gensym)]
      `(let [~sym ~(first args)
             ~it ~sym]
         ~(a+expand (next args) (concat syms (list sym)))))
    `(+ ~@syms)))

(defmacro alist [& args]
  (alist-expand args nil))

(defn alist-expand [args syms]
  (if args
    (let [it 'it sym (gensym)]
      `(let [~sym ~(first args)
             ~it ~sym]
         ~(alist-expand (next args) (concat syms (list sym)))))
    `(list ~@syms)))

; clearly a pattern here...

; this is one variation of the pattern but not the one I'd do:
;
; I'm not sure how to get clojure to look up my symbol name in the
; current namespace. Not going to dig into that for now, so the
; following single arg version does not work.
;
; Struggled with the following exception again:
;   Can't use qualified name as parameter: user/argh
; The problem was that I had to change
;  `(defmacro ~name [& argh] (anaphex argh (list ~expr)))))
; to
;  `(defmacro ~name [& argh#] (anaphex argh# (list ~expr)))))
; or
;  `(defmacro ~name [& ~'argh] (anaphex ~'argh (list ~expr)))))
; to prevent argh from expanding to user/argh.
(declare anaphex)
(defmacro defaneph
  ([name]
   ; broken
   `(defmacro ~name [& argh#] (anaphex argh# ~(symbol (subs (str name) 1)))))
  ([name expr]
   `(defmacro ~name [& argh#] (anaphex argh# (list ~expr)))))

(defn anaphex [args expr]
  (if args
    (let [it 'it sym (gensym)]
      `(let [~sym ~(first args)
             ~it ~sym]
         ~(anaphex (next args) (concat expr (list sym)))))
    expr))

(defaneph a+ +)
(defaneph al list)
(defaneph av vector)
(a+ 10 (* it 0.15) it)
(al 10 (* it 0.15) it) ; not sure why this doesn't work... (eval `(~list 1 2)) doesn't either...
(av 10 (* it 0.15) it)

(declare anaphex2 anaphex3)
(defmacro defaneph
  ([name expr] `(defaneph ~name ~expr :all))
  ([name expr rule]
   (let [args `args#
         body (case rule
                :all `(anaphex ~args (list ~expr))
                :first `(anaphex2 '~expr ~args)
              ; :place `(anaphex3 '~expr ~args)
                )]
     `(defmacro ~name [& ~args] ~body))))

(defn anaphex2 [op args]
  `(let [~'it ~(first args)]
     (~op ~'it ~@(next args))))

; Not implementing anaphex3 since it's about mutating a global...
