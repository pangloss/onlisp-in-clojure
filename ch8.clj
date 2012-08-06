(declare bounds redraw)

(defmacro with-redraw [[var objs] & body]
  `(let [[x0# y0# x1# y1#] (bounds ~objs)]
     (let [objs# (for [~var ~objs] ~@body)]
       (let [[xa# ya# xb# yb#] (bounds objs#)]
         (redraw (min x0# xa#) (min y0# ya#)
                 (max x1# xb#) (max y1# yb#))))))

(defn move-objs [objs dx dy]
  (with-redraw [o objs]
    (let [o (assoc o :x (+ (:x o) dx))
          o (assoc o :y (+ (:y o) dy))]
      o)))

(defn scale-objs [objs factor]
  (with-redraw [o objs]
    (let [o (assoc o :dx (* (:dx o) factor))
          o (assoc o :dy (* (:dy o) factor))]
      o)))

; I'd redefine it to be used as follows to be more clojure idiomatic
#_(do
  (defn move-objs [objs dx dy]
    (with-redraw [o objs]
      :x (+ (:x o) dx)
      :y (+ (:y o) dy)))
  (defn move-horiz [objs dx]
    (with-redraw [o objs]
      :x (+ (:x o) dx))))
  (defn scale-objs [objs factor]
    (with-redraw [o objs]
      :dx (* (:dx o) factor)
      :dy (* (:dy o) factor)))
