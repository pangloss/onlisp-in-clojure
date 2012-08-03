(def *nodes* (atom {}))

(defn defnode
  ([name conts] (defnode name conts nil nil))
  ([name conts yes no]
   (swap! *nodes* assoc name {:name name :contents conts :yes yes :no no})))

(do
  (defnode 'people "Is the person a man?" 'male 'female)
  (defnode 'male "Is he living?" 'liveman 'deadman)
  (defnode 'deadman "Was he American?" 'us 'them')
  (defnode 'us "Is he on a coin?" 'coin 'cidence')
  (defnode 'coin "Is the coin a penny?" 'penny 'farthing)
  (defnode 'penny 'lincoln)
  )

