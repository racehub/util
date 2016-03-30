(ns racehub.algebra
  "Shared algebras for cljs and clj.")

(defprotocol Semigroup
  (plus [l r]))

;; Shared extensions
(extend-protocol Semigroup
  nil
  (plus [l r] r))

#?(:cljs
   (extend-protocol Semigroup
     string
     (plus [l r] (str l r))

     number
     (plus [l r] (+ l r))

     cljs.core/PersistentVector
     (plus [l r] (into l r))

     cljs.core/List
     (plus [l r] (concat l r))

     cljs.core/LazySeq
     (plus [l r]
       (lazy-cat l r))

     cljs.core/PersistentArrayMap
     (plus [l r] (merge-with plus l r))

     cljs.core/PersistentHashMap
     (plus [l r] (merge-with plus l r))))


;; Clojure-specific extension
#?(:clj
   (extend-protocol Semigroup
     String
     (plus [l r] (str l r))

     java.lang.Integer
     (plus [l r] (+ l r))

     java.lang.Double
     (plus [l r] (+ l r))

     java.lang.Float
     (plus [l r] (+ l r))

     java.lang.Long
     (plus [l r] (+ l r))

     clojure.lang.Ratio
     (plus [l r] (+ l r))

     clojure.lang.IPersistentVector
     (plus [l r] (concat l r))

     clojure.lang.IPersistentList
     (plus [l r] (concat l r))

     clojure.lang.IPersistentMap
     (plus [l r]
       (merge-with plus l r))

     clojure.lang.LazySeq
     (plus [l r]
       (lazy-cat l r))))

(defn monoid [zero]
  (fn
    ([] zero)
    ([l r] (plus l r))))

(defn sum [mon coll]
  (if (empty? coll)
    (mon)
    (reduce mon coll)))

(def numeric-monoid
  (monoid 0))

(def map-monoid
  (monoid {}))

(def vector-monoid
  (monoid []))
