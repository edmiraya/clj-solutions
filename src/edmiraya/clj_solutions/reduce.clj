(ns edmiraya.clj-solutions.reduce)

#_("my reduce")
(defn my-reduce
  ([f [a & others]] (my-reduce f a others))
  ([f init coll]
   (if-let [[a & others] (seq coll)]
     (recur f (f init a) others)
     init)))

#_("https://www.4clojure.com/problem/60"
   "Sequence Reductions")
(defn my-reductions
  ([f [a & others]] (my-reductions f a others))
  ([f init coll]
   (letfn [(results-reduce [f r coll]
             (if-let [[a & others] (seq coll)]
               (recur f (conj r (f (peek r) a)) others)
               r))]
     (results-reduce f [init] coll))))