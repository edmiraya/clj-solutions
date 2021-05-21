(ns edmiraya.clj-solutions.primes)

#_(defn- sqrtFloor [num]
  (-> num Math/sqrt Math/floor int))

(defn- sqrtCeil [num]
  (-> num Math/sqrt Math/ceil int))

(defn rm-multiples [x coll]
  (remove #(zero? (mod % x)) coll))

(defn primes-by-limit [end]
  (let [stop (sqrtCeil end)]
    (loop [[num & rest-nums :as nums] (range 3 end 2) #_odds
           primes [2]]
      (if (> num stop)
        (concat primes nums)
        (recur (rm-multiples num rest-nums) (conj primes num))))))

(defn prime? [x]
  (and (> x 1)
       (not-any? #(zero? (mod x %)) (range 2 x))))

#_("https://www.4clojure.com/problem/67"
   "Prime Numbers")
(defn primes [n]
  (let [end (* n n)]
    (take n (primes-by-limit end))))

#_("https://www.4clojure.com/problem/67"
   "Prime Numbers - Simple")
(defn primes-simple-way [n]  
    (->> (range)
         (filter prime?)
         (take n)))

(defn- side-primes [num]
  (letfn [(first-prime [num f]
            (first (filter prime? (iterate f (f num)))))]
    (map #(first-prime num %) [dec inc])))

#_("https://www.4clojure.com/problem/116"
   "Prime Sandwich")
(defn balanced-prime? [num]
  (and (> num 2) (prime? num)
       (let [[prev nxt] (side-primes num)]
         (= (+ nxt prev) (* 2 num)))))
