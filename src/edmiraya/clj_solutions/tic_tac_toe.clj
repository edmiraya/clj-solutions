(ns edmiraya.clj-solutions.tic-tac-toe)

#_("https://www.4clojure.com/problem/73"
   "Analyze a Tic-Tac-Toe Board")
(defn get-winner [rows]
  (let [columns (apply map list rows)
        diagonals (map #(map nth % (range)) [rows (rseq rows)])]
    (->> (concat rows columns diagonals)
         (some #(when (apply = %) (#{:o :x} (first %)))))))