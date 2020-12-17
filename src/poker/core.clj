(ns poker.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]))

(def suits ["hearts" "spades" "clubs" "diamonds"])
(def cards (into ["A" "K" "Q" "J"] (map str (range 2 11))))
(def deck (map vec (combo/cartesian-product suits cards)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
