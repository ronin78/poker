(ns poker.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [emoji.core :as e]))

(def suits (mapv e/emojify [":hearts:" ":spades:" ":clubs:" ":diamonds:"]))
(def cards (into ["A" "K" "Q" "J"] (map str (range 2 11))))
(def deck (shuffle (map vec (combo/cartesian-product cards suits))))
(def num-cards-in-hand 5)

(def deal-card (fn [hand d]
                    [(into hand (first deck)) (vec (rest deck))]))

(defn deal-hand
  ([n d]
   (deal-hand n [] d)
   )
  ([n h d]
   (loop [i n
          hand h
          deck d]
     (cond
       (= i 0) [hand deck]
       :else (recur (- i 1) (conj hand (first deck)) (vec (rest deck)))))))

(defn deal-players [player-vec num-cards d]
  (loop [v player-vec
         player-hands {}
         deck d]
    (cond
      (empty? v) [player-hands deck]
      :else (let [[hand rest-of-deck] (deal-hand num-cards deck)]
              (recur (rest v) (conj player-hands {(keyword (first v)) {:hand hand}}) rest-of-deck)))))

(defn exchange-card [hand i deck discard]
  (conj (deal-hand 1 (concat (subvec hand 0 i)
                             (subvec hand (inc i))) deck)
        (conj discard (subvec hand i (inc i)))))
(defn reorder-hand [hand new-order]
  (cond
    (not (= (count hand) (count new-order))) "Please enter an order for each card in your hand."
    :else (mapv #(get hand %) new-order)))

(defn swap-cards-in-hand [hand x y]
  (let [fetch (fn [i] (subvec hand i (inc i)))
        [card1 card2] (mapv fetch [x y])]
    (-> hand
        (assoc x card2)
        (assoc y card1))))

(defn move [w m]
  (let [hand (get-in w [:player :hand])
        {deck :deck
         discard :discard
         turn :turn} w]
    (case m
      "exchange" (let [[new-hand new-deck new-discard] (do "What card would you like to exchange?"
                                                           (exchange-card hand (read-line) deck discard))]
                   (-> w
                       (assoc-in [:players turn :hand] new-hand)
                       (assoc :deck new-deck)
                       (assoc :discard new-discard))))))
(defn play [w]
  (do (println (str "It is " (name (:turn w)) "'s turn!"))
      (println w)
      ))

(defn start [player-vec]
  (let [pv player-vec
        d deck
        discard []]
    (play (zipmap [:players :deck :discard :turn] (conj (deal-players pv 5 d) discard (keyword (rand-nth player-vec)))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
