(ns poker.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [emoji.core :as e]))

(def suits (mapv e/emojify [":hearts:" ":spades:" ":clubs:" ":diamonds:"]))
(def cards (into ["A" "K" "Q" "J"] (map str (range 2 11))))
(def deck (shuffle (map vec (combo/cartesian-product cards suits))))
(def num-cards-in-hand 5)

(defn string->number-vector [s]
  (vec (distinct (map #(dec (Integer/parseInt (str %))) s))))

(defn key-map [w k]
  (let [mapping {:hand [:players (:turn w) :hand]
                 :deck [:deck]
                 :bankroll [:players (:turn w) :bankroll]
                 :pot [:pot]
                 :discard [:discard]}]
    (get mapping k)))
(defn get-or-set
  ([w k]
   (cond
     (vector? k) (loop [keys k
                        rvec []]
                   (cond (empty? keys) rvec
                         :else (recur (rest keys) (conj rvec (get-or-set w (first keys))))))
     :else (get-in w (key-map w k))))
  ([w k v]
   (cond
     (vector? k) (loop [keys k
                        vals v
                        world w]
                   (cond
                     (empty? keys) world
                     :else (recur (rest keys)
                                  (rest vals)
                                  (get-or-set world (first keys) (first vals)))))
     :else (assoc-in w (key-map w k) v))))

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

(defn make-bank [w amount]
  (reduce #(assoc-in % [:players %2 :bankroll] amount) w (keys (:players w))))

(defn ante [w amount]
  (let [players (keys (:players w))]
    (-> (reduce (fn [m p] (update-in m [:players p :bankroll] #(- % amount))) w players)
        (get-or-set :pot (* amount (count players))))))

(defn exchange-card [w]
  (let [v [:hand :deck :discard]
        [hand deck discard] (get-or-set w v)
        i (do (println "What is the number of the card you would like to exchange?")
              (dec (Integer/parseInt (read-line))))]
    (get-or-set w v (conj (deal-hand 1 (vec (concat (subvec hand 0 i)
                                                    (subvec hand (inc i)))) deck)
                     (conj discard (subvec hand i (inc i)))))))

(defn reorder-hand [w]
  (let [hand (get-or-set w :hand)
        new-order (do (println "Please enter the new card order.")
                      (string->number-vector (read-line)))]
    (cond
      (not (= (count hand) (count new-order))) (println (str "Please enter " num-cards-in-hand " unique numbers."))
      :else (get-or-set w :hand (mapv #(get hand %) new-order)))))

(defn swap-cards-in-hand [w]
  (let [hand (get-or-set w :hand)
        [x y] (do (println "Please enter the two card positions you would like to swap")
                  (string->number-vector (read-line)))
        fetch (fn [i] (subvec hand i (inc i)))
        [card1 card2] (mapv fetch [x y])]
    (get-or-set w :hand (-> hand
                            (assoc x card2)
                            (assoc y card1)))))

(defn end-game [w]
  (assoc w :over true))

;; Refactor to allow for extensible dispatch and movement without these giant functions. Standard look inside functions combined with just passing w?
(defn move [w m]
  (let [hand (get-in w [:player :hand])
        {deck :deck
         discard :discard
         turn :turn} w]
    ((case m
       "exchange" exchange-card
       "reorder" reorder-hand
       "swap" swap-cards-in-hand
       "q" end-game) w)))
(defn play [w]
  (do (println (str "It is " (name (:turn w)) "'s turn! Here is your hand: " (get-or-set w :hand) "\n You have " (get-or-set w :bankroll) " in your bankroll. There is " (:pot w) " in the pot."))
      (if (not (:over w)) (recur (move w (do (println "What would you like to do?")
                                             (read-line))))
          "It's over!")))

(defn start [player-vec]
  (let [pv player-vec
        d deck
        discard []]
    (play (-> (zipmap [:players :deck :discard :turn] (conj (deal-players pv 5 d) discard (keyword (rand-nth player-vec))))
              (make-bank 100)
              (ante 10)))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
