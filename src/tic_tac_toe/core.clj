(ns tic-tac-toe.core
  (:gen-class))

(def starting-board [1 2 3 4 5 6 7 8 9])

(def player-sequence (cycle [:x :o]))

(defn triples [board]
  (concat
    (partition-all 3 board)
    (list
      (take-nth 3 board)
      (take-nth 3 (drop 1 board))
      (take-nth 3 (drop 2 board))
      (take-nth 4 board)
      (take-nth 4 (reverse board)))))

(defn triple-winner [triple]
  (cond
    (every? #{:x} triple) :x
    (every? #{:o} triple) :o))

(defn check-winner-of-board [board]
  (->> board
       triples
       (map triple-winner)
       (some #{:x :o})))

(defn player-name [k]
  (subs (str k) 1))

(defn full-board? [board]
  (every? #{:x :o} board))

(defn move-on-board-pre-check! [board]
  (let [input (try (. Integer parseInt (read-line))
                   (catch Exception e nil))]
    (if (some #{input} board)
      input
      nil)))

(defn move-board! [player board]
  (println "It is player:" (player-name player) "to move. Hit (1-9) to choose where to place your chess.")
  (loop [checked-result-of-move (move-on-board-pre-check! board)]
    (if checked-result-of-move
      (assoc board (dec checked-result-of-move) player)
      (do
        (println "the move is not valid.Reselect your move,player:" (player-name player))
        (recur
          (move-on-board-pre-check! board))))))

(defn print-board [board]
  (let [name-board (map #(if (keyword? %) (subs (str %) 1) %) board)]
    (println (nth name-board 0) (nth name-board 1) (nth name-board 2))
    (println (nth name-board 3) (nth name-board 4) (nth name-board 5))
    (println (nth name-board 6) (nth name-board 7) (nth name-board 8))))

(defn game-begin []
  (loop [board starting-board
         player-sequence player-sequence]
    (let [winner (check-winner-of-board board)]
      (println "Current board:")
      (print-board board)
      (cond
        winner (println "winner is " (player-name winner))
        (full-board? board) (println "game is a draw.")
        :else (recur
                (move-board! (first player-sequence) board)
                (rest player-sequence))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "welcome to the tic-tac-toe game")
  (game-begin))
