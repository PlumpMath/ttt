(ns ttt.print
  (:use [ttt.state]))

;; Misc functions
(defn print-information []
  " -> "
  (println "")
  (println "This is information about the game:")
  (println "Press 'i' to show this information.")
  (println "Press 'p' to print the board.")
  (println "Press 'm' to make a move (computer or human).")
  (println "Press 'q' to quit the game.")
  (println ""))

(defn print-board [state]
  "state -> "
  (let [board (state :board)]
    (println "")
    (println "Current state of the board:")
    (println (board :1) " " (board :2) " " (board :3))
    (println (board :4) " " (board :5) " " (board :6))
    (println (board :7) " " (board :8) " " (board :9))
    (println)))

(defn declare-winner [state]
  "state -> "
  (let [previous-player
        (toggle-current-player (state :current-player))]
    (cond (winner-board? state)
          (println "The winner is " previous-player)
          (draw? state) (println "It's a draw!")
          :else (throw (Exception. "Something went wrong.")))))
