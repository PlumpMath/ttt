;;; =================================================================
;;;  Tic-tac-toe with unbeatable computer opponent
;;;
;;;  File: core.clj
;;;  Updated: March 29 2015 by Robert Johansson
;;; =================================================================
;;
;; To start the game (computer is playing X and human is playing O):
;; (ttt (make-game "computer" "human"))
;;
;; To make the human player start the game (play as X):
;; (ttt (make-game "human" "computer"))
;; 
;; Print "i" in the game for instructions.
;;
;; Get debug info with: (ttt (debug (make-game "computer" "human")))

(ns ttt.core)

;; Initialize board and state
(defn make-board []
  " -> board"
  {:1 0, :2 0, :3 0,
   :4 0, :5 0, :6 0,
   :7 0, :8 0, :9 0})

(defn make-game [human-status-x human-status-o]
  "human-status x human-status -> state"
  {:player-x human-status-x, :player-o human-status-o,
   :board (make-board) :current-player "x" :debug false})

;; Queries about the state
(defn empty-position? [pos]
  "pos -> bool"
  (= 0 pos))

(defn empty-board? [board]
  "board -> bool"
  (reduce #(and %1 %2)
          (map empty-position?
               (list (board :1) (board :2) (board :3)
                     (board :4) (board :5) (board :6)
                     (board :7) (board :8) (board :9)))))

(defn human-playing? [state]
  "state -> bool"
  (let [current-player (state :current-player)
        player-keyword (keyword (str "player-" current-player))]
    (= "human" (state player-keyword))))

(defn moves [state]
  "state -> list of pos"
  (filter (fn [e] (empty-position?
                   ((state :board) (keyword (str e)))))
          (range 1 10)))

(defn human-status [state]
  "state -> human-status"
  (let [current-player (state :current-player)
        player-keyword (keyword (str "player-" current-player))]
    (state player-keyword)))

(defn any-full-row? [state]
  "state -> bool"
  (let [board (state :board)]
    (or (and (= (board :1) (board :2) (board :3))
             (not (empty-position? (board :1))))
        (and (= (board :4) (board :5) (board :6))
             (not (empty-position? (board :4))))
        (and (= (board :7) (board :8) (board :9))
             (not (empty-position? (board :7)))))))

(defn any-full-column? [state]
  "state -> bool"
  (let [board (state :board)]
    (or (and (= (board :1) (board :4) (board :7))
             (not (empty-position? (board :1))))
        (and (= (board :2) (board :5) (board :8))
             (not (empty-position? (board :2))))
        (and (= (board :3) (board :6) (board :9))
             (not (empty-position? (board :3)))))))

(defn any-full-diagonal? [state]
  "state -> bool"
  (let [board (state :board)]
    (or (and (= (board :1) (board :5) (board :9))
             (not (empty-position? (board :1))))
        (and (= (board :3) (board :5) (board :7))
             (not (empty-position? (board :3)))))))

(defn winner-board? [state]
  "state -> bool"
  (cond (empty-board? (state :board)) false
        (any-full-row? state) true
        (any-full-column? state) true
        (any-full-diagonal? state) true
        :else false))

(defn draw? [state]
  "state -> bool"
  (and (empty? (moves state))
       (not (any-full-row? state))
       (not (any-full-column? state))
       (not (any-full-diagonal? state))))

(defn game-over? [state]
  "state -> bool"
  (or (winner-board? state) (draw? state)))

(defn valid-move? [pos state]
  "pos x state -> bool"
  (letfn [(member [e l]
            (if (empty? l) false
                (if (= e (first l)) true
                    (member e (rest l)))))]
    (if (number? pos)
      (and (> pos 0)
           (< pos 10)
           (member pos (moves state)))
      false)))

;; Functions that operate on the state
(defn toggle-current-player [current-player]
  "current-player -> current-player"
  (if (= current-player "x") "o" "x"))

(defn update-board [board pos tic]
  "board x pos x tic -> board"
  (assoc board (keyword (str pos)) tic))

;; Minimax
(defn result [move state]
  "state -> state"
  (-> state
      (update-in [:board] update-board move (state :current-player))
      (update-in [:current-player] toggle-current-player)))
  
(defn return-winning-score [state]
  "state -> value"
  (let [previous-player
        (toggle-current-player (state :current-player))]
    (cond (draw? state) 0
          (winner-board? state) (if (= "x" previous-player) 10 -10)
          :else "ERROR")))

(declare min-value)

(defn max-value [state]
  "state -> value"
  (if (game-over? state) (return-winning-score state)
      (let [next-states
            (map (fn [m] (result m state)) (moves state))]
        (apply max (map min-value next-states)))))
        
(defn min-value [state]
  "state -> value"
  (if (game-over? state) (return-winning-score state)
      (let [next-states
            (map (fn [m] (result m state)) (moves state))]
        (apply min (map max-value next-states)))))

(defn find-max-position [max-val position-list value-list]
  "value x list of pos x list of values -> pos"
  (cond (empty? position-list) (println "ERROR")
        (= max-val (first value-list)) (first position-list)
        :else (find-max-position
               max-val (rest position-list) (rest value-list))))

(defn find-min-position [min-val position-list value-list]
  "value x list of pos x list of values -> pos"
  (cond (empty? position-list) (println "ERROR")
        (= min-val (first value-list)) (first position-list)
        :else (find-min-position
               min-val (rest position-list) (rest value-list))))

(defn minimax-decision [state]
  "state -> pos"
  (let [actions (moves state)
        next-states (map (fn [m] (result (str m) state)) actions)
        min-values (map (fn [s] (min-value s)) next-states)
        max-values (map (fn [s] (max-value s)) next-states)
        max-among-mins (apply max min-values)
        min-among-maxs (apply min max-values)]
    (if (state :debug)
      (do (println "Current player is " (state :current-player))
          (println "max-val is " max-among-mins)
          (println "min-val is " min-among-maxs)
          (println "list of positions is " actions)
          (println "list of max-values is " max-values)
          (println "list of min-values is " min-values)))
    (if (= "x" (state :current-player))
      (let [minimax
            (find-max-position max-among-mins actions min-values)]
        (if (state :debug)
          (println "Suggested Minimax position is " minimax))
        minimax)
      (let [minimax
            (find-min-position min-among-maxs actions max-values)]
        (if (state :debug)
          (println "Suggested Minimax position is " minimax))
        minimax))))

;; Misc functions
(defn debug [state]
  "state -> state"
  (assoc-in state [:debug] true))

(defn print-information []
  " -> "
  (println "This is information about the game:")
  (println "Press 'i' to show this information.")
  (println "Press 'p' to print the board.")
  (println "Press 'm' to make a move (computer or human).")
  (println "Press 'q' to quit the game.")
  (println ""))

(defn print-board [state]
  "state -> "
  (let [board (state :board)]
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
          :else (println "Something went wrong."))))
  
;; Ask for moves
(defn ask-human-for-move []
  " -> pos"
  (println "Enter move (1-9) => ")
  (read-string (read-line)))

(defn ask-computer-for-move [state]
  "state -> pos"
  (minimax-decision state))

;; Input functions
(defn get-input [state]
  "state -> string"
  (let [turn (case (state :current-player) "x" 1 "o" 2)]
    (println (str "Player " turn " ("
                  (human-status state) "): => "))
    (read-line)))

;; Main loop
(defn ttt [state]
  "state -> state"
  (if (game-over? state)
    (do (declare-winner state) (print-board state))
    (let [input (get-input state)]
      (case input
        "i" (do (print-information)
                (ttt state))
        "q" (println "Good bye!")
        "p" (do (print-board state)
                (ttt state))
        "t" (ttt (-> state
                     (update-in [:current-player]
                                toggle-current-player)))
        "m" (if (human-playing? state)
              (let [move (ask-human-for-move)]
                (if (valid-move? move state)
                  (ttt (-> state
                           (update-in [:board] update-board move
                                      (state :current-player))
                           (update-in [:current-player]
                                      toggle-current-player)))
                  (do (println "Not a valid move.") (ttt state))))
              (ttt (-> state
                       (update-in [:board] update-board
                                  (ask-computer-for-move state)
                                  (state :current-player))
                       (update-in [:current-player]
                                  toggle-current-player))))
        (do (println "Not valid input.") (ttt state))))))
