;;; =================================================================
;;;  Tic-tac-toe with unbeatable computer opponent
;;;
;;;  File: core.clj
;;;  Updated: April 13 2015 by Robert Johansson
;;; =================================================================
;;
;; To start the game (computer is playing X and human is playing O):
;; (ttt (make-game "computer" "human"))
;;
;; To make the human player start the game (play as X):
;; (ttt (make-game "human" "computer"))
;; 
;; Print "i" in the game for instructions.

(ns ttt.core
  (:use [ttt.state])
  (:use [ttt.minimax])
  (:use [ttt.print]))
  
;; Input functions
(defn get-input [state]
  "state -> string"
  (let [turn (case (state :current-player) "x" 1 "o" 2)]
    (println (str "Player " turn " ("
                  (human-status state) "): => "))
    (read-line)))

;; Ask for moves
(defn ask-human-for-move []
  " -> pos"
  (println "Enter move (1-9) => ")
  (read-string (read-line)))

(defn ask-computer-for-move [state]
  "state -> pos"
  (minimax-decision state))

;; Main loop
(defn ttt [state]
  "state -> state"
  (if (game-over? state)
    (declare-winner state)
    (let [input (get-input state)]
      (case input
        "i" (do (print-information)
                (ttt state))
        "q" (println "Good bye!")
        "p" (do (print-board state)
                (ttt state))
        "m" (if (human-playing? state)
              (let [move (ask-human-for-move)]
                (if (valid-move? move state)
                  (let [state (-> state
                                  (update-in [:board] update-board move
                                             (state :current-player))
                                  (update-in [:current-player]
                                             toggle-current-player))]
                    (do (print-board state)
                        (ttt state)))
                  (do (println "Not a valid move.") (ttt state))))
              (let [state (-> state
                              (update-in [:board] update-board
                                         (ask-computer-for-move state)
                                         (state :current-player))
                              (update-in [:current-player]
                                         toggle-current-player))]
                (do (print-board state)
                    (ttt state))))
        (do (println "Not valid input.") (ttt state))))))
