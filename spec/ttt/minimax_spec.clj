(ns ttt.minimax-spec
  (:require [speclj.core :refer :all]
            [ttt.state :refer :all]
            [ttt.minimax :refer :all]))

(def testboard1 (update-board
                 (update-board
                  (update-board (make-board) 1 "x") 2 "o") 3 "x"))

(def testboard2 (update-board
                 (update-board
                  (update-board
                   (update-board (make-board) 1 "x") 2 "o") 3 "x") 4 "o"))

(def testboard3 (update-board
                 (update-board
                  (update-board
                   (update-board
                    (update-board
                     (make-board) 1 "x") 2 "x") 3 "x") 4 "o") 5 "o"))

(def testboard4 (update-board
                 (update-board
                  (update-board
                   (update-board
                    (update-board
                     (update-board
                      (update-board
                       (update-board
                        (update-board
                         (make-board) 1 "o") 2 "x") 3 "x")
                      4 "x") 5 "o") 6 "o")
                   7 "o") 8 "x") 9 "x"))
  
(def no-winner-state-1 (assoc (make-game "computer" "human") :board testboard1))

(def no-winner-state-2 (assoc (make-game "computer" "human") :board testboard2))

(def winner-state-x (assoc (make-game "computer" "human")
                           :board testboard3
                           :current-player "o"))

(def draw-state (assoc (make-game "computer" "human")
                       :board testboard4
                       :current-player "o"))




