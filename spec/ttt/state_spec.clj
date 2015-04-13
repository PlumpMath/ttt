(ns ttt.state-spec
  (:require [speclj.core :refer :all]
            [ttt.state :refer :all]))

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

(describe "board"

  (it "new board is empty"
    (should-be empty-board? (make-board)))

  (it "another board is not empty"
    (should-not-be empty-board? (no-winner-state-2 :board))))

(describe "empty-position"

  (it "returns true for 0"
    (let [result (empty-position? 0)]
      (should= true result)))

  (it "returns false for other values than 0"
    (let [result (empty-position? 7)]
      (should= false result))))

(describe "moves returns a list"

  (it "returns a list of moves"
    (let [result (moves no-winner-state-2)]
      (should= '(5 6 7 8 9) result))))

(describe "tests wins and draws"

  (it "tests a winning state"
    (let [result (winner-board? winner-state-x)]
      (should result)))

  (it "tests a false draw state"
    (let [result (draw? no-winner-state-2)]
      (should-not result)))

  (it "tests a draw state"
    (let [result (draw? draw-state)]
      (should result))))

(describe "game-overs"

  (it "tests a full board"
    (let [result (game-over? draw-state)]
      (should result)))

  (it "tests a board that is not full, and no winner"
    (let [result (game-over? no-winner-state-2)]
      (should-not result)))

  (it "tests a winning board"
    (let [result (game-over? winner-state-x)]
      (should result))))


  


