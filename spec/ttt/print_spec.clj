(ns ttt.print-spec
  (:require [speclj.core :refer :all]
            [ttt.state :refer :all]
            [ttt.print :refer :all]))

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
  
(def no-winner-state (assoc (make-game "computer" "human") :board testboard2))

(def winner-state-x (assoc (make-game "computer" "human")
                           :board testboard3
                           :current-player "o"))

(describe "empty-position"

  (it "returns true for 0"
    (let [result (empty-position? 0)]
      (should= true result)))

  (it "returns false for other values than 0"
    (let [result (empty-position? 7)]
      (should= false result))))

(describe "winner tests"
  
  (it "throws exception"
    (should-throw Exception (declare-winner no-winner-state))))
