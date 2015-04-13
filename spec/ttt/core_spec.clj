(ns ttt.core-spec
  (:require [speclj.core :refer :all]
            [ttt.state :refer :all]
            [ttt.core :refer :all]))

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

;; Tests for input functions

(describe "ttt.core"
  (around [it]
          (with-out-str (it)))
  
  (it "tests asking human for a move"
    (should= 9
             (with-in-str "9"
               (ask-human-for-move)))))
