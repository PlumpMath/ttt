(ns ttt.state)

;; Initialize board and state
(defn make-board []
  " -> board"
  {:1 0, :2 0, :3 0,
   :4 0, :5 0, :6 0,
   :7 0, :8 0, :9 0})

(defn make-game [human-status-x human-status-o]
  "human-status x human-status -> state"
  {:player-x human-status-x, :player-o human-status-o,
   :board (make-board) :current-player "x"})

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
