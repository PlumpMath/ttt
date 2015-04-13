(ns ttt.minimax
  (:use [ttt.state]))

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
          :else (throw (Exception. "Something went wrong.")))))

(declare min-value)

(defn max-value [state]
  "state -> value"
  (letfn [(find-max-value [v l]
            (if (empty? l) v
                (find-max-value (max v (min-value (result (first l) state)))
                                (rest l))))]
    (if (game-over? state) (return-winning-score state)
        (find-max-value -10 (moves state)))))

(defn min-value [state]
  "state -> value"
  (letfn [(find-min-value [v l]
            (if (empty? l) v
                (find-min-value (min v (max-value (result (first l) state)))
                                (rest l))))]
    (if (game-over? state) (return-winning-score state)
        (find-min-value 10 (moves state)))))

(declare min-value-ab)

(defn max-value-ab [state a b]
  "state x alpha x beta -> value"
  (letfn [(find-max-value [v l a b]
            (cond (empty? l) v
                  (>= v b) v
                  :else (find-max-value (max v (min-value-ab (result (first l) state) a b))
                                        (rest l) (max a v) b)))]
    (if (game-over? state) (return-winning-score state)
        (find-max-value -10 (moves state) a b))))

(defn min-value-ab [state a b]
  "state x alpha x beta -> value"
  (letfn [(find-min-value [v l a b]
            (cond (empty? l) v
                  (<= v a) v
                  :else (find-min-value (min v (max-value-ab (result (first l) state) a b))
                                        (rest l) a (min b v))))]
    (if (game-over? state) (return-winning-score state)
        (find-min-value 10 (moves state) a b))))

(defn find-max-position [max-val position-list value-list]
  "value x list of pos x list of values -> pos"
  (cond (empty? position-list) (throw (Exception. "Something went wrong."))
        (= max-val (first value-list)) (first position-list)
        :else (find-max-position
               max-val (rest position-list) (rest value-list))))

(defn find-min-position [min-val position-list value-list]
  "value x list of pos x list of values -> pos"
  (cond (empty? position-list) (throw (Exception. "Something went wrong."))
        (= min-val (first value-list)) (first position-list)
        :else (find-min-position
               min-val (rest position-list) (rest value-list))))

(defn minimax-decision [state]
  "state -> pos"
  (if (= "x" (state :current-player))
    (let [actions (moves state)
          next-states (map (fn [m] (result (str m) state)) actions)
          min-values (map (fn [s] (min-value-ab s -10 10)) next-states)
          max-among-mins (apply max min-values)
          minimax (find-max-position max-among-mins actions min-values)]
      minimax)
    (let [actions (moves state)
          next-states (map (fn [m] (result (str m) state)) actions)
          max-values (map (fn [s] (max-value-ab s -10 10)) next-states)
          min-among-maxs (apply min max-values)
          minimax (find-min-position min-among-maxs actions max-values)]
      minimax)))

