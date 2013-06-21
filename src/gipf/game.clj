(ns gipf.core
  (:import (gipfj Board GameState Reserves IDRNode GameCalc)))

;; This file should contain all the interface between the logic
;; and the rest of the game... Yeah. Right.

(setup-move-ranking-func! rank-board-hybrid idr-ab-ranking 6 100)

(defn compound-ai-move
  [board ^long player ^Reserves reserves adv-phase]

  ;; we could do an (if (pos? player) (smrf! options) (smrf!
  ;; options));
  ;; or best yet, give a player arg to smrf!, for easier testing
  
  ;; we assume the opening strategy ignores the gipfiness when in
  ;; :filling mode
  (swap! ranks-count (constantly 0)) 
  (let [pieces-left (get-reserves reserves player)
        possible-moves (shuffle
                        (list-possible-moves-and-board board reserves player))
        ngipfs (count-over-hex-array board (* 2 player))
        degree (if (and (= adv-phase :filling) (< ngipfs 4)) 2 1)
        current-rank (move-ranking-func* (->GameState board reserves) player)
        optimal (timec (rand-best
                       (fn [[move [board res]]]
                         (let [rank
                               (timec (move-ranking-func*
                                      (->GameState board res)
                                      player))]
                           ;(println "Rank:" rank)
                           (direct-visualize-ai-ranking (second move) (- rank current-rank))
                           rank))
                       nil -100000 possible-moves))
        [c1 m c2] (first (or optimal (rand-nth possible-moves)))]
    (println "Nodes evaluated:" @ranks-count)
    ;; best would be, until mouse is moved...
    (busy-doing-important-stuff 1.0)
    ;; note positive sig
    
    [c1 (sign-line m degree) c2]))



(defn get-gipf-potentials-in-line
  [board line]
  (loop [cur (line-start line) fps (list)]
    (if (= 4 (pt-radius cur))
      fps
      (if (= (abs (get-hex-array board cur)) 2)
        (recur (pt+ cur (line-delta line)) (cons cur fps))
        (recur (pt+ cur (line-delta line)) fps)))))

(defn get-own-gipf-potentials-in-line
  [board player line]
  (filter #(same-sign? (get-hex-array board %1) player)
          (get-gipf-potentials-in-line board line)))



;; new!

(defn new-board
  "Return a newly set up board."
  [mode]
  (if (= mode :advanced)
    (make-hex-array)
    (let [m (if (= mode :basic) 1 2)]
      (applyto-repeatedly
       #(change-hex-array %1 %2 %3)
       (make-hex-array)
       [(pt 3 0 0) m]
       [(pt 0 3 0) (- m)]
       [(pt 0 0 3) m]
       [(pt -3 0 0) (- m)]
       [(pt 0 -3 0) m]
       [(pt 0 0 -3) (- m)]))))


(defn new-reserves
  "Return a new set of reserves."
  [mode]
  (case mode
    :basic (->Reserves 3 1)
    :advanced (->Reserves 18 0)
    :normal (->Reserves 15 3)))

(defn lost?
  [board reserves player mode advm]
  ;; TODO: make the GameState advm phase aware...; 
  ;; then allow gipfs to be placed under advm
  (let [res (incrementally-list-state-continuations (->GameState board reserves) player)]
    (println "Moves available:" (count res))
    (empty? res)))
