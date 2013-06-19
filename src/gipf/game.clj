(ns gipf.core
  (:import (gipfj Board GameState Reserves IDRNode GameCalc)))

;;
;; TODO: next order of business 
;;
;; Make a board-ranking function that actually... um... 
;; ... tries to win.
;;
;;
;;
;;
;;
;;



;; action 

(defn minimax2
  [gamestate player rank-func max? depth]
  (if (equals 0 depth)
    (if max?
      (rank-func gamestate player)
      (negate (rank-func gamestate player)))
    (let [conts (list-possible-boards gamestate player)]
      ;; NOTE: need to implement this drying up of moves..
      (if (empty? conts)
        ;; loss
        (if max? neg-ranking-infinity ranking-infinity)
        ;; continue
        (reduce (if max? #(fastmax %1 %2) #(fastmin %1 %2))
                (map
                 (fn [new-b-and-r]
                   (minimax2 new-b-and-r
                             (negate player)
                             rank-func
                             (not max?)
                             (dec-1 depth)))
                 conts))))))


;; take a node. replace (loop) it with the rank of its children, and
;; add pointers. Loop over all nodes.
;;  .  .  .  .
;;     .  .  .
;;        .  .
;;           .

(defn idr-sub
  ;; why can't I hint the first node?
  [node depth ede endtime rank-func]
  (cond
   (and (equals depth 0) (past-time? endtime)) node
   (equals depth ede)
   (let [subgs (list-possible-boards (idr-node-gamestate node) (idr-node-player node))
         newp (negate (idr-node-player node))
         childlist (map (fn [gamestate]
                          (make-idr-node
                           gamestate
                           newp
                           (if (fast-even? ede)
                             (rank-func gamestate newp)
                             (negate (rank-func gamestate newp)))))
                        subgs)
         rank (if (fast-even? ede)
                (reduce #(fastmax %1 %2) (map #(idr-node-rank %) childlist))
                (reduce #(fastmin %1 %2) (map #(idr-node-rank %) childlist)))]
     (idr-node-update node rank childlist))
   :else
   (let [childlist (map #(idr-sub % (inc-1 depth) ede endtime rank-func)
                        (idr-node-children node))
         rank (if (fast-even? ede)
                (reduce #(fastmax %1 %2) (map #(idr-node-rank %) childlist))
                (reduce #(fastmin %1 %2) (map #(idr-node-rank %) childlist)))]
     (idr-node-update node rank childlist))))

;; I want a macro that inlines x times, then iterates in a function..
;; (optimizer)

(defn iterative-deepening-ranking
  "Ranks a position. It deepens. Iteratively."
  [gamestate player rank-func depth time]

  (let [starttime (. System (nanoTime))
        endtime (+ (* time 1e6) starttime)]
    (loop [nodetree (make-idr-node gamestate
                            player
                            (rank-func gamestate player))
           level 0]
      (if (or (past-time? endtime) (equals level depth))
        (do
          (idr-node-rank nodetree))
        (recur (idr-sub nodetree 0 level endtime rank-func) (inc-1 level))))))

(defn rank-board-1-setup
  []
  (def expected-max-rank* 100000)
     (set-value-cell-constants 15 40 -10 -50 3)
     (set-value-line-cell-constants -3 50 100 500 4))

(defn rank-board-1
  ;; why measure lines after the fact?
  "Finally kinda efficient"
  [gamestate player]
  (let [pos-points (rank-board-org gamestate player)
        lines-points (rank-board-lines gamestate player)] 
    (add (divide pos-points 7) (multiply 10 lines-points))))


(def-ranking-function rank-board-2
  (:setup
   []
   (def expected-max-rank* 800)
   ;; mp mg op og rl
   (set-value-cell-constants 1 3 -1 -3 5))
  (:eval
   [gamestate player]
   
   (let [reserves (game-state-reserves gamestate)
         antiplayer (negate player)]
     (cond (losing-reserve? reserves player)
           -1000
           (losing-reserve? reserves antiplayer)
           1000
           :else
           (let [gipf-points (subtract
                              (get-gipfs reserves player)
                              (get-gipfs reserves antiplayer))
                 piece-points (subtract (get-reserves reserves player)
                                        (get-reserves reserves antiplayer))
                 pos-points (rank-board-org gamestate player)]
             (add pos-points
                  (add
                   (multiply 200 gipf-points)
                   (multiply 50 piece-points))))))))

;; best would be to disconnect the two choices
;; between ai search algorithm and ranking heuristic

;; ie, (setup-move-ranking-func! rank-func search-func & args-to-rankfunc)  


(setup-move-ranking-func! rank-board-2 iterative-deepening-ranking 2 50)

;; move-ranking-func* comes from game-aux

(defn compound-ai-move
  [board ^long player ^Reserves reserves adv-phase]

  ;; we assume the opening strategy ignores the gipfiness when in
  ;; :filling mode
  
  (let [pieces-left (get-reserves reserves player)
        possible-moves (shuffle
                        (list-possible-moves-and-board board reserves player))
        ngipfs (count-over-hex-array board (* 2 player))
        degree (if (and (= adv-phase :filling) (< ngipfs 4)) 2 1)
        optimal (timec (rand-best
                       (fn [[move [board res]]]
                         (let [rank
                               (timec (move-ranking-func*
                                      (->GameState board res)
                                      player))]
                           ;(println (second move) "->" rank)
                           (direct-visualize-ai-ranking (second move) rank)
                           rank))
                       nil -100000 possible-moves))
        [c1 m c2] (first (or optimal (rand-nth possible-moves)))]
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
    :basic (->Reserves 12 1)
    :advanced (->Reserves 18 0)
    :normal (->Reserves 15 3)))

(defn lost?
  [board reserves player mode advm]
  (println reserves)
  (if (or (= advm :filling) (= mode :basic))
    (= 0 (get-reserves reserves player))
    (or
     (= 0 (get-reserves reserves player))
     (= 0 (get-gipfs reserves player)))))
