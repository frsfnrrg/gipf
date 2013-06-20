(ns gipf.core)

(def-ranking-function rank-board-old
  (:setup []
          (def expected-max-rank* 100000)
          (set-value-cell-constants 15 40 -10 -50 3)
          (set-value-line-cell-constants -3 50 100 500 4))
  (:eval [gamestate player]
    (let [pos-points (rank-board-org gamestate player)
          lines-points (rank-board-lines gamestate player)] 
      (add (divide pos-points 7) (multiply 10 lines-points)))))

(def-ranking-function rank-board-simple
  (:setup
   []
   (def expected-max-rank* 80))
  (:eval
   [gamestate player]
   (let [reserves (game-state-reserves gamestate)
         antiplayer (negate player)]
     (cond (losing-reserve? reserves player)      -100
           (losing-reserve? reserves antiplayer)   100
           :else
           (let [gipf-points (subtract
                              (get-gipfs reserves player)
                              (get-gipfs reserves antiplayer))
                 piece-points (subtract (get-reserves reserves player)
                                        (get-reserves reserves antiplayer))]
             (add
              (multiply 20 gipf-points)
              (multiply 5 piece-points)))))))

(def-ranking-function rank-board-hybrid
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
