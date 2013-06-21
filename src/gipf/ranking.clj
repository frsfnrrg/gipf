(ns gipf.core)

;; Notably...
;;
;; Rank board simple is too quantized: typically, it ranks
;; a large portion of gamestates identically. This leads
;; to rather random behavior; anyway, no tactical advantage
;; is sought. It can, however, look twice as
;; many nodes in as rank-board-hybrid. They seem to be equal as
;; far as win ratio...
;;
;; On the same depth though, hybrid is definitely more powerful.
;;
;; Is there any fast, efficient location heuristic?
;; Or should that be done incrementally in a subclass of GameState?
;;
;; Another test:
;; idr-ab, depth 0, hybrid 1 vs simple -1: 100- 50:50
;; idr-ab, depth 1, hybrid 1 vs simple -1: 50-  50:0
;; idr-ab, depth 2, hybrid 1 vs simple -1: 50-  0:50
;;
;;

(defrename add-weight-arrays `GeneralizedPointWeighting/mergeWeights 4)
(defrename diag-weight-array `GeneralizedPointWeighting/diagWeights 7)
(defrename radial-weight-array `GeneralizedPointWeighting/radiusWeights 8)
(defrename apply-weight-array `GeneralizedPointWeighting/calcVal 3)

(def-ranking-function rank-board-old
  "Why does this suck? It no longer fully applies.
  Ranking the board lines that are to be removed by the opponent
  (this rarely happens) is inefficent."
  (:setup []
          (def expected-max-rank* 100000)
          (set-value-cell-constants 15 40 -10 -50 3)
          (set-value-line-cell-constants -3 50 100 500 4))
  (:eval
   [gamestate player]
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
     (let [gipf-points (subtract
                        (get-gipfs reserves player)
                        (get-gipfs reserves antiplayer))
           piece-points (subtract (get-reserves reserves player)
                                  (get-reserves reserves antiplayer))]
       (add
        (multiply 20 gipf-points)
        (multiply 5 piece-points))))))

(def-ranking-function rank-board-hybrid
  (:setup
   []
   (def expected-max-rank* 200)
   ;; mp mg op og rl
   (set-value-cell-constants 10 30 -10 -30 5))
  (:eval
   [gamestate player]
   ;; temp
   (let [reserves (game-state-reserves gamestate)
         antiplayer (negate player)
         gipf-points (subtract
                      (msquare (get-gipfs reserves player))
                      (msquare (get-gipfs reserves antiplayer)))
         piece-points (subtract (msquare (get-reserves reserves player))
                                (msquare (get-reserves reserves antiplayer)))
         pos-points (rank-board-org gamestate player)]
     (add pos-points
          (add
           (multiply 20 gipf-points)
           (multiply 5 piece-points))))))

(def-ranking-function rank-not-at-all
  "Iterates over pieces; ranks by presence on lines, center, etc."
  (:setup
   []
   (def expected-max-rank* 1000))
  (:eval
   [gs p]
   1000))

(def-ranking-function rank-tactical
  "Iterates over pieces; ranks by presence on lines, center, etc."
  (:setup
   []
   (def expected-max-rank* 1000)
   ;;                         good -> bad ; strong -> weak
   (let [diagp (diag-weight-array 5 1 -1 -5   10 8 2)
         radp (radial-weight-array 5 1 -1 -5   8 4 2 0)
         res (add-weight-arrays diagp 1 radp 1)]
     (def rank-tactical-weights res)))

  (:eval
   [gs p]
   (apply-weight-array (game-state-board gs) p rank-tactical-weights)))
