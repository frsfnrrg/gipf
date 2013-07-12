(ns gipf.core
  (:import (gipfj Ranking)))

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



(def-ranking-function rank-board-simple
  (:setup
   []
   (set-emr! 80))
  (:eval
   [gamestate player]   (reserve-diff-linear (game-state-reserves gamestate)
     player 20 5 5)))

(let [weighting-board (radial-weight-array 30 10 -10 -30 5 4 3 2)]
  (def-ranking-function rank-board-hybrid
    (:setup
     []
     (set-emr! 200))
    (:eval
     [gamestate p]
     
     (let [player (long p) ;; otherwise cljr does two longcasts
           reserves (game-state-reserves gamestate)]
       (weighted-add-2
         5 (reserve-diff-quadratic reserves player 4 1 1)
         1 (apply-weight-array (game-state-board gamestate) player weighting-board))))))
  
  

(def-ranking-function rank-not-at-all
  "This is the null heuristic; note that rankings still
   come from win/loss"
  (:setup
   []
   (set-emr! (divide positive-infinity 2)))
  (:eval
   [gs p]
   0))

(let [diagp (diag-weight-array 5 1 -1 -5   10 8 2)
      radp (radial-weight-array 5 1 -1 -5   8 4 2 0)
      rank-tactical-weights (add-weight-arrays diagp 1 radp 1)]
  (def-ranking-function rank-tactical
    "Iterates over pieces; ranks by presence on lines, center, etc.
     Ultimately dumb; however, it is more efficient than using
     rank-board-org, and shows more power."
    (:setup
      []
      (set-emr! 100))
    (:eval
      [gs p]
      (apply-weight-array (game-state-board gs) p rank-tactical-weights))))

(def-ranking-function rank-gf1
  "Should be identical to Gipf for One's ranking function."
  (:setup
   []
   (set-emr! (divide positive-infinity 3)))
  (:eval [gamestate player]
    (Ranking/gf1Rank gamestate player)))
