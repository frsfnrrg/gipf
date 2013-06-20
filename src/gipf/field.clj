(ns gipf.core
    (:import (gipfj Board GameState Reserves IDRNode GameCalc)))

(def order-distinguishing-pause 0.6) ; sec
(def ranking-infinity (long 100000))
(def neg-ranking-infinity (long -100000))

(defrename place-and-shove `GameCalc/placeAndShove 3) 
(defrename ->GameState `GameState/makeGameState 2)
(defrename game-state-board `GameState/getBoard 1)
(defrename game-state-reserves `GameState/getReserves 1)

(defrename row-full? `GameCalc/lineFull 3)

(defrename value-cell `GameCalc/valueCell 3)
(defrename rank-board-org `GameCalc/rankBoardOrg 2)
(defrename rank-line `GameCalc/rankLine 3)
(defrename rank-board-lines `GameCalc/rankBoardLines 2)

(defrename make-idr-node `IDRNode/makeIDRNode 3)
(defrename idr-node-update `IDRNode/updateIDRNode 3)
(defrename idr-node-gamestate `IDRNode/getGameState 1)
(defrename idr-node-player `IDRNode/getPlayer 1)
(defrename idr-node-rank `IDRNode/getRank 1)
(defrename idr-node-children `IDRNode/getChildren 1)

(defrename losing-reserve? `Reserves/losingReserve 2)

(defrename set-value-line-cell-constants `GameCalc/setValueLineCellConstants 5)
(defrename set-value-cell-constants `GameCalc/setValueCellConstants 5)

;; predicates/extraction

(defn get-lines-of-four
  [board]
  (vec (GameCalc/getBoardLines board)))

(def list-of-move-lines
  (doall (reduce concat
                 (map #(let [inside-neighbors (filter
                                               (fn [p] (= (pt-radius p) 3))
                                               (map (fn [q] (pt+ q %))
                                                    (get-ring-of-hex-uv-points 1)))]
                         (map (fn [ne] (->Line % (pt- ne %)))
                              inside-neighbors))
                      (get-ring-of-hex-uv-points 4)))))

(defn get-open-moves
  "Returns a list of the available moves..."
  [board]
  ;; does it work
  (filter #(not (row-full? board (pt+ (line-start %) (line-delta %)) (line-delta %)))  
          list-of-move-lines))

(def axial-points
  (map #(pt* 4 %) unit-ring-points))

(def place-point-open?
  "Can a point be placed here?"
  (fn [board loc]
    (and (= 4 (pt-radius loc))
         (if (some #(pt= % loc) axial-points)
           (let [dekl (pt- (pt-div-4 loc))]
             (not (row-full? board (pt+ loc dekl) dekl)))
           ;; the second condition is a superset of the first. Why test?
           ;; (the second is slower...) TODO pull out, optimize!
           (let [pts (filter #(= (pt-radius %) 3)
                             (map #(pt+ % loc) unit-ring-points))]
             (some #(not (row-full? board % (pt- % loc))) pts))))))


(defn impacted-cells
  [board loc shove]
  (vec (GameCalc/getImpactedCells board loc shove)))
    

(defn do-move
  "Does not update properly"
  [board value loc shove]
  [ (game-state-board (place-and-shove
                        (->GameState board (->Reserves 0 0))
                        value
                        (->Line loc shove)))
   (impacted-cells board loc shove)])

(defn get-line-taking-orderings
  "Returns a list of [[[prot] take1 take2] newboard newreserves]
   for all possible line
   orderings"
  [board reserves player]
  ;; we go dumb, take only the first ones...
  ;; if we really want to, we can make a slower, more complete
  ;; version.
  ;; We protect ourselves..
  
  (list
   (loop [cb board rr reserves taken [] protected []]
     (let [found (filter #(same-sign? (line-sig %) player) (get-lines-of-four cb))]
       (if (empty? found)
         [(concat [protected] taken) cb rr]

         ;; this seems to work...
         (let [chosen (first found)
               delta (line-delta chosen)
               [prot nb nr]
               (loop [cur (line-start chosen) bpr [] bb cb brr rr]
                 (if (= 4 (pt-radius cur))
                   [bpr bb brr]
                  
                   (case (int (multiply (get-hex-array bb cur) player))
                     (-2 -1) ;; opponent
                     (recur (pt+ cur delta)
                            bpr
                            (change-hex-array bb cur 0)
                            brr)
                     0
                     (recur (pt+ cur delta)
                            bpr bb brr)
                     1
                     (recur (pt+ cur delta)
                            bpr
                            (change-hex-array bb cur 0)
                            (inc-reserves brr player))
                     2 ;; save own gipfs
                     (do
                       (recur (pt+ cur delta)
                         (conj bpr cur)
                         bb brr)))))]
           
           (recur nb nr (conj taken chosen) (conj protected prot))))))))

(defn expand-gamestate
  [gs]
  [(game-state-board gs) (game-state-reserves gs)])

(defn do-shove
  [board reserves player shove]
  (expand-gamestate (place-and-shove (->GameState board reserves) player shove)))

(defn get-line-taking-results
  [gamestate player]
  (vec (GameCalc/getLineTakingResults gamestate player)))

(defn fget-open-moves
  [board]
  (vec (GameCalc/getOpenMoves board)))

(defn list-possible-moves-and-board
  "Like list-possible-boards, just returns the moves along with the boards.
  ([move board reserves] ... )"
  [board reserves player]
  
  (let [lines-board (get-line-taking-orderings board reserves player)
        actions-board (expand
                       (fn [[lmove board reserves]]
                         ;; return all possible shoves after this,
                         ;; form [[lmove shove]]
                         (map
                          (fn [shove]
                            (let [[nb nr] (do-shove board reserves
                                                    player
                                                    (advance-line shove))]
                              [[lmove shove] nb nr]))
                          (get-open-moves board)))
                       lines-board)
        flines-board (expand
                      (fn [[move board reserves]]
                        (map
                         (fn [[lmove board reserves]]
                           [(conj move lmove) [board reserves]])
                         (get-line-taking-orderings board reserves player))
                        )
                      actions-board)]
    flines-board))

(defn list-possible-boards-cheat
  [gamestate player]
  (map (constantly gamestate) (range 42)))

;; one issue: it is not lazy; which could waste a bit given pruning algorithms
(defn list-possible-boards-opt
  "Costs 60% more in ai than the cheat version"
  [gamestate player]
  (vec (GameCalc/listPossibleBoards gamestate player)))
  
(def list-possible-boards list-possible-boards-opt)

(defmacro past-time?
  [time]
  `(let [newtime# (System/nanoTime)]
     (greater newtime# ~time)))


;; should make this easily changeable... (per menu?; with registering stuf)
(def expected-max-rank* nil)
(def move-ranking-func* nil)

(defrecord Heuristic [setup eval])

(defn setup-move-ranking-func!
  "evalf must take 2 args: gamestate, player, and
  return a number. setupf does whatever"
  [lead-heuristic search-func & sfargs]
  ((:setup lead-heuristic))
  (def move-ranking-func*
    (fn [state player]
      (apply search-func state player (:eval lead-heuristic) sfargs))))

(defn dfr-helper
  [name doc setupexprs evalarg1 evalarg2 evalexprs]
  `(def ~name ~doc (->Heuristic (fn [] ~@setupexprs)
                           (fn [~evalarg1 ~evalarg2] ~@evalexprs))))

(defmacro def-ranking-function
  "Example input:

  (def-ranking-function oogscape
    (:eval [g p] (println \"rankin'\" (* p 20 (random-long -10 10))))
    (:setup [] (initialize-random-float)))
"
  [name [key1 [& key1args] & key1exprs] [key2 [& key2args] & key2exprs]]
  (cond (and (= key1 :setup) (= key2 :eval)
             (empty? key1args)
             (= 2 (count key2args)))
        (dfr-helper name "" key1exprs (first key2args) (second key2args) key2exprs)
        (and (= key2 :setup) (= key1 :eval)
             (empty? key2args)
             (= 2 (count key1args)))
        (dfr-helper name "" key2exprs (first key1args) (second key1args) key1exprs)
        :else
        (throw (IllegalArgumentException. "wrong clauses to def-ranking-function"))))


(def timing** false)
(defmacro
  timec
  [expr]
  (if timing**
    `(time ~expr)
    expr))
