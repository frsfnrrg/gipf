(ns gipf.core
  (:import (gipfj Geometry MathUtil Board GameState Reserves Line
                  IDRNode GameCalc IncrementalGameCalc GeneralizedPointWeighting
                  LTranspTable CompressedSGS)))

(definline place-and-shove [a b c] `(GameCalc/placeAndShove ~a ~b ~c))
(definline ->GameState [a b] `(GameState/makeGameState ~a ~b))
(definline game-state-board [gs] `(GameState/getBoard ~gs))
(definline game-state-reserves [gs] `(GameState/getReserves ~gs))

(definline row-full? [b s e] `(GameCalc/lineFull ~b ~s ~e))

(definline make-idr-node [a b c] `( IDRNode/makeIDRNode ~a ~b ~c))
(definline idr-node-update [a b c] `( IDRNode/updateIDRNode ~a ~b ~c))
(definline idr-node-gamestate [a] `( IDRNode/getGameState ~a))
(definline idr-node-player [a] `( IDRNode/getPlayer ~a))
(definline idr-node-rank [a] `( IDRNode/getRank ~a))
(definline idr-node-children [a] `( IDRNode/getChildren ~a))

(definline add-weight-arrays [aa aw ba bw]
  `(GeneralizedPointWeighting/mergeWeights ~aa ~aw ~ba ~bw))
(definline diag-weight-array [mg mp op og c d e]
  `(GeneralizedPointWeighting/diagWeights ~mg ~mp ~op ~og ~c ~d ~e))
(definline radial-weight-array [mg mp op og r0 r1 r2 r3]
  `(GeneralizedPointWeighting/radiusWeights ~mg ~mp ~op ~og ~r0 ~r1 ~r2 ~r3))
(definline apply-weight-array [b p ar]
  `(GeneralizedPointWeighting/calcVal ~b ~p ~ar))
(definline total-weight-sub-array [center r1 star2 mid2 star3 mid3]
  `(GeneralizedPointWeighting/totalControlSubLevel ~center ~r1 ~star2 ~mid2 ~star3 ~mid3))
(definline total-weight-array [pg pp np ng]
  `(GeneralizedPointWeighting/totalControl ~pg ~pp ~np ~ng))

(definline weighted-add-2 [va ca vb cb]
  `(Ranking/weightedAdd ~va ~ca ~vb ~cb))
(definline weighted-add-3 [va ca vb cb vc cc]
  `(Ranking/weightedAdd ~va ~ca ~vb ~cb ~vc ~cc))
(definline weighted-add-4 [va ca vb cb vc cc vd cd]
  `(Ranking/weightedAdd ~va ~ca ~vb ~cb ~vc ~cc ~vd ~cd))

(definline balance-normalize [good bad]
  `(Ranking/balanceNormalize ~good ~bad))

(definline reserve-diff-linear [r p gipfs pieces store]
  `(Ranking/reserveLinearDiff ~r ~p ~gipfs ~pieces ~store))
(definline reserve-diff-quadratic [r p gipfs pieces store]
  `(Ranking/reserveQuadDiff ~r ~p ~gipfs ~pieces ~store))
(definline reserve-diff-cubic [r p gipfs pieces store]
  `(Ranking/reserveCubicDiff ~r ~p ~gipfs ~pieces ~store))

(definline lazy-next-gamestates
  [gamestate player]
  `(from-iterator (IncrementalGameCalc. ~gamestate ~player)))

(definline make-transp-table
  "Advised coefficients: pool exponent large - 20, 22; array exponent small 0 or 1.
  I do not know how much it costs to do an equality comparison...
  At 10^6 nodes, the pool is filled with mainly 1-4 items, log style."
  [poole]
  `(LTranspTable/tmake ~poole))
(definline get-transp-table
  [table key]
  `(LTranspTable/tget ~table ~key))
(definline add-transp-table
  [table key val]
  `(LTranspTable/tadd ~table ~key ~val))
(definline size-transp-table
  [table]
  `(LTranspTable/tsize ~table))
(definline flush-transp-table
  [table]
  `(LTranspTable/tclear ~table))

(definline make-signed-gamestate
  [gamestate player]
  `(CompressedSGS/compress ~gamestate ~player))


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
                        (->GameState board null-reserves)
                        value
                        (->Line loc shove)))
   (impacted-cells board loc shove)])

(defn do-linemoves
  [gs player move]
  (let [prot (ffirst move)]
    (loop [lleft (rest move) gamestate gs]
      (if (empty? lleft)
        gamestate
        (recur
         (rest lleft)
         (let [line (first lleft)
               delta (line-delta line)]
           (loop [pos (line-start line)
                  board (game-state-board gamestate)
                  reserves (game-state-reserves gamestate)]
             (if (= (pt-radius pos) 4)
               (->GameState board reserves)
               (let [val (get-hex-array board pos)]
                 (cond (some #(pt= pos %) prot)
                       (recur (pt+ pos delta) board reserves)
                       
                       (same-sign? val player)
                       (if (equals 2 (abs val))
                         (recur (pt+ pos delta)
                                (change-hex-array board pos 0)
                                (reserve-delta reserves player 2 0 -1))
                         (recur (pt+ pos delta)
                                (change-hex-array board pos 0)
                                (reserve-delta reserves player 1 -1 0)))

                       (not (equals val 0))
                       (if (equals 2 (abs val))
                         (recur (pt+ pos delta)
                                (change-hex-array board pos 0)
                                (reserve-delta reserves (negate player) 0 -1 0))
                         (recur (pt+ pos delta)
                                (change-hex-array board pos 0)
                                reserves))
                       
                       :else
                       (recur (pt+ pos delta) board reserves)))))))))))

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
                     -2
                     (recur (pt+ cur delta)
                            bpr
                            (change-hex-array bb cur 0)
                            (reserve-delta brr (negate player)
                              0 0 -1))
                     -1 ;; opponent
                     (recur (pt+ cur delta)
                            bpr
                            (change-hex-array bb cur 0)
                            (reserve-delta brr (negate player)
                              0 -1 0))
                     0
                     (recur (pt+ cur delta)
                            bpr bb brr)
                     1
                     (recur (pt+ cur delta)
                            bpr
                            (change-hex-array bb cur 0)
                            (reserve-delta brr player 1 -1 0))
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

;; should make this easily changeable... (per menu?; with registering stuf)
(def expected-max-rank* nil)
(def move-ranking-func* nil)

(defrecord Heuristic [setup eval])

(let [mrfs (atom {})]
  (defn setup-move-ranking-func!
    [player ^Heuristic lead-heuristic search-func & sfargs]
    (swap! mrfs
             #(assoc % player [(:setup lead-heuristic)
                               (fn [state player]
                                 (apply search-func state player
                                        (:eval lead-heuristic) sfargs))])))


  (defn use-move-ranking-func!
    [player]
    (let [rr (get @mrfs player)
          [setupf lmda] rr]
      (setupf)
      (def move-ranking-func* lmda))))


(def ranks-count (atom (long 0)))

(defn dfr-helper
  [name doc setupexprs evalarg1 evalarg2 evalexprs]
  ;; odd behavior: (instance? Heuristic %) only always works if the Heuristic
  ;; was made using the (Heuristic. ) method, not the (->Heuristic
  ;; map) method. Whatever
  `(def ~name ~doc (Heuristic.
                    (fn [] ~@setupexprs)
                    (fn ~name [~evalarg1 ~evalarg2]
                      (swap! ranks-count #(inc-1 %))
                      ~@evalexprs))))

(defmacro def-ranking-function
  "Example input:

  (def-ranking-function oogscape
    (:eval [g p] (println \"rankin'\" (* p 20 (random-long -10 10))))
    (:setup [] (initialize-random-float)))
"
  ([name [key1 [& key1args] & key1exprs] [key2 [& key2args] & key2exprs]]
     `(def-ranking-function ~name ""
        (~key1 [~@key1args] ~@key1exprs)
        (~key2 [~@key2args] ~@key2exprs)))
  ([name docstring [key1 [& key1args] & key1exprs] [key2 [& key2args] & key2exprs]]
     (cond (and (= key1 :setup) (= key2 :eval)
                (empty? key1args)
                (= 2 (count key2args)))
           (dfr-helper name docstring key1exprs (first key2args) (second key2args) key2exprs)
           (and (= key2 :setup) (= key1 :eval)
                (empty? key2args)
                (= 2 (count key1args)))
           (dfr-helper name docstring key2exprs (first key1args) (second key1args) key1exprs)
           :else
           (throw (IllegalArgumentException. "wrong clauses to def-ranking-function")))))

(defn print-board
  [board]
  ;; I want macrolet ;-)
  (let [enm (str "0123456789" "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        transform
        (str
         "         j            \n"
         "      k     A         \n"
         "   l     7     z      \n"
         "m     8     i     y   \n"
         "   9     1     h      \n"
         "n     2     6     x   \n"
         "   a     0     g      \n"
         "o     3     5     w   \n"
         "   b     4     f      \n"
         "p     c     e     v   \n"
         "   q     d     u      \n"
         "      r     t         \n"
         "         s            \n")
        
        zmap (map list
                  (map str
                       enm)
                  (map
                   #(case (int (get-hex-array board %))
                      -2 "="
                      -1 "-"
                      0 "."
                      1 "+"
                      2 "#")
                   (range 37)))]
    (loop [meat transform rk zmap]
      (if (empty? rk)
        (println meat)
        (let [k (ffirst rk)
              v (second (first rk))]
          (recur (clojure.string/replace meat k v)
                 (rest rk)))))))

;;
;;
;; (def-search
;;   (:setup []
;;     (export foo 1)
;;   (:teardown [] 
;;     (analyze foo)
;;     )
;;   (:run [& args]
;;      (* foo foo)
;;         ))
;;
;; ... macro recursively macroexpands its subitems...
;; ... macro throws if you try to bind foo in any way.. (let/blah)
;;
;;  -> (let [foo @foo#]
;;       (fn [blargh] ooog)
;;       )
;;
