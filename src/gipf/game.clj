(ns gipf.core
  (:import (gipfj Board GameState Reserves IDRNode)))

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


(def order-distinguishing-pause 0.6) ; sec
(def ranking-infinity (long 100000))
(def neg-ranking-infinity (long -100000))

(defrename place-and-shove `Board/placeAndShove 3) 
(defrename ->GameState `GameState/makeGameState 2)
(defrename game-state-board `GameState/getBoard 1)
(defrename game-state-reserves `GameState/getReserves 1)

(defrename row-full? `Board/lineFull 3)

(defrename value-cell `Board/valueCell 3)
(defrename rank-board-org `Board/rankBoardOrg 2)
(defrename rank-line `Board/rankLine 3)
(defrename rank-board-lines `Board/rankBoardLines 2)

(defrename make-idr-node `IDRNode/makeIDRNode 3)
(defrename idr-node-update `IDRNode/updateIDRNode 3)
(defrename idr-node-gamestate `IDRNode/getGameState 1)
(defrename idr-node-player `IDRNode/getPlayer 1)
(defrename idr-node-rank `IDRNode/getRank 1)
(defrename idr-node-children `IDRNode/getChildren 1)

(defrename losing-reserve? `Reserves/losingReserve 2)

;; predicates/extraction

(defn get-lines-of-four
  [board]
  (vec (Board/getBoardLines board)))

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

(defn rank-board-1
  ;; why measure lines after the fact?
  "Finally kinda efficient"
  [gamestate player]
  (let [pos-points (rank-board-org gamestate player)
        lines-points (rank-board-lines gamestate player)] 
    (add (divide pos-points 10) (multiply 20 lines-points))))

(defn rank-board-2
  "Finally kinda efficient"
  [gamestate player]
  ;; hmm. could roll in # of gipfs in play,
  ;; number of gipfs taken, etc into the Reserves
  
  (let [reserves (game-state-reserves gamestate)
        antiplayer (negate player)]
    (cond (losing-reserve? reserves player)
          -100000
          (losing-reserve? reserves antiplayer)
          100000
          :else
          (let [
                gipf-points (subtract
                              (get-gipfs reserves player)
                              (get-gipfs reserves antiplayer))
                piece-points (subtract (get-reserves reserves player)
                       (get-reserves reserves antiplayer))]
            (add
             (multiply 20000 gipf-points)
             (multiply 5000 piece-points))))))

;; action 

(defn do-move
  "Takes the board, move, gamemode, returns the changed board and list of changed squares."
  [board value loc ^long shove]
  (loop [b board ^long cur loc ^long last value shift (list)]
    (let [next (get-hex-array b cur)]
      (cond
       (equals (pt-radius cur) long-4)
       (do
         (println "pushed until radius was 4. ??")
         :should-never-happen)
       (equals long-zero next)
       (list (change-hex-array b cur last) (cons cur shift))
       :else
       (recur (change-hex-array b cur last)
              (pt+ cur shove)
              next
              (cons cur shift))))))

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
                       (println cur (get-hex-array bb cur) player)
                       (recur (pt+ cur delta)
                         (conj bpr cur)
                         bb brr)))))]
           
           (recur nb nr (conj taken chosen) (conj protected prot))))))))

(defn do-shove
  [board reserves player shove]
  [(first (do-move board player (line-start shove) (line-delta shove)))
   (dec-reserves reserves player)])

(defn expand-gamestate
  [gs]
  [(game-state-board gs) (game-state-reserves gs)])

(defn get-line-taking-results
  [gamestate player]
  (vec (Board/getLineTakingResults gamestate player)))

(defn fget-open-moves
  [board]
  (vec (Board/getOpenMoves board)))

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

(defn list-possible-boards-opt
  "Costs 60% more in ai than the cheat version"
  [gamestate player]
  (vec (Board/listPossibleBoards gamestate player)))
  
(def list-possible-boards list-possible-boards-opt)

(defn minimax2
  [gamestate player max? depth rank-func]
  (if (equals 0 depth)
    (if max?
      (rank-func gamestate player)
      (negate (rank-board gamestate player)))
    (let [conts (list-possible-boards gamestate player)]
      (if (empty? conts)
        ;; loss
        (if max? neg-ranking-infinity ranking-infinity)
        ;; continue
        (reduce (if max? #(fastmax %1 %2) #(fastmin %1 %2))
                (map
                 (fn [new-b-and-r]
                   (minimax2 new-b-and-r
                             (negate player)
                             (not max?)
                             (dec-1 depth)))
                 conts))))))

(defmacro past-time?
  [time]
  `(>  (. System (nanoTime)) ~time))

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
                           (if (fast-odd? ede)
                             (rank-func gamestate newp)
                             (negate (rank-func gamestate newp)))))
                        subgs)
         rank (if (fast-odd? ede)
                (reduce #(fastmax %1 %2) (map #(idr-node-rank %) childlist))
                (reduce #(fastmin %1 %2) (map #(idr-node-rank %) childlist)))]
     (idr-node-update node rank childlist))
   :else
   (let [childlist (map #(idr-sub % (inc-1 depth) ede endtime rank-func)
                        (idr-node-children node))
         rank (if (fast-odd? ede)
                (reduce #(fastmax %1 %2) (map #(idr-node-rank %) childlist))
                (reduce #(fastmin %1 %2) (map #(idr-node-rank %) childlist)))]
     (idr-node-update node rank childlist))))

;; I want a macro that inlines x times, then iterates in a function..
;; (optimizer)

(defn iterative-deepening-ranking
  "Ranks a position. It deepens. Iteratively."
  [gamestate player depth time rank-func]

  (let [starttime (. System (nanoTime))
        endtime (+ (* time 1e6) starttime)]
    (loop [nodetree (make-idr-node gamestate
                            player
                            (rank-board gamestate player))
           level 0]
      (if (or (past-time? endtime) (equals level depth))
        (do
          (idr-node-rank nodetree))
        (recur (idr-sub nodetree 0 level endtime rank-func) (inc-1 level))))))

(defn idr-fast-r2
  [gamestate player]
  (iterative-deepening-ranking gamestate player 2 50 rank-board-2))

(defn idr-fast-r1
  [gamestate player]
  (iterative-deepening-ranking gamestate player 2 50 rank-board-1))

;; should make this easily changeable... (per menu?; with registering stuf)

(def  move-ranking-func "Takes a gamestate and a player. Woo" idr-fast-r2)


(def timing** false)

(defmacro
  timec
  [expr]
  (if timing**
    `(time ~expr)
    expr))

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
                               (timec (move-ranking-func
                                      (->GameState board res)
                                      player))]
                           (on-swing-thread
                            (direct-visualize-ai-ranking (second move) rank))
                           rank))
                       nil -100000 possible-moves))
        [c1 m c2] (first (or optimal (rand-nth possible-moves)))]
    ;; best would be, until mouse is moved...
    (busy-doing-important-stuff 1.0)
;
;;  (on-swing-thread)  How do we repaint all?
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
    (= 0 (get-reserves reserves player)))
    (or
     (= 0 (get-reserves reserves player)))
     (= 0 (get-gipfs reserves player)))
