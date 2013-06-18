(ns gipf.core
  (:import (gipfj Board GameState)))

;;;
;;; There is a really nice way of expressing moves.
;;; One move consists of three phases:
;;;
;;;  Pre clean
;;;  Action
;;;  Post clean
;;;
;;; All are done by the same player; players alternate.
;;; Preclean is eqv. to the cleaning response to the other
;;; player's move.
;;;
;;; Under this framework, the aimove as a whole can
;;; be done in one action; [:aimove [[save save] clean clean]
;;;                                 shoveline
;;;                                 [[save save] clean clean]]
;;;
;;; Then, given two functions: list-possible-boards
;;;                            list-possible-moves-and-board
;;;
;;; Given a board and a player, one can get the list of the results,
;;; potentially with the move that caused it.
;;;
;;;


(def order-distinguishing-pause 0.6) ; sec
(def ranking-infinity (long 100000))
(def neg-ranking-infinity (long -100000))

;; predicates/extraction

(defrename row-full? `Board/lineFull 3)

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

(defrename value-cell `Board/valueCell 3)
(defrename rank-board-org `Board/rankBoardOrg 2)
(defrename rank-line `Board/rankLine 3)
(defrename rank-board-lines `Board/rankBoardLines 2)

(defn rank-board-1
  "Finally kinda efficient"
  ^long [board ^long player ^Reserves reserves]
  (let [pos-points (rank-board-org board player)
        lines-points (rank-board-lines board player)] 
    (add pos-points (multiply 20 lines-points))))

(defn rank-board-2
  "Finally kinda efficient"
  ^long [board ^long player ^Reserves reserves]
  (rank-board-org board player))

(defn rank-board-3
  "Finally kinda efficient"
  ^long [board ^long player ^Reserves reserves]
  (rank-board-lines board player))

(defn rank-board-4
  ^long [board ^long player reserves]
  10)

(def rank-board
  "Returns a number stating how favorable
   a board state is to a given player."
  rank-board-1)

;; action 

;; this is _purely_ abstract...
;; TODO split into two functions: one for ai, one for getting
;; the updated pieces..

;; TODO again: put into java for optimization (if needed)

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
                   (case (int (unchecked-multiply (get-hex-array bb cur) player))
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
                     (recur (pt+ cur delta)
                            (conj bpr cur)
                            bb brr))))]
           
           (recur nb nr (conj taken chosen) (conj protected prot))))))))

(defrename place-and-shove `Board/placeAndShove 3) 
(defrename ->GameState `GameState/makeGameState 2)

(defn do-shove
  [board reserves player shove]
  [(first (do-move board player (line-start shove) (line-delta shove)))
   (dec-reserves reserves player)])

(defn expand-gamestate
  [gs]
  [(GameState/getBoard gs) (GameState/getReserves gs)])

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
  [board reserves player]
  (map (fn [_] [board reserves]) (range 42)))

(defn list-possible-boards-opt
  "Costs 60% more in ai than the cheat version"
  [board reserves player]
  (map expand-gamestate
    (vec (Board/listPossibleBoards
           board reserves player))))
  
(def list-possible-boards list-possible-boards-opt)
  
(defn minimax2
  [board-and-reserves player max? depth]
  (let [[board reserves] board-and-reserves]
    (if (zero? depth)
      (if max?
        (rank-board board player reserves)
        (negate (rank-board board player reserves)))
      (let [conts (list-possible-boards board reserves player)]
        (if (empty? conts)
          ;; loss
          (if max? neg-ranking-infinity ranking-infinity)
          ;; continue
          (reduce (if max? max min)
                  (map
                   (fn [new-b-and-r]
                     (minimax2 new-b-and-r
                              (negate player)
                              (not max?)
                              (dec-1 depth)))
                   conts)))))))

(defn compound-ai-move
  [board ^long player ^Reserves reserves adv-phase]

  ;; TODO: this takes 60% more time than the simple ai move...
  ;; we need to go deeper (profiling)

  (busy-doing-important-stuff 0.5)
  
  ;; we assume the opening strategy ignores the gipfiness when in
  ;; :filling mode

  (let [pieces-left (get-reserves reserves player)
        possible-moves (list-possible-moves-and-board board reserves player)
        ngipfs (count-over-hex-array board player)
        degree (if (and (= adv-phase :filling) (< ngipfs 4)) 2 1)
        optimal (time (rand-best
                       (fn [[move board-and-res]]
                         (time (minimax2 board-and-res
                                        player
                                        true
                                        3)))
                       nil -100000 possible-moves))
        [c1 m c2] (first (or optimal (rand-nth possible-moves)))]
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
    :basic (->Reserves 12)
    :advanced (->Reserves 18)
    :normal (->Reserves 15)))

(defn lost?
  [board reserves player mode advm]
  (if (or (= advm :filling) (= mode :basic))
    (= 0 (get reserves (if (= player -1) 0 1)))
    (or
     (= 0 (get reserves (if (= player -1) 0 1)))
     (= 0 (count-over-hex-array
           board
           (* player 2)
           )))))
