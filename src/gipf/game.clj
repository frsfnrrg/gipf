(ns gipf.core)

(def order-distinguishing-pause 0.6) ; sec 

;; predicates/extraction

(defn row-full?
  [board start delta]
  (defn f
    [curr]
    (if (= 0 (get-hex-array board curr))
      false
      (let [next (pt+ curr delta)]
        (if (= 4 (pt-radius next))
          true
          (recur next)))))
  (f start))

(defn get-four-line-in-row
  ;; TODO: why are there so many calls to pt-radius? - 5 per call
  [board startpos lvec]
  (loop [cur startpos run -1 player 0]
    (cond
     (= run 4)
     (->SignedLine player startpos lvec)
     (= (pt-radius cur) 4)
     false
     :else
     (let [np (get-hex-array board cur)]
       (if (and (not= player 0) (same-sign? np player))
         (recur (pt+ lvec cur) (inc run) player)
         (recur (pt+ lvec cur) 1 np))))))

(defn four-line-in-rows
  [avec rvec]
  (let [start (pt* -3 avec)]
    (loop [h 0 cur start found (list)]
      (if (= h 7)
        found
        (recur (inc h)
               (pt+ cur avec)
               (cons (->Line (get-line-limit-point
                              cur
                              (pt- rvec))
                             rvec)
                     found))))))

(def lines-on-board
  (loop [dir (pt 1 0 0) found (list)]
    (if (pt= dir (pt -1 0 0))
      found
      ;; why pt-
      (let [new (four-line-in-rows dir (pt- (pt-rot+60 dir)))]
        (recur (pt-rot+60 dir) (concat new found))))))

(defn get-lines-of-four
  "Returns a list of lists of form (player a b c d)"
  ;; TODO: this currently costs 0.90 msec per call. I want half that
  [board]
  (filter identity
          (map
           (fn [line]
             (get-four-line-in-row board
                                   (:start line)
                                   (:delta line)))
           lines-on-board)))

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
  (filter #(not (row-full? board (pt+ (:start %) (:delta %)) (:delta %)))  
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

(defn value-cell
  "What is the \"value\" of a cell?
   ie, how useful is the ownership of
   that cell to the good-player?"
  [board pos good-player]
  ;; TODO improve analysis. Bunching?

  ;; negative numbers correspond to the opponent.
  (* (- 3 (pt-radius pos))
     (case (* (get-hex-array board pos) good-player)
       -2  -50 ; other gipf is baaad.
       -1  -10
       0   0
       1   15
       2   40 ;; my gipf is goood
       )))

;; issue; taking 3 opp is better than 3 self
(defn value-pieces
  "Returns the \"value\" of pieces on a line."
  [board line good-player]
  (loop [cur (:start line) count 0]
        (if (= 4 (pt-radius cur))
          count
          (recur (pt+ (:delta line) cur)
                 (+ count (value-cell board cur good-player))))))

(defn rank-board-1
  "225-353"
  [board player reserves]
  (let [pos-points (summap (fn [pt] (value-cell board pt player))
                           arrayfull-of-points)
        lines-points (summap (fn [li] (value-pieces board li (- player)))
                             (get-lines-of-four board))] 
    (+ pos-points (* 20 lines-points))))

(defn rank-board-2
  "77 - 143 ms"
  [board player reserves]
  (let [pos-points (summap (fn [pt] (value-cell board pt player))
                           arrayfull-of-points)]
    pos-points))


(defn rank-board-3
  "162 - 183 ms"
  [board player reserves]
  (let [ lines-points (summap (fn [li] (value-pieces board li (- player)))
                             (get-lines-of-four board))] 
    (* 20 lines-points)))

(defn rank-board-4
  [board player reserves]
  10)


(def rank-board
  "Returns a number stating how favorable
   a board state is to a given player."
  rank-board-1)

;; action 

(defn change-board-cell
  [board pos val]
  ;; "Changing " pos "from" (get-hex-array board pos) "to" val)
  (map-hex-array (fn [p v] (if (pt= p pos) val v)) board))

;; this is _purely_ abstract...
(defn do-move
  "Takes the board, move, gamemode, returns the changed board and list of changed squares."
  [board value loc shove]
  (loop [b board cur loc last value shift (list)]
    (let [next (get-hex-array b cur)]
      (cond
       (= (pt-radius cur) 4)
       (do
         (println "pushed until radius was 4. ??")
         :should-never-happen)
       (= 0 next)
       (list (change-board-cell b cur last) (cons cur shift))
       :else
       (recur (change-board-cell b cur last)
              (pt+ cur shove)
              next
              (cons cur shift))))))


(defn act-move
  [gamestate move player]
  (let [[board reserves] gamestate]
    ; remember; player == piece
    [(first (do-move board player (pt+ (:start move) (:delta move)) (:delta move)))
     (atv reserves (player->index player) dec)]))

(defn minimax
  "Ranks a position resulting from a move based
  one what the player would respond and the response
  to that etc."
  [gamestate player max? depth]      
  (let [[board reserves] gamestate]
    (if (zero? depth)
      (* (if max? 1 -1)
         (rank-board board player reserves))
      (reduce (if max? max min)
              (map
               (fn [move]
                 (minimax (act-move gamestate move player)
                          (- player)
                          (not max?)
                          (dec depth)))
               (get-open-moves board))))))

(defn ai-move
  "Returns place & shove vector."
  [board player reserves adv]
  (busy-doing-important-stuff order-distinguishing-pause)
  (let [possible-moves (get-open-moves board)
        ngipfs (count-over-hex-array
                #(= %2 (* player 2))
                board)        
        degree (if (and (= adv :filling) (< ngipfs 4)) 2 1)
        optimal (time (rand-best
                       (fn [move]
                         (time (minimax (act-move [board reserves] move player)
                                        player
                                        true
                                        0)))
                       nil -100000 possible-moves))
        chosen (or optimal (rand-nth possible-moves))]
    [(:start chosen) (:delta chosen) degree]))


(defn get-gipf-potentials-in-line
  [board line]
  (loop [cur (:start line) fps (list)]
    (if (= 4 (pt-radius cur))
      fps
      (if (= (abs (get-hex-array board cur)) 2)
        (recur (pt+ cur (:delta line)) (cons cur fps))
        (recur (pt+ cur (:delta line)) fps)))))

(defn get-own-gipf-potentials-in-line
  [board player line]
  (filter #(same-sign? (get-hex-array board %1) player)
          (get-gipf-potentials-in-line board line)))

(defn ai-clear
  "Returns (list line keep)"
  [board player lines]
  (busy-doing-important-stuff order-distinguishing-pause)
  (let [line (rand-nth (filter #(same-sign? (:sig %) player) lines))
        gipf-potentials (get-own-gipf-potentials-in-line board player line)]
    (list
     line
     gipf-potentials)))

;; new!

(defn new-board
  "Return a newly set up board."
  [mode]
  (if (= mode :advanced)
    (make-hex-array)
    (let [m (if (= mode :basic) 1 2)]
      (applyto-repeatedly
       change-board-cell
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
    :basic (vector 12 12)
    :advanced (vector 18 18)
    :normal (vector 15 15)))


(defn lost?
  [board reserves player mode advm]
  (if (or (= advm :filling) (= mode :basic))
    (= 0 (get reserves (if (= player -1) 0 1)))
    (or
     (= 0 (get reserves (if (= player -1) 0 1)))
     (= 0 (count-over-hex-array
           #(= %2 (* player 2))
           board)))))
