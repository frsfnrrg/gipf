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

(defn on-line?
  [loc line]
  (let [delta (pt- loc (second line))
        dist (pt-radius delta)
        approx (pt* dist (third line))]
    (or (pt= delta approx)
      (pt= delta (pt- approx)))))

(defn line=
  [linea lineb]
  (and (on-line? (second linea) lineb)
       (or (pt= (third linea) (third lineb))
           (pt= (pt- (third linea)) (third lineb)))))

; line=??

(defn get-line-limit-point
  [pos vecback]
  (loop [cur pos]
    (let [next (pt+ cur vecback)]
      (if (= (pt-radius next) 4)
          cur
          (recur next)))))

;; a least you can't have two four-in-a rows
(defn get-four-line-in-row
  "Returns, if a 4line found, a (player position delta) representing that line"
  [board startpos lvec]
  (let [sp (get-hex-array board startpos)]
    (loop [cur startpos run -1 player 0]
      (cond
        (= run 4)
        (list player startpos lvec)
        (= (pt-radius cur) 4)
        nil
        :else
        (let [np (get-hex-array board cur)]
          (if (and (not= player 0) (same-sign? np player))
            (recur (pt+ lvec cur) (inc run) player)
            (recur (pt+ lvec cur) 1 np)))))))

(defn four-line-in-rows
  [avec rvec]
  (let [start (pt* -3 avec)]
    (loop [h 0 found (list)]
      (if (= h 7)
        found
        (recur (inc h)
               (cons (list 0
                           (get-line-limit-point
                            (pt+ start (pt* h avec))
                            (pt- rvec))
                           rvec)
                     found))))))

(def lines-on-board
  (loop [dir (pt 1 0 0) found (list)]
    (if (pt= dir (pt -1 0 0))
      found
      (let [new (four-line-in-rows dir (pt- (pt-rot+60 dir)))]
        (recur (pt-rot+60 dir) (concat new found))))))

(defn get-lines-of-four
  "Returns a list of lists of form (player a b c d)"
  ;; TODO: this currently costs 0.9 msec per call. I want half that
  [board]
  (filter seq
          (map
           (fn [line]
             (get-four-line-in-row board
                                   (second line)
                                   (third line)))
           lines-on-board)))

(defn get-open-moves
  "Returns a list of the available moves..."
  [board]
  ;; does it work
  (filter #(not (row-full? board (pt+ (first %) (second %)) (second %)))  
    (reduce concat
      ;; loc -> list of 2 lines
      (map #(let [inside-neighbors (filter (fn [p] (= (pt-radius p) 3)) (map (fn [q] (pt+ q %)) (get-ring-of-hex-uv-points 1)))]
              ;; pt, neighbor ->  
              (map (fn [ne] (list % (pt- ne %)))
                inside-neighbors))
        (get-ring-of-hex-uv-points 4)))))

(defn place-point-open?
  "Can a point be placed here?"
  [board loc]
  (and (= 4 (pt-radius loc))
    (if (some #(pt= % loc)
          (map #(pt* 4 %) (get-ring-of-hex-uv-points 1)))
      (let [dekl (pt* (- (/ 4)) loc)]
        (not (row-full? board (pt+ loc dekl) dekl)))
      ; the second condition is a superset of the first. Why test?
      (let [pts (filter #(= (pt-radius %) 3) (map #(pt+ % loc) (get-ring-of-hex-uv-points 1)))]
        (some #(not (row-full? board % (pt- % loc))) pts)))))

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
  (loop [cur (second line) count 0]
        (if (= 4 (pt-radius cur))
          count
          (recur (pt+ (third line) cur)
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


(def rank-board
  "Returns a number stating how favorable
   a board state is to a given player."
  rank-board-3)

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
    [(first (do-move board player (pt+ (first move) (second move)) (second move)))
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
                       nil -100000 possible-moves))]
    (conj (into [] (or optimal (rand-nth (get-open-moves board))))
          degree)))


(defn get-gipf-potentials-in-line
  [board line]
;;  (println 'get-gipf-potentials line)
  (loop [cur (second line) fps (list)]
    (if (= 4 (pt-radius cur))
      fps
      (if (= (abs (get-hex-array board cur)) 2)
        (recur (pt+ cur (third line)) (cons cur fps))
        (recur (pt+ cur (third line)) fps)))))

(defn get-own-gipf-potentials-in-line
  [board player line]
  (filter #(same-sign? (get-hex-array board %1) player)
          (get-gipf-potentials-in-line board line)))

(defn ai-clear
  "Returns (list line keep)"
  [board player lines]
  (busy-doing-important-stuff order-distinguishing-pause)
  (let [line (rand-nth (filter #(same-sign? (first %) player) lines))
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
