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
  [board pos-in-line lvec]
  (let [start (get-line-limit-point pos-in-line (pt- lvec))
        sp (get-hex-array board start)]
    (loop [cur start run -1 player nil]
      (cond
        (= run 4)
        (list player start lvec)
        (= (pt-radius cur) 4)
        nil
        :else
        (let [np (get-hex-array board cur)]
          (if (and (not= player 0) (= np player))
            (recur (pt+ lvec cur) (inc run) player)
            (recur (pt+ lvec cur) 1 np)))))))

(defn four-line-in-rows
  [board avec rvec]
  (let [start (pt* -3 avec)]
    (loop [h 0 found (list)]
      (if (= h 7)
        found
        (recur (inc h)
          (let [new (get-four-line-in-row board
                      (pt+ start (pt* h avec))
                      rvec)]
            (if (nil? new) found (cons new found))))))))

(defn get-lines-of-four
  "Returns a list of lists of form (player a b c d)"
  [board]
  (loop [dir (pt 1 0 0) found (list)]
    (if (pt= dir (pt -1 0 0))
        found
        (let [new ;(concat 
                    (four-line-in-rows board dir (pt- (pt-rot+60 dir)))]
                   ; (four-line-in-rows board dir (pt- (pt-rot-60 dir))))]
          (recur (pt-rot+60 dir) (concat new found))))))

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

;; issue; taking 3 opp is better than 3 self
(defn value-pieces
  "Returns the \"value\" of pieces on a line."
  [board line player]
  (println line)
  (log (loop [cur (second line) count 0]
    (if (= 4 (pt-radius cur))
        count
        (recur (pt+ (third line) cur) (+ count (case (get-hex-array board cur)
                                                 0 0
                                                 1 (if (= player 1) 1 3)
                                                 -1 (if (= player -1) 1 3))))))))

(defn rank-board
  "Returns a number stating how favorable a board state is to a given player."
  [board player reserves]
  ;; additionally, one can add points based on how many pieces are how close to 
  ;; the center: center is good...

  (let [pos-points (reduce +
                     (map
                       (fn [pt] (let [q (pt-radius pt)
                                      v (get-hex-array board pt)]
                                  (cond
                                    (= 0 v) 0
                                    (= player v) q
                                    :else (- q))))
                       (map n->pt (range (hexagonal-number 5)))))
        lines-points (reduce (fn [sup line]
                               (let [r (if (= (first line) player) 1 -1)]
                                 (+ sup (* r (value-pieces board line (first line))))))
                       0 (get-lines-of-four board))]
    ;; pos-points ; -->|6|; lines-points; 8-14/line
    (+ pos-points (* 2 lines-points))))

;; action 

(defn change-board-cell
  [board pos val]
  ;; "Changing " pos "from" (get-hex-array board pos) "to" val)
  (map-hex-array (fn [p v] (if (pt= p pos) val v)) board))

;; this is _purely_ abstract...
(defn do-move
  "Takes the board, move, gamemode, returns the changed board and list of changed squares."
  [board player loc shove]
  (let [del (pt- loc shove)
        pboard (change-board-cell board del 0)
        [slidboard shifteds]
        (loop [b pboard cur loc last player shift (list)]
          (let [next (get-hex-array b cur)]
            (cond
              (= (pt-radius cur) 4)
              (do
                (println "pushed until radius was 4. ??")
                :should-never-happen)
              (= 0 next)
              (list (change-board-cell b cur last) (cons cur shift))
              :else
              (recur (change-board-cell b cur last) (pt+ cur shove) next (cons cur shift)))))]
    
    (list slidboard (apply list loc del shifteds))))

(defn ai-move
  "Returns place & shove vector."
  [board player reserves]
  (busy-doing-important-stuff order-distinguishing-pause)
  
  (let [possible-moves (get-open-moves board)
        optimal (rand-best
                  (fn [move]
                    (rank-board
                      (first (do-move board player
                               (pt+ (first move) (second move)) 
                               (second move)))
                      player
                      reserves))
                  nil -100000 possible-moves)]
    (or optimal (rand-nth (get-open-moves board)))))

(defn ai-clear
  "Returns a set of lines to be cleared. lines must have content."
  [board player lines]
  (busy-doing-important-stuff order-distinguishing-pause)
  (first (filter #(= (first %) player) lines)))

;; new!

(defn new-board
  "Return a newly set up board."
  []
  (applyto-repeatedly
    change-board-cell
    (make-hex-array (constantly 0) 5)
    [(pt 3 0 0) 1]
    [(pt 0 3 0) -1]
    [(pt 0 0 3) 1]
    [(pt -3 0 0) -1]
    [(pt 0 -3 0) 1]
    [(pt 0 0 -3) -1]))


(defn new-reserves
  "Return a new set of reserves."
  []
  (vector 15 15))
