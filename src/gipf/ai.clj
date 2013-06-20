(ns gipf.core)

;; AI.clj

;;
;; TODO: next order of business 
;;
;; Get an acceptable heuristic that kinda likes winning. Also,
;; fix any bugs in the current idr search. It _should_ be minimax eqv.
;;
;; how does alpha-beta search work??
;;
;;
;;
;;
;;
;;

;; action 

(defn minimax2
  [gamestate player rank-func max? depth]
  (if (equals 0 depth)
    (rank-func gamestate player)
    (let [conts (list-possible-boards gamestate player)]
      ;; NOTE: need to implement this drying up of moves..
      (if (empty? conts)
        ;; loss
        (if max? neg-ranking-infinity ranking-infinity)
        ;; continue
        (reduce (if max? #(fastmax %1 %2) #(fastmin %1 %2))
                (map
                 (fn [new-b-and-r]
                   (minimax2 new-b-and-r
                             player
                             rank-func
                             (not max?)
                             (dec-1 depth)))
                 conts))))))

;; REMEMBER: the ranking algorithms work one step in;
;; They simulate the opponent's move ... but that means
;; list-possible-boards is wonky...

(defn negamax
  [gamestate player rank-func depth]
  (if (equals 0 depth)
    (rank-func gamestate player)
    (let [conts (list-possible-boards gamestate player)]
      (if (empty? conts)
        -10000000000000
        (reduce #(fastmin %1 %2)
                (map
                 (fn [gs]
                   (negate (negamax gs
                                    player
                                    rank-func
                                    (dec-1 depth))))
                 conts))))))

;; take a node. replace (loop) it with the rank of its children, and
;; add pointers. Loop over all nodes.
;;  .  .  .  .
;;     .  .  .
;;        .  .
;;           .

(defmacro idr-subsub
  [ede childlist]
  `(if (fast-odd? ~ede)
     (reduce #(fastmax %1 %2) (map #(idr-node-rank %) ~childlist))
     (reduce #(fastmin %1 %2) (map #(idr-node-rank %) ~childlist))))

(defn idr-sub
  [node depth ede endtime rank-func good-player]

  ;; depth 0 aims to minimize; depth 1 max, depth 2 min...
  
  (cond
   ;; the greater the depth is for the timechecks, the more accurate
   ;; things become. 1 is (calc'ed) off by max 1/42th of the time delta
   ;; 0 is horrible, and 2 is unmeasureable (but loses efficiency)
   (and (less depth 1) (past-time? endtime)) node

   ;; expand a node
   (equals depth ede)
   (let [player (idr-node-player node)
         childlist
         (map
          (fn [gamestate]
            (make-idr-node
             gamestate
             (negate player)
             (rank-func gamestate good-player)))
          (list-possible-boards (idr-node-gamestate node) player))
         rank (idr-subsub ede childlist)]
     (idr-node-update node rank childlist))

   ;; update a node
   :else
   (let [childlist
         (map #(idr-sub % (inc-1 depth) ede
                        endtime rank-func good-player)
              (idr-node-children node))
         rank (idr-subsub ede childlist)]
     (idr-node-update node rank childlist))))

;; I want a macro that inlines x times, then iterates in a function..
;; (optimizer)

(defn iterative-deepening-ranking
  "Ranks a position. It deepens. Iteratively."
  [gamestate player rank-func depth time]

  (let [starttime (. System (nanoTime))
        endtime (+ (* time 1e6) starttime)]
    ;; the initial node is owned by the opponent; it moves from that node
    (loop [nodetree (make-idr-node gamestate
                                   (negate player)
                                   (rank-func gamestate player))
           level 0]
      (if (or (past-time? endtime) (equals level depth))
        (do
          (idr-node-rank nodetree))
        (recur (idr-sub nodetree 0 level endtime rank-func player) (inc-1 level))))))

;;
;; Negamax search:
;; 
;; at each node: maximize the negation of the value of the subnodes
;;
;;
;;
;;
;;
;;

;;; negamax: rank := (fastmax (map #(negate %) ranks)

;;;          rank := (negate (fastmin)

(def negative-infinity -1000000000)

(defn abps
  [gamestate owner ranker gp depth bestrank]
  (if (equals depth 0)
    (ranker gamestate gp)

    ;; otherwise, go through all direct subnodes.
    ;; if one of its subs has a worse than bestrank, ignore them.
    ;; once an node has a better negrank, take it
    (let [subnodes
          (list-possible-boards gamestate owner)
          ;; question: is it worth it to sort the boards in terms
          ;; of their ranks? - no, by an order of magnitude?? by
          ;; sort-by, which is not incremental..
          ]
      ;; todo: empty check for terminal (losing) node
      
      (loop [rem subnodes record negative-infinity]
        (if (empty? rem) record
            (let [nr (negate (abps (first rem)
                                   (negate owner)
                                   ranker gp
                                   (dec-1 depth)
                                   (negate record)))]
              ;; (println (apply str (map (constantly ". ") (range (subtract 4 depth))))
              ;;          nr bestrank)
              (if (greater nr bestrank)
                nr
                (recur (rest rem) (fastmax nr record)))))))))

(defn abprune
  [gamestate gp rank-func depth]
  ;; core is negamax, using 2-deep cutting
  (abps gamestate (negate gp) rank-func gp depth -10000000))

(defrecord Node [gamestate owner rank children])
(defmacro generate-node
  [gamestate owner]
  `(->Node ~gamestate ~owner 0 (transient {})))

;; need let over lambda's g!, o! symbols
(defmacro add-tr!
  [thing index val]
  `(let [th# ~thing
         existing# (get th# ~index)]
    (if existing#
      (assoc! th# ~index
              (cons ~val existing#))
      (assoc! th# ~index (list ~val)))))

(defmacro expand-trm
  [thing]
  `(let [t# ~thing]
     (expand #(get t# %) (sort (keys t#)))))

;; rankfunc, gp remain constant the entire time....
(defn idr-ab-s
  [node rankfunc gp cdepth edepth etime bestrank]
  (if
      (and (less cdepth 1) (past-time? etime))
    node
    (let [gamestate (:gamestate node)
          owner (:owner node)
          antiowner (negate owner)
          ochildren (:children node)
          nchildren (transient {})
          nrank
          (if (equals cdepth edepth)
            (loop [rem (list-possible-boards gamestate owner)
                   record negative-infinity]
              (if (empty? rem)
                record
                (let [gs (first rem)
                      nr (negate (rankfunc gs gp))]
                  (add-tr! nchildren nr (generate-node gs antiowner))
                  (if (greater nr bestrank)
                    record
                    (recur
                     (rest rem)
                     (fastmax nr record))))))
            (loop [rem (expand-trm ochildren)
                   record negative-infinity]
              (if (empty? rem)
                record
                (let [child (first rem)
                      nn (idr-ab-s child rankfunc gp
                                   (inc-1 cdepth) edepth etime
                                   (negate record))
                      nr (negate (:rank nn))]
                  (add-tr! nchildren nr nn)
                  (if (greater nr bestrank)
                    record
                    (recur
                     (rest rem)
                     (fastmax nr record)))))))]
      ;; children have mutated :-)
      (->Node gamestate owner nrank (persistent! nchildren)))))


;; warning: doesn't idr & alpha-beta prune
;; potentially good nodes? Yes, but the ranking add is worth it

(defn idr-ab-ranking
  "A ranking function"
  [gamestate gp rank-func depth max-time]

  (let [endtime (+ (System/nanoTime) (* 1e6 max-time))]
    (loop [nodetree (generate-node gamestate (negate gp)) level (long 0)]
      (if (or (past-time? endtime) (equals level depth))
        (:rank nodetree)
        (recur (idr-ab-s nodetree rank-func gp 0 level endtime -10000000)
               (inc-1 level))))))
