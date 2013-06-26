(ns gipf.core
  (:import (gipfj IncrementalGameCalc
                  GameState TranspositionTable
                  SignedGameState CompressedSGS
                  LTranspTable MoveSignedIGC
                  MoveSignedGS HistoryTable)))


;; AI.clj

;;
;; TODO: next order of business 
;;
;; Look for the nearest todo list;
;; What other optimizations exist?
;; 
;; Killer heuristic
;; Transp tables
;; Incremental ranking
;; Incremental board-lines search
;; Quiescent search
;; MTD-f
;; profiling ;-)
;;
;; We will ignore tournament mode for a while, until the ai can beat
;; us..

;; action 

(defn minimax2
  [gamestate player rank-func max? depth]
  (if (equals 0 depth)
    (rank-func gamestate player)
    (let [conts (list-possible-boards gamestate player)]
      ;; NOTE: need to implement this drying up of moves..
      (if (empty? conts)
        ;; loss
        (if max? negative-infinity positive-infinity)
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
        (recur (idr-sub nodetree 0 level endtime rank-func player) (inc-1 level))))) )

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
  `(->Node ~gamestate ~owner 0 {}))

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

(defmacro flag
  [expr & phrases]
  `(do
     (println ~@phrases)
     ~expr))

(defmacro ablm
  "Helper macro for alpha-beta pruning.
  Read the source.
  Negation is the responsiblity of the user.
  Read the source.
  Thank you.
  Read the source."
  [feed bestrank [entry record] & block]
  `(loop [rem# ~feed ~record negative-infinity]
     (if (empty? rem#)
       ~record
       (let [~entry (first rem#)
             q# (do ~@block)]
         (if (greater q# ~bestrank)
           q#
           (recur (rest rem#) (fastmax q# ~record)))))))

(defmacro ablmi
  "Like ablm, just that it takes a java.util.Iterator as its feed"
  [iterator bestrank [entry record] & block]
  `(let [^java.util.Iterator it# ~iterator]
     (loop [~record negative-infinity]
       (if (.hasNext it#)
         (let [~entry (.next it#)
               q# (do ~@block)]
           (if (greater q# ~bestrank)
             q#
             (recur (fastmax q# ~record))))
         ~record))))

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
            (let [rq (lazy-next-gamestates gamestate owner)]
              (if (empty? rq)
                (if (equals gp owner) negative-infinity positive-infinity)
                (ablm rq bestrank [gs _]
                      (let [nr (negate (rankfunc gs gp))]
                        (add-tr! nchildren nr (generate-node gs antiowner))
                        nr))))
            (if (empty? ochildren)
              (:rank node)
              (let [rq (expand-trm ochildren)]
                (ablm rq bestrank [child record]
                      (let [nn (idr-ab-s child rankfunc gp
                                         (inc-1 cdepth) edepth etime
                                         (negate record))
                            nr (negate (:rank nn))]
                        (add-tr! nchildren nr nn)
                        nr)))))]
      ;; children have mutated :-)
      (->Node gamestate owner nrank (persistent! nchildren)))))


;; warning: doesn't idr & alpha-beta prune
;; potentially good nodes? Yes, but the ranking add is worth it

(defn idr-ab-ranking
  "A ranking function. How good is this board condition for gp, who just moved?"
  [gamestate gp rank-func depth max-time]
  (let [endtime (+ (System/nanoTime) (* 1e6 max-time))]
    (loop [nodetree (generate-node gamestate (negate gp)) level 0]
      (if (or (past-time? endtime) (equals level depth))
        (negate (:rank nodetree))
        (recur (idr-ab-s nodetree rank-func gp 0 level endtime positive-infinity)
               (inc-1 level))))))



;; theoretically could make this return a number; if 0, quiet, if
;; positive, boost interest that many turns
(defn simple-quiet
  "True when nothing interesting happened (piece taken)"
  [oldgs newgs]
  (was-taken? (game-state-reserves oldgs) (game-state-reserves newgs)))

;; if depth <= iboost; test quiescence; if true, set to iboost.
;; once depth == 0 and tranquil, heuristic;

;; increment level always; force heuristic when levelcap is reached;

;; another thing to think about; moves are taken infrequently,
;; so one might have a chunked interest boost

(defn quiescent-ab-search
  "Goal: to unify alpha-beta and quiescent search.
  Depth: min depth to search
  Levelcap: max depth to search.
  IBoost: how much further to go until quiet"
  [gamestate good-player rank-func quiet-func depth levelcap iboost]
  (letfn [(qab [gamestate owner depth level best-rank]
            (if (or (equals depth 0) (equals level levelcap))
              (rank-func gamestate good-player)
              (let [subs (IncrementalGameCalc. gamestate owner)]
                (if (.hasNext subs)
                  (ablmi subs best-rank [ngs record]
                         (negate (if (or (greater-equals depth iboost)
                                         (quiet-func ngs gamestate))
                                   (qab ngs (negate owner) (dec-1 depth)
                                        (inc-1 level) (negate record))
                                   (qab ngs (negate owner) iboost
                                        (inc-1 level) (negate record)))))
                  negative-infinity))))]
    (negate (qab gamestate (negate good-player) depth 0 positive-infinity))))


;; OH NO! now the ranking algorithms require setup/teardown..
;; (especially to avoid statically loading this monster)

;; the large table is actually worth it - low collision rates are
;; imperative.
;; size 23 :: 8*10^6 entries, 32 MB. Access is still fast.
(def movetable (make-transp-table 23))

(defn clear-transp!
  []
  (flush-transp-table movetable))

(defn post-mortem-transp
  []
  (analyze-transp-table movetable))

(defn qab-transp
  [gamestate good-player rank-func quiet-func depth levelcap iboost]

  ;; Fundamental downside: movetable would grow indefinately...
  ;; idea: clear it before every complete move...
  ;;
  
                                        ;(println "ent")
  (letfn [(qab [gamestate owner depth level best-rank]
            ;;(when (equals level 1)
            ;;  (println "SIZE:" (size-transp-table movetable)))
            
            (if (or (equals depth 0) (equals level levelcap))
              (rank-func gamestate good-player)
              (let [subs (IncrementalGameCalc. gamestate owner)]
                (if (.hasNext subs)
                  (if (less level 6)
                    (ablmi subs best-rank [ngs record]
                           (let [key (make-signed-gamestate ngs good-player)
                                 lrnk (get-transp-table movetable key)]
                             (if lrnk
                               lrnk
                               (let [rr (negate (if (or (greater-equals depth iboost)
                                                        (quiet-func ngs gamestate))
                                                  (qab ngs (negate owner) (dec-1 depth)
                                                       (inc-1 level) (negate record))
                                                  (qab ngs (negate owner) iboost
                                                       (inc-1 level) (negate record))))]
                                 (add-transp-table movetable key rr)
                                 rr))))
                    (ablmi subs best-rank [ngs record]
                           (negate (if (or (greater-equals depth iboost)
                                           (quiet-func ngs gamestate))
                                     (qab ngs (negate owner) (dec-1 depth)
                                          (inc-1 level) (negate record))
                                     (qab ngs (negate owner) iboost
                                          (inc-1 level) (negate record))))))
                  negative-infinity))))]
    (negate
     (qab gamestate (negate good-player) depth 0 positive-infinity))))


(defn cls-ab-search
  "I never actually did alpha beta with the min-max idea.
   Notably - alpha, beta are not symmetric. Other ab - qab, idr-ab, blah
   Were. Is this costing us??

  Condition: beta < alpha
"
  [gamestate player rank-func depth alpha beta]
  (letfn [(rec [gamestate owner depth alpha beta max?]
            (if (equals depth 0)
                  (rank-func gamestate player)
                  (case-pattern
                   [max? true false]
                   [cur alpha beta
                    opp beta alpha
                    compo greater-equals less-equals
                    mnmx fastmax fastmin
                    lossv negative-infinity positive-infinity]
                   (let [rd (IncrementalGameCalc. gamestate owner)]
                     (if (.hasNext rd)
                       (loop [cur cur]
                         (if (.hasNext rd)
                           (let [ngs (.next rd)
                                 rank (rec ngs (negate owner) (dec-1 depth)
                                           alpha
                                           beta
                                           (not max?))]
                                        ;  (println "max" rank alpha beta)
                             (let [cur (mnmx rank cur)]
                               (if (compo cur opp)
                                 cur
                                 (recur cur))))
                           cur))                      
                       lossv)))))]
    (when (<= beta alpha)
      (println "What's up with the window??"))
    (rec gamestate (negate player) depth alpha beta false)))

(def hist-table (HistoryTable/hmake))

(defn cls-ab-hist-search
  "History heuristic alpha beta search."
  [gamestate player rank-func depth alpha beta]
  (letfn [(rec [gamestate owner level alpha beta max?]
            (if (equals level depth)
                (rank-func gamestate player)
                  (case-pattern
                   [max? true false]
                   [cur alpha beta
                    opp beta alpha
                    compo greater-equals less-equals
                    mnmx fastmax fastmin
                    lossv negative-infinity positive-infinity]
                   (let [rd (MoveSignedIGC. gamestate owner (HistoryTable/hordering hist-table))]
                     (if (.hasNext rd)
                       (loop [cur cur recm -1]
                         (if (.hasNext rd)
                           (let [ngs (.next rd)
                                 rank (rec ngs (negate owner) (inc-1 level)
                                           alpha
                                           beta
                                           (not max?))]
                             (let [cur (mnmx rank cur)]
                               (let [recm (if (equals cur rank)
                                            (MoveSignedGS/getMove ngs)
                                            recm)]
                                 (if (compo cur opp)
                                   (do
                                     (when-not (equals recm -1)
                                       (HistoryTable/hadd hist-table level recm))
                                     cur)
                                 (recur cur recm)))))
                           (do
                             (when-not (equals recm -1)
                               (HistoryTable/hadd hist-table level recm))
                             cur)))                      
                       lossv)))))]
    (when (<= beta alpha)
      (println "What's up with the window??"))
    (rec gamestate (negate player) 0 alpha beta false)))

(defn cls-ab-transp-search
  "So what if I indent five times?
"
  [gamestate player rank-func depth alpha beta]
  (letfn [(rec [gamestate owner level alpha beta max?]
            (if (equals level depth)
              (rank-func gamestate player)
              (case-pattern
               [max? true false]
               [cur alpha beta
                opp beta alpha
                compo greater-equals less-equals
                mnmx fastmax fastmin
                lossv negative-infinity positive-infinity]
               (let [rd (IncrementalGameCalc. gamestate owner)]
                 (if (.hasNext rd)
                   (loop [cur cur]
                     (if (.hasNext rd)
                       (let [ngs (.next rd)
                             rank (if (less-equals level 3)
                                    (let [key (make-signed-gamestate ngs owner)
                                          lrnk (get-transp-table movetable key)]
                                      (if lrnk lrnk
                                          (let [r (rec ngs (negate owner) (inc-1 level)
                                                       alpha
                                                       beta
                                                       (not max?))]
                                            (add-transp-table movetable key r)
                                            r)))
                                    (rec ngs (negate owner) (inc-1 level)
                                         alpha
                                         beta
                                         (not max?)))]

                         (let [cur (mnmx rank cur)]
                           (if (compo cur opp)
                             cur
                             (recur cur))))
                       cur))                      
                   lossv)))))]
    (when (<= beta alpha)
      (println "What's up with the window??"))
    (rec gamestate (negate player) 0 alpha beta false)))

(defn aspiration
  "We can pass in the guess-func rather nicely.
   WARNING: do not use a transp-table using
   guess func until the search-structure is well
   developed."
  [gamestate player rankf radius depth guess-func & guess-args]
  (let [guess (apply guess-func gamestate player guess-args)]
    (let [alpha (subtract guess radius)
          beta (add guess radius)
          res (cls-ab-transp-search gamestate player rankf depth alpha beta)]
      (cond (equals res alpha)
            (do
              ;; (println "Failed low")
              (cls-ab-transp-search gamestate player rankf depth negative-infinity beta))
            (equals res beta)
            (do
              ;; (println "Failed high")
              (cls-ab-transp-search gamestate player rankf depth alpha positive-infinity))
            :else
            res))))

(defn mtd-f
  "MTD-F works best with quantized heuristics."
  [gamestate player rankf depth guess-func & guess-args]
  (let [guess (apply guess-func gamestate player guess-args)]
    (loop [guess guess upper positive-infinity lower negative-infinity]
      (let [beta 
            (if (equals guess lower)
              (inc-1 guess)
              guess)]
        (let [next (cls-ab-transp-search gamestate player rankf depth (dec-1 beta) beta)]
          (if (less next beta)
            (let [upper next]
              (if (less-equals upper lower)
                upper
                (recur next upper lower)))
            (let [lower next]
              (if (less-equals upper lower)
                lower
                (recur next upper lower)))))))))


;;
;; Still more ideas: the back/forth between moves, as demonstrated
;; by the heuristic, can throw off iterative deepening, as it prunes
;; too early. One can convert this into two-depth idr, which could
;; be smoother...
;;
;;
;;

;;
;; killer heuristic - needs cross tree communication - use let-bound
;; atom? or pass it around... (thread-safer)
;;
