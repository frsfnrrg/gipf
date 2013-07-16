(ns gipf.core)

(def neginf negative-infinity)
(def posinf positive-infinity)

;; AI.clj

;;
;; TODO: next order of business 
;;
;; Then, perf.
;;
;; Performance: simply pre-cast all entering longs (or type hint if
;; doable)
;; Even?/odd? =~= max?/min?
;;
;; Lastly, tournament mode
;;
;; action 

;; theoretically could make this return a number; if 0, quiet, if
;; positive, boost interest that many turns
(defn simple-quiet
  "True when nothing interesting happened (piece taken)"
  [oldgs newgs]
  (was-taken? (game-state-reserves oldgs) (game-state-reserves newgs)))

(defmacro ab-h-m-s
  ""
  [source move-extractor cleanup max? alpha beta hist height [next] & block]
  `(case-pattern
    [~max? true false]
    [cur# ~alpha ~beta
     opp# ~beta ~alpha
     compo# greater-equals less-equals
     mnmx# fastmax fastmin
     lossv# negative-infinity positive-infinity]
    (let [rd# ~source
          qfrp#
          (if (.hasNext rd#)
            (loop [cur# cur# recm# -1]
              (if (.hasNext rd#)
                (let [~next (.next rd#)
                      rank# (do ~@block)              
                      cur# (mnmx# rank# cur#)
                      recm# (if (equals cur# rank#)
                              (~move-extractor ~next)
                              recm#)]
                  (if (compo# cur# opp#)
                    (do
                      (when-not (equals recm# -1)
                        (hist-add! ~hist ~height recm#))
                      cur#)
                    (recur cur# recm#)))
                (do
                  (when-not (equals recm# -1)
                    (hist-add! ~hist ~height recm#))
                  cur#)))
            lossv#)]
      (~cleanup rd#)
      qfrp#)))

(defmacro ab-h-m
  "Depth: how many iterations left. Name it height?"
  [buffer gamestate owner max? alpha beta hist height [next] & block]
  `(ab-h-m-s (move-generator ~buffer ~gamestate ~owner (hist-ordering ~hist ~buffer))
             signed-gs-move dispose-move-generator!
             ~max? ~alpha ~beta ~hist ~height [~next] ~@block))


(defmacro ab-n-m
  "Alpha beta loop holder. Read the damn source."
  [buffer gamestate owner max? alpha beta [next] & block]
  `(case-pattern
    [~max? true false]
    [cur# ~alpha ~beta
     opp# ~beta ~alpha
     compo# greater-equals less-equals
     mnmx# fastmax fastmin
     lossv# negative-infinity positive-infinity]
    (let [rd# (unordered-move-generator ~buffer ~gamestate ~owner)]
      (if (.hasNext rd#)
        (loop [cur# cur#]
          (if (.hasNext rd#)
            (let [~next (.next rd#)
                  rank# (do ~@block)              
                  cur# (mnmx# rank# cur#)]
              (if (compo# cur# opp#)
                cur#
                (recur cur#)))
            cur#))                      
        lossv#))))

(def-search cls-ab-search
  ""
  {0 [1 neginf posinf]
   1 [2 neginf posinf]
   2 [3 neginf posinf]
   3 [5 neginf posinf]
   4 [8 neginf posinf]}
  []
  (:eval
   [buffer gamestate player rank-func depth alpha beta]
   (letfn [(rec [gamestate owner depth alpha beta max?]
             (if (equals depth 0)
               (rank-func gamestate player)
               (ab-n-m buffer gamestate owner max? alpha beta [ngs]
                       (rec ngs (negate owner) (dec-1 depth)
                            alpha beta (not max?)))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) depth alpha beta false))))

;; longify macro;
;;
;; (longify [alpha beta gamma delta]
;;    block)
;;

(def-search cls-ab-hist-search
  "History heuristic alpha beta search."
  {0 [1 neginf posinf]
   1 [2 neginf posinf]
   2 [3 neginf posinf]
   3 [5 neginf posinf]
   4 [8 neginf posinf]}
  [hihi]
  (:pre [& args]
        (export hihi (make-hist-table)))
  (:post [& args]
         (hist-clear! hihi))
  (:eval
   [buffer gamestate player rank-func depth alpha beta]
   (longify [player]
            (letfn [(rec [gamestate owner depth alpha beta max?]
                      (longify [depth alpha beta]
                               (if (equals 0 depth)
                                 (rank-func gamestate player)
                                 (ab-h-m buffer gamestate owner max? alpha beta hihi depth [ngs]
                                         (rec ngs (negate owner) (dec-1 depth)
                                              alpha beta (not max?))))))]
              (when (<= beta alpha)
                (println "What's up with the window??"))
              (rec gamestate (negate player) depth alpha beta false)))))

(def-search cls-ab-transp-search
  "So what if I indent five times?"
  {0 [1 neginf posinf]
   1 [2 neginf posinf]
   2 [3 neginf posinf]
   3 [5 neginf posinf]
   4 [8 neginf posinf]}
  [mtable]
  (:pre [& args]
        (export mtable (make-dtab 21)))
  (:post [& args]
         (dtab-clear! mtable))
  (:eval
   [buffer gamestate player rank-func depth alpha beta]
   (letfn [(rec [gamestate owner level alpha beta max?]
             (if (equals level depth)
               (rank-func gamestate player)
               (ab-n-m buffer gamestate owner max? alpha beta [ngs]
                       (if (less-equals level 3)
                         (let [key (compress-sgs buffer ngs owner)
                               lrnk (dtab-get mtable key (negate level))]
                           (if lrnk lrnk
                               (let [r (rec ngs (negate owner) (inc-1 level)
                                            alpha
                                            beta
                                            (not max?))]
                                 (dtab-add! mtable key (negate level) r)
                                 r)))
                         (rec ngs (negate owner) (inc-1 level)
                              alpha
                              beta
                              (not max?))))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) 0 alpha beta false))))

(def-search cab-transp-hist
  "Awesomeness."
  {0 [1 neginf posinf]
   1 [2 neginf posinf]
   2 [3 neginf posinf]
   3 [5 neginf posinf]
   4 [8 neginf posinf]}
  [mtable hist]
  (:pre [& args]
        (export mtable (make-dtab 21))
        (export hist (make-hist-table)))
  (:post [& args]
         (hist-clear! hist)
         (dtab-clear! mtable))
  (:eval
   [buffer gamestate player rank-func depth alpha beta]
   (letfn [(rec [gamestate owner depth alpha beta max?]
             (if (equals 0 depth)
               (rank-func gamestate player)
               (ab-h-m buffer gamestate owner max? alpha beta
                       hist depth [ngs]
                       (let [key (compress-sgs buffer ngs owner)
                             lrnk (dtab-get mtable key depth)]
                         (if lrnk lrnk
                             (let [r (rec ngs (negate owner) (dec-1 depth)
                                          alpha beta (not max?))]
                               (dtab-add! mtable key depth r)
                               r))))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) depth alpha beta false))))

(def-search aspiration
  "We can pass in the guess-func rather nicely.
   WARNING: do not use a transp-table using
   guess func until the search-structure is well
   developed."
  {0 [80 1 (:eval rank-board-hybrid)]
   1 [80 2 (:eval rank-board-hybrid)]
   2 [80 3 (:eval rank-board-hybrid)]
   3 [80 5 (:eval rank-board-hybrid)]
   4 [80 8 (:eval rank-board-hybrid)]}
  []
  (:pre [& args]
        ((:pre cls-ab-transp-search)))
  (:post [& args]
         ((:post cls-ab-transp-search)))
  (:eval
   [buffer gamestate player rankf radius depth guess-func & guess-args]
   (let [guess (apply guess-func gamestate player guess-args)]
     (let [alpha (subtract guess radius)
           beta (add guess radius)
           res (cls-ab-transp-search-func buffer gamestate player rankf depth alpha beta)]
       (cond (equals res alpha)
             (do
               ;; (println "Failed low")
               (cls-ab-transp-search-func buffer gamestate player rankf depth negative-infinity beta))
             (equals res beta)
             (do
               ;; (println "Failed high")
               (cls-ab-transp-search-func buffer gamestate player rankf depth alpha positive-infinity))
             :else
             res)))))

(def-search mtd-f
  "MTD-F works best with quantized heuristics."
  {0 [1 (:eval rank-board-hybrid)]
   1 [2 (:eval rank-board-hybrid)]
   2 [3 (:eval rank-board-hybrid)]
   3 [5 (:eval rank-board-hybrid)]
   4 [8 (:eval rank-board-hybrid)]}
  []
  (:pre [& args]
        ((:pre cls-ab-transp-search)))
  (:post [& args]
         ((:post cls-ab-transp-search)))
  (:eval
   [buffer gamestate player rankf depth guess-func & guess-args]
   (let [guess (apply guess-func gamestate player guess-args)]
     (loop [guess guess upper positive-infinity lower negative-infinity]
       (let [beta 
             (if (equals guess lower)
               (inc-1 guess)
               guess)]
         (let [next (cls-ab-transp-search-func buffer gamestate player rankf depth (dec-1 beta) beta)]
           (if (less next beta)
             (let [upper next]
               (if (less-equals upper lower)
                 upper
                 (recur next upper lower)))
             (let [lower next]
               (if (less-equals upper lower)
                 lower
                 (recur next upper lower))))))))))

(definline node-move
  [node]
  `(signed-gs-move (idr-node-gamestate ~node)))

(definline do-nothing
  [thing]
  `nil)

(defn-iter-with-context itr
  "Helper to idrn-ab-h"
  [buffer good-player rank-func mxdepth endtime hist transp]
  [node depth trange max? alpha beta]
  (longify [depth trange alpha beta]
           (if (and (greater-equals trange 3) (past-time? endtime))
             node
             (let [gamestate (idr-node-gamestate node)
                   owner (idr-node-player node)
                   antiowner (negate owner)
                   ^java.util.Iterator ochildren (idr-node-children node)
                   nchildren (clist-make)
                   orank (idr-node-rank node)
                   nrank
                   ;;
                   ;; Children:
                   ;; {} empty - terminal
                   ;; nil - expandme!
                   ;; {bob, jones, frank} - update!!
                   ;;
                   (if (and (not (nil? ochildren)) (not (.hasNext ochildren)))
                     orank
                     (if (nil? ochildren)
                       (ab-h-m buffer gamestate owner max? alpha beta hist depth
                               [ngs]
                               (let [key (compress-sgs buffer ngs antiowner)
                                     next (if (equals depth 0)
                                            (make-idr-node
                                             ngs antiowner                
                                             (rank-func ngs good-player))
                                            (let [nork (dtab-get transp key depth)]
                                              (if (nil? nork)
                                                (itr (make-idr-node ngs antiowner 0)
                                                     (dec-1 depth) (dec-1 trange) (not max?) alpha beta)
                                                (make-idr-node ngs antiowner nork))))
                                     rank (idr-node-rank next)]
                                 (dtab-add! transp key depth rank)
                                 (clist-add nchildren next rank)
                                 rank))
                       (ab-h-m-s ochildren node-move do-nothing max? alpha beta hist depth [onodule]
                                 (let [okey (compress-sgs buffer (idr-node-gamestate onodule)
                                                          (idr-node-player onodule))
                                       nodule (let [ttr (dtab-get transp okey depth)]
                                                (if (nil? ttr)
                                                  (let [nog (itr onodule (dec-1 depth)
                                                                 (dec-1 trange) (not max?) alpha beta)
                                                        nkey (compress-sgs buffer
                                                                           (idr-node-gamestate nog)
                                                                           (idr-node-player nog))]
                                                    (dtab-change! transp nkey
                                                                  depth (idr-node-rank nog))
                                                    nog)                                       
                                                  ;; note that the children are
                                                  ;; not updated - would the table
                                                  ;; entry ever dissappear, we would
                                                  ;; have to go through the entire
                                                  ;; tree again.
                                                  (idr-node-update onodule ttr
                                                                   (idr-node-children onodule))))
                                       rank (idr-node-rank nodule)]
                                   (clist-add nchildren nodule rank)
                                   rank))))]
               (idr-node-update node nrank (clist-pack nchildren max?))))))

(def-search idrn-ab-h
  "Godlike."
  {0 [2 1 50 neginf posinf]
   1 [2 1 100 neginf posinf]
   2 [4 2 500 neginf posinf]
   3 [8 2 10000 neginf posinf]
   4 [12 2 20000 neginf posinf]}
  [hist transp]
  (:pre [& args]
        ;; could we do a fully static, with overwrites? saves
        ;; 16 bytes mem/47 each, no overflow (save on alloc)
        (export transp (make-dtab 21))
        (export hist (make-hist-table)))
  (:post [& args]
         (hist-clear! hist)
         (dtab-clear! transp))
  (:eval
   [buffer gamestate good-player rank-func depth istep time alpha beta]
   (let [endtime (add (get-thread-time) (* 1e6 time))]
     (when (greater-equals alpha beta)
       (println "Warning: alpha and beta have incorrect ordering."))
     (loop [nodetree (make-idr-node gamestate (negate good-player) 0)
            level 0]
       (if (or (past-time? endtime) (greater level depth))
         (idr-node-rank nodetree)
         (recur (itr buffer good-player rank-func depth endtime hist transp
                     nodetree level 4 false alpha beta)
                (add level istep)))))))

(defn-iter-with-context qht-sub
  "lt - long term depth; s - short term depth"
  [buffer good-player rank-func quiet-func qboost hist transp]
  [gamestate owner sdepth ltdepth alpha beta max?]
  (longify
   [sdepth ltdepth alpha beta]
   (if (or (equals sdepth 0) (equals ltdepth 0))
     (rank-func gamestate good-player)
     (ab-h-m buffer gamestate owner max? alpha beta hist ltdepth [ngs]
             (let [key (compress-sgs buffer ngs (negate owner))
                   llrk (dtab-get transp key ltdepth)]
               (if llrk 
                 llrk
                 (let [ww (qht-sub ngs (negate owner)
                                   (if (quiet-func gamestate ngs)
                                     (dec-1 sdepth)
                                     qboost)
                                   (dec-1 ltdepth)
                                   alpha
                                   beta
                                   (not max?))]
                   (dtab-add! transp key ltdepth ww)
                   ww)))))))

;; just mix quiescient, hist, transp. simple. right??>
(def-search qab-hist-transp
  {0 [simple-quiet 1 2 1 neginf posinf]
   1 [simple-quiet 2 3 1 neginf posinf]
   2 [simple-quiet 2 4 2 neginf posinf]
   3 [simple-quiet 3 7 3 neginf posinf]
   4 [simple-quiet 6 12 4 neginf posinf]}
  [hist transp]
  (:pre [& args]
        (export transp (make-dtab 21))
        (export hist (make-hist-table)))
  (:post [& args]
         (hist-clear! hist)
         (dtab-clear! transp))
  (:eval [buffer gamestate good-player rank-func quiet-func mindepth maxdepth qboost alpha beta]
         (qht-sub buffer good-player rank-func
                  quiet-func qboost
                  hist transp
                  gamestate (negate good-player)
                  mindepth maxdepth alpha beta false)))

(def-config deep-quiescent
  "Focus on unpredicability"
  qab-hist-transp
  {0 [simple-quiet 1 2 1 neginf posinf]
   1 [simple-quiet 2 4 2 neginf posinf]
   2 [simple-quiet 3 6 3 neginf posinf]
   3 [simple-quiet 4 10 4 neginf posinf]
   4 [simple-quiet 5 12 4 neginf posinf]})

;; inline it?? nah... Large func vs. small func..
(definline play-game
  [buffer good-player gs owner]
  `(loop [gs# ~gs owner# ~owner count# 0]
     (let [nxt# (get-random-progression ~buffer gs# owner#)]
       (if nxt#
         (recur nxt# (negate owner#) (inc-1 count#))
         (do
           ;;(println count#) ;; This shows that most of these are failure by exhaustion (no takes)
           (multiply ~good-player owner#))))))

(def monte-carlo-difficulties {0 [10]
                               1 [200]
                               2 [5000]
                               3 [100000]
                               4 [2000000]
                               5 [50000000]})

(def-search foolish-monte-carlo
  "Magic"
  monte-carlo-difficulties
  []
  (:eval
   [buffer gamestate good-player _ iterations]
   (let [anti (negate good-player)]
     (loop [i 0 rnk 0]
       (if (greater-equals i iterations)
         rnk
         (recur (inc-1 i)
                (add rnk
                     (play-game buffer good-player gamestate anti))))))))

(definline mcr
  [buf good gs actor count]
  `(loop [i# 0 rnk# 0]
     (if (greater-equals i# ~count)
       rnk#
       (recur (inc-1 i#)
              (add rnk# (play-game ~buf ~good ~gs ~actor))))))

(def-search tree-monte-carlo
  "Uses transp, hist, & plain alpha beta. Evaluation is monte-carlo style."
  {0 [1 10 neginf posinf]
   1 [2 50 neginf posinf]
   2 [3 250 neginf posinf]
   3 [4 1250 neginf posinf]
   4 [5 6250 neginf posinf]}
  [transp hist]
  (:pre [& args]
        (export transp (make-dtab 21))
        (export hist (make-hist-table)))
  (:post [& args]
         (hist-clear! hist)
         (dtab-clear! transp))
  (:eval
   [buffer gamestate good-player _ depth iterations alpha beta]
   (longify
    [good-player depth iterations alpha beta]
    (letfn [(tmc [gamestate owner depth alpha beta max?]
              (let [opp (negate owner)
                    dd (dec-1 depth)]
                (if (equals depth 0)
                  (mcr buffer good-player gamestate opp iterations)
                  (ab-h-m buffer gamestate owner max? alpha beta hist depth [ngs]
                          (let [key (compress-sgs buffer ngs opp)
                                llrk (dtab-get transp key depth)]
                            (if llrk llrk
                                (let [ww (tmc ngs opp dd
                                              alpha beta (not max?))]
                                  (dtab-add! transp key depth ww)
                                  ww)))))))]
      (tmc gamestate (negate good-player) depth alpha beta false)))))

(def-search evaluate
  "Static evaluation."
  {0 [] 1 [] 2 [] 3 [] 4 []}
  []
  (:eval [buffer gamestate good-player rank-func]
         (rank-func gamestate good-player)))

(definline uct-adv
  [buffer good-player uk gamestate node owner]
  `(let [next# (uctn-select ~node)
         res# (~uk next# (apply-move ~buffer ~gamestate ~owner (uctn-move next#)) (negate ~owner))]
     (uctn-post! ~node ~good-player res#)
     res#))

(def uctsc 0.10)

(def-search uct-search
  "Almost direct copy from a go website."
  {0 [10 uctsc]
   1 [200 uctsc]
   2 [5000 uctsc]
   3 [100000 uctsc]
   4 [2000000 uctsc]}
  []
  (:pre [it_ selectivity-constant]
    (set-uct-constant! selectivity-constant))
  (:post [& _])
  (:eval
   [buffer gamestate good-player rf_ iterations selc_]
   (letfn [(uk [node gamestate owner]
             ;; notice the three-fold repetition. definline!
             (if (uctn-untried node)
               (let [res (play-game buffer good-player gamestate owner)]
                 (uctn-post! node good-player res)
                 res)
               (if (uctn-children? node)
                 (uct-adv buffer good-player uk gamestate node owner)
                 (let [desc (random-move-generator buffer gamestate owner)]
                   (if (.hasNext desc)
                     (do (loop []
                           (uctn-grow! node (make-uctn (.next desc)))
                           (when (.hasNext desc)
                             (recur)))
                         (uct-adv buffer good-player uk gamestate node owner))
                     (do
                       (uctn-terminate! node good-player owner)
                       (negate owner)))))))]
     (let [parent (make-uctn gamestate)
           antiplayer (negate good-player)]
       (loop [i 0]
         (if (greater i iterations)
           (uctn-rank parent good-player good-player)
           (do
             (uk parent gamestate antiplayer)
             (recur (inc i)))))))))

(def-search uct-ab-search
  "Not high level optimized, but whatever"
  {0 [1 10 uctsc]
   1 [1 70 uctsc]
   2 [2 100 uctsc]
   3 [2 1000 uctsc]
   4 [3 2000 uctsc]}
  [evf
   uckf]
  (:pre [dp_ it_ selconst]
        ((:pre uct-search) it_ selconst)
        ((:pre cab-transp-hist) dp_)
        (export evf (:eval cab-transp-hist))
        (export uckf (:eval uct-search)))
  (:post [dp_ it_ selconst]
         ((:post cab-transp-hist) it_ selconst)
         ((:post uct-search) dp_))
  (:eval
   [buffer gamestate good-player rf_ depth iterations selc_]
   (evf buffer gamestate good-player
        (fn [gamestate player]
          (uckf buffer gamestate player nil iterations nil))
        depth negative-infinity positive-infinity)))
