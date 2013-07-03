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
  [source move-extractor max? alpha beta hist height [next] & block]
  `(case-pattern
    [~max? true false]
    [cur# ~alpha ~beta
     opp# ~beta ~alpha
     compo# greater-equals less-equals
     mnmx# fastmax fastmin
     lossv# negative-infinity positive-infinity]
    (let [rd# ~source]
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
        lossv#))))

(defmacro ab-h-m
  "Depth: how many iterations left. Name it height?"
  [gamestate owner max? alpha beta hist height [next] & block]
  `(ab-h-m-s (move-generator ~gamestate ~owner (hist-ordering ~hist))
             signed-gs-move
             ~max? ~alpha ~beta ~hist ~height [~next] ~@block))


(defmacro ab-n-m
  "Alpha beta loop holder. Read the damn source."
  [gamestate owner max? alpha beta [next] & block]
  `(case-pattern
    [~max? true false]
    [cur# ~alpha ~beta
     opp# ~beta ~alpha
     compo# greater-equals less-equals
     mnmx# fastmax fastmin
     lossv# negative-infinity positive-infinity]
    (let [rd# (unordered-move-generator ~gamestate ~owner)]
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
   [gamestate player rank-func depth alpha beta]
   (letfn [(rec [gamestate owner depth alpha beta max?]
             (if (equals depth 0)
               (rank-func gamestate player)
               (ab-n-m gamestate owner max? alpha beta [ngs]
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
   [gamestate player rank-func depth alpha beta]
   (longify [player]
     (letfn [(rec [gamestate owner depth alpha beta max?]
               (longify [depth alpha beta]
                 (if (equals 0 depth)
                   (rank-func gamestate player)
                   (ab-h-m gamestate owner max? alpha beta hihi depth [ngs]
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
   [gamestate player rank-func depth alpha beta]
   (letfn [(rec [gamestate owner level alpha beta max?]
             (if (equals level depth)
               (rank-func gamestate player)
               (ab-n-m gamestate owner max? alpha beta [ngs]
                        (if (less-equals level 3)
                          (let [key (compress-sgs ngs owner)
                                lrnk (dtab-geta mtable key)]
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
   [gamestate player rank-func depth alpha beta]
   (letfn [(rec [gamestate owner level alpha beta max?]
             (if (equals level depth)
               (rank-func gamestate player)
               (ab-h-m gamestate owner max? alpha beta
                       hist (subtract level depth) [ngs]
                       (let [key (compress-sgs ngs owner)
                             lrnk (dtab-geta mtable key)]
                         (if lrnk lrnk
                             (let [r (rec ngs (negate owner) (inc-1 level)
                                          alpha
                                          beta
                                          (not max?))]
                               (dtab-add! mtable key (negate level) r)
                               r))))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) 0 alpha beta false))))

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
   [gamestate player rankf radius depth guess-func & guess-args]
   (let [guess (apply guess-func gamestate player guess-args)]
     (let [alpha (subtract guess radius)
           beta (add guess radius)
           res (cls-ab-transp-search-func gamestate player rankf depth alpha beta)]
       (cond (equals res alpha)
             (do
               ;; (println "Failed low")
               (cls-ab-transp-search-func gamestate player rankf depth negative-infinity beta))
             (equals res beta)
             (do
               ;; (println "Failed high")
               (cls-ab-transp-search-func gamestate player rankf depth alpha positive-infinity))
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
   [gamestate player rankf depth guess-func & guess-args]
   (let [guess (apply guess-func gamestate player guess-args)]
     (loop [guess guess upper positive-infinity lower negative-infinity]
       (let [beta 
             (if (equals guess lower)
               (inc-1 guess)
               guess)]
         (let [next (cls-ab-transp-search-func gamestate player rankf depth (dec-1 beta) beta)]
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

(defn-iter-with-context itr
  "Helper to idrn-ab-h"
  [good-player rank-func mxdepth endtime hist transp]
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
               (ab-h-m gamestate owner max? alpha beta hist depth
                       [ngs]
                       (let [key (compress-sgs ngs antiowner)
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
               (ab-h-m-s ochildren node-move max? alpha beta hist depth [onodule]
                         (let [okey (compress-sgs (idr-node-gamestate onodule)
                                                  (idr-node-player onodule))
                               nodule (let [ttr (dtab-get transp okey depth)]
                                        (if (nil? ttr)
                                          (let [nog (itr onodule (dec-1 depth)
                                                         (dec-1 trange) (not max?) alpha beta)
                                                nkey (compress-sgs
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
   3 [6 2 10000 neginf posinf]
   4 [6 2 20000 neginf posinf]}
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
   [gamestate good-player rank-func depth istep time alpha beta]
   (let [endtime (add (System/nanoTime) (* 1e6 time))]
     (when (greater-equals alpha beta)
       (println "Warning: alpha and beta have incorrect ordering."))
     (loop [nodetree (make-idr-node gamestate (negate good-player) 0)
            level 0]
       (if (or (past-time? endtime) (greater level depth))
         (idr-node-rank nodetree)
         (recur (itr good-player rank-func depth endtime hist transp
                     nodetree level 4 false alpha beta)
                (add level istep)))))))

(defn-iter-with-context qht-sub
  "lt - long term depth; s - short term depth"
  [good-player rank-func quiet-func qboost hist transp]
  [gamestate owner sdepth ltdepth alpha beta max?]
  (longify
   [sdepth ltdepth alpha beta]
   (if (or (equals sdepth 0) (equals ltdepth 0))
     (rank-func gamestate good-player)
     (ab-h-m gamestate owner max? alpha beta hist ltdepth [ngs]
             (let [key (compress-sgs ngs (negate owner))
                   llrk (dtab-get transp key ltdepth)]
               (if (nil? llrk)
                 (let [ww (qht-sub ngs (negate owner)
                                   (if (quiet-func gamestate ngs)
                                     (dec-1 sdepth)
                                     qboost)
                                   (dec-1 ltdepth)
                                   alpha
                                   beta
                                   (not max?))]
                   (dtab-add! transp key ltdepth ww)
                   ww)
                 llrk))))))

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
  (:eval [gamestate good-player rank-func quiet-func mindepth maxdepth qboost alpha beta]
         (qht-sub good-player rank-func
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
