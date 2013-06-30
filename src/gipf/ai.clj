(ns gipf.core)


;; AI.clj

;;
;; TODO: next order of business 
;;
;; We need a "ab-hist-transp" search, for use in mtd-f & aspiration
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

(def-search cls-ab-search
  "I never actually did alpha beta with the min-max idea.
   Notably - alpha, beta are not symmetric. Other ab - qab, idr-ab, blah
   Were. Is this costing us??

  Condition: beta < alpha
  "
  []
  (:eval
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
                (let [rd (unordered-move-generator gamestate owner)]
                  (if (.hasNext rd)
                    (loop [cur cur]
                      (if (.hasNext rd)
                        (let [ngs (.next rd)
                              rank (rec ngs (negate owner) (dec-1 depth)
                                        alpha
                                        beta
                                        (not max?))]
                          (let [cur (mnmx rank cur)]
                            (if (compo cur opp)
                              cur
                              (recur cur))))
                        cur))                      
                    lossv)))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) depth alpha beta false))))

(def-search cls-ab-hist-search
  "History heuristic alpha beta search."
  [hihi]
  (:pre [& args]
        (export hihi (make-hist-table)))
  (:post [& args]
         (hist-clear! hihi))
  (:eval
   [gamestate player rank-func depth alpha beta]
   ;; TODO refactor out into defn-iter-with-context
   (letfn [(rec [gamestate owner depth alpha beta max?]
             (if (equals 0 depth)
               (rank-func gamestate player)
               (case-pattern
                [max? true false]
                [cur alpha beta
                 opp beta alpha
                 compo greater-equals less-equals
                 mnmx fastmax fastmin
                 lossv negative-infinity positive-infinity]
                (let [rd (move-generator gamestate owner (hist-ordering hihi))]
                  (if (.hasNext rd)
                    (loop [cur cur recm -1]
                      (if (.hasNext rd)
                        (let [ngs (.next rd)
                              rank (rec ngs (negate owner) (dec-1 depth)
                                        alpha
                                        beta
                                        (not max?))]
                          (let [cur (mnmx rank cur)]
                            (let [recm (if (equals cur rank)
                                         (signed-gs-move ngs)
                                         recm)]
                              (if (compo cur opp)
                                (do
                                  (when-not (equals recm -1)
                                    (hist-add! hihi depth recm))
                                  cur)
                                (recur cur recm)))))
                        (do
                          (when-not (equals recm -1)
                            (hist-add! hihi depth recm))
                          cur)))                      
                    lossv)))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) depth alpha beta false))))

(def-search cls-ab-transp-search
  "So what if I indent five times?"
  [mtable]
  (:pre [& args]
        (export mtable (make-dtab 23)))
  (:post [& args]
         (dtab-clear! mtable))
  (:eval
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
                (let [rd (unordered-move-generator gamestate owner)]
                  (if (.hasNext rd)
                    (loop [cur cur]
                      (if (.hasNext rd)
                        (let [ngs (.next rd)
                              rank (if (less-equals level 3)
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
                                          (not max?)))]

                          (let [cur (mnmx rank cur)]
                            (if (compo cur opp)
                              cur
                              (recur cur))))
                        cur))                      
                    lossv)))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) 0 alpha beta false))))

(def-search cab-transp-hist
  "Awesomeness."
  [mtable hist]
  (:pre [& args]
        (export mtable (make-dtab 23))
        (export hist (make-hist-table)))
  (:post [& args]
         (hist-clear! hist)
         (dtab-clear! mtable))
  (:eval
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
                (let [rd (move-generator gamestate owner (hist-ordering hist))]
                  (if (.hasNext rd)
                    (loop [cur cur recm -1]
                      (if (.hasNext rd)
                        (let [ngs (.next rd)
                              ;; transp block
                              rank (let [key (compress-sgs ngs owner)
                                         lrnk (dtab-geta mtable key)]
                                     (if lrnk lrnk
                                         (let [r (rec ngs (negate owner) (inc-1 level)
                                                      alpha
                                                      beta
                                                      (not max?))]
                                           (dtab-add! mtable key (negate level) r)
                                           r)))]
                          ;; hist block. Maaaacroooo....
                          (let [cur (mnmx rank cur)]
                            (let [recm (if (equals cur rank)
                                         (signed-gs-move ngs)
                                         recm)]
                              (if (compo cur opp)
                                (do
                                  (when-not (equals recm -1)
                                    (hist-add! hist (subtract 20 level) recm))
                                  cur)
                                (recur cur recm)))))
                        (do
                          (when-not (equals recm -1)
                            (hist-add! hist (subtract 20 level) recm))
                          cur)))                      
                    lossv)))))]
     (when (<= beta alpha)
       (println "What's up with the window??"))
     (rec gamestate (negate player) 0 alpha beta false))))

(def-search aspiration
  "We can pass in the guess-func rather nicely.
   WARNING: do not use a transp-table using
   guess func until the search-structure is well
   developed."
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

(defn-iter-with-context itr
  "Helper to idrn-ab-h"
  [good-player rank-func depth endtime hist transp]
  [node depth trange max? alpha beta]
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
            (case-pattern
             [max? true false]
             [cur alpha beta
              opp beta alpha
              compo greater-equals less-equals
              mnmx fastmax fastmin
              lossv negative-infinity positive-infinity]
             (if (nil? ochildren)
               (let [rd (move-generator gamestate owner (hist-ordering hist))]
                 (if (.hasNext rd)
                   (loop [cur cur recm -1]
                     (if (.hasNext rd)
                       (let [ngs (.next rd)
                             ;; transp!
                             next (if (equals depth 0)
                                    (make-idr-node
                                     ngs antiowner                
                                     (rank-func ngs good-player))

                                    (let [nork (dtab-get transp (compress-sgs
                                                                    ngs
                                                                    antiowner)
                                                 depth)]
                                      (if (nil? nork)
                                        (itr
                                         (make-idr-node ngs antiowner 0)
                                         (dec-1 depth)
                                         (dec-1 trange)
                                         (not max?)
                                         alpha beta)
                                        (make-idr-node ngs antiowner nork))))
                             rank (idr-node-rank next)]
                         (dtab-add! transp (compress-sgs
                                              (idr-node-gamestate next)
                                              (idr-node-player next)) depth rank)
                         (clist-add nchildren next rank)
                         (let [cur (mnmx rank cur)]
                           (let [recm (if (equals cur rank)
                                        (signed-gs-move ngs)
                                        recm)]
                             (if (compo cur opp)
                               (do
                                 (when-not (equals recm -1)
                                   (hist-add! hist depth recm))
                                 cur)
                               (recur cur recm)))))
                       (do
                         (when-not (equals recm -1)
                           (hist-add! hist depth recm))
                         cur)))
                   lossv))
               (let [rq ochildren]
                 (if (.hasNext rq)
                   (loop [cur cur recm -1]
                     (if (.hasNext rq)
                       (let [onodule (.next rq)
                             nodule (let [ttr (dtab-get transp
                                                            (compress-sgs
                                                           (idr-node-gamestate onodule)
                                                           (idr-node-player onodule)) depth)]
                                      (if (nil? ttr)
                                        (let [nog
                                              (itr
                                               onodule
                                               (dec-1 depth)
                                               (dec-1 trange)
                                               (not max?)
                                               alpha beta)]
                                          (dtab-change! transp (compress-sgs
                                                                  (idr-node-gamestate nog)
                                                                  (idr-node-player nog))
                                                          depth (idr-node-rank nog))
                                          nog)                                       
                                        ;; note that the children are
                                        ;; not updated - would the table
                                        ;; entry ever dissappear, we would
                                        ;; have to go through the entire
                                        ;; tree again.
                                        (idr-node-update onodule ttr
                                                         (idr-node-children onodule))))
                             ngs (idr-node-gamestate onodule)
                             rank (idr-node-rank nodule)]
                         (clist-add nchildren nodule rank)
                         (let [cur (mnmx rank cur)]
                           (let [recm (if (equals cur rank)
                                        (signed-gs-move ngs)
                                        recm)]
                             (if (compo cur opp)
                               (do
                                 (when-not (equals recm -1)
                                   (hist-add! hist depth recm))
                                 cur)
                               (recur cur recm)))))
                       (do
                         (when-not (equals recm -1)
                           (hist-add! hist depth recm))
                         cur)))
                   orank)))))]
      (idr-node-update node nrank (clist-pack nchildren max?)))))

(def-search idrn-ab-h
  "Godlike."
  [hist transp]
  (:pre [& args]
        ;; could we do a fully static, with overwrites? saves
        ;; 16 bytes mem/47 each, no overflow (save on alloc)
        (export transp (make-dtab 23))
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
  ;; one downside: qab has unpredicable depths
  ""
  [good-player rank-func quiet-func qboost hist transp]
  [gamestate owner sdepth ltdepth alpha beta max?]
  (if (or (equals sdepth 0) (equals ltdepth 0))
    (rank-func gamestate good-player)
    (let [subs (move-generator gamestate owner (hist-ordering hist))]
      (case-pattern
       [max? true false]
       [cur alpha beta
        opp beta alpha
        compo greater-equals less-equals
        mnmx fastmax fastmin
        lossv negative-infinity positive-infinity]
       (if (.hasNext subs)
         (loop [cur cur recm -1]
           (if (.hasNext subs)
             (let [ngs (.next subs)
                   rank (let [key (compress-sgs ngs (negate owner))
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
                            llrk))]
               (let [cur (mnmx rank cur)]
                 (let [recm (if (equals cur rank)
                              (signed-gs-move ngs)
                              recm)]
                   (if (compo cur opp)
                     (do
                       (when-not (equals recm -1)
                         (hist-add! hist ltdepth recm))
                       cur)
                     (recur cur recm)))))
             (do
               (when-not (equals recm -1)
                 (hist-add! hist ltdepth recm))
               cur)))
         lossv)))))

;; just mix quiescient, hist, transp. simple. right??>
(def-search qab-hist-transp
  [hist transp]
  (:pre [& args]
        (export transp (make-dtab 23))
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


;;
;; Still more ideas: the back/forth between moves, as demonstrated
;; by the heuristic, can throw off iterative deepening, as it prunes
;; too early. One can convert this into two-depth idr, which could
;; be smoother...
;;
;;
;;

