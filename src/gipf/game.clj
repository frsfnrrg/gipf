(in-ns 'gipf.core)

;; This file should contain all the interface between the logic
;; and the rest of the game... Yeah. Right.

(def tournament-ai-gipfs 5)

(defn compound-ai-move
  [gamestate player]

  (ond :move-newlines
       (println))

  (init-move-ranking-func! player)
  (ond :pre-calc-message
       (println "Beginning move"))

  ;; we assume the opening strategy ignores the gipfiness when in
  ;; :filling mode. Should we? I do not think so..
  (clear-counter ranks-count) 
  (let [move-ranking-func (get-move-ranking-func player)
        possible-moves (shuffle
                        (list-possible-moves-and-board gamestate player))
        current-rank (move-ranking-func gamestate player)
        _ (ond :pre-rank-value (println "Starting rank:" current-rank))
        optimal (timev (rand-best
                        (fn [[move gamestate]]
                          (let [rank
                                (timev (move-ranking-func gamestate player)
                                       (get-diagnostic-level :incremental-time))]
                            (ond :rank-value
                                 (println "Rank:" rank))
                            (ond :screen-display
                                 (direct-visualize-ai-ranking (second move) (- rank current-rank)))
                            rank))
                        nil -100000 possible-moves
                        (get-diagnostic-level :equal-moves))
                       (get-diagnostic-level :total-time))
        [c1 m c2] (first (or optimal (rand-nth possible-moves)))]

    (ond :evaluation-count
         (println "Nodes evaluated:" (read-counter ranks-count)))

    (teardown-move-ranking-func! player)

    ;; we use gipfs-on-board for compat with simple mode    
    (if (= 0 (get-gipfs-on-board (game-state-reserves gamestate) player))
        [c1 (sign-line m 2) c2]
        [c1 (sign-line m (abs (line-sig m))) c2])))



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
    :basic (->Reserves 12 3 1)
    :advanced (->Reserves 18 0 0)
    :normal (->Reserves 15 0 3)))

(defn new-gamestate
  [mode]
  (->GameState (new-board mode) (new-reserves mode)
               (= mode :advanced) (= mode :advanced)))

(defn lost?
  ([board reserves player mode advm]
     (let [res (lazy-next-gamestates (->GameState board reserves
                                                  (= advm :filling)
                                                  (= advm :filling))
                                     player)]
       (ond :moves-available
            (println "Moves available:" (count res)))
       (empty? res)))
  ([gamestate player]
     (not (.hasNext (unordered-move-generator gamestate player)))))

(defn next-move
  [gamestate player]
  (let [move (compound-ai-move gamestate player)]
    (do-linemoves (place-and-shove (do-linemoves gamestate
                                                 player
                                                 (first move))
                                   player
                                   (advance-line (second move)))
                  player
                  (third move))))

;; I want a recur-through/recur over, with opt. break
;;
;; (loop-over [item foo] [n 0]
;;   (end n)
;;   (if (nil? item)
;;       (break n)
;;       (continue (inc n))))
;;
;; additionally, a loop-through, in similar style
;;

(defn run-match
  [mode]
  (let [md :playing]
    (loop [gamestate (new-gamestate mode)
           player 1
           counter 0]
      (ond :move-newlines
           (println))
      (ond :move-numbers
           (println (str "Move #: " counter "; Player " player)))
      (if (lost? gamestate player)
        (do
          (ond :match-result
               (println "Player" (negate player) "won"))
          (negate player))
        (let [ng (next-move gamestate player)]
          (ond :reserve-status
               (println "->" (game-state-reserves gamestate)))
          (ond :board-snapshot
               (print-board (game-state-board gamestate)))
          (recur ng (negate player) (inc-1 counter)))))))

(def number-of-trials 1)

(defn mct-effect-move
  [gamestate player move]
  (do-linemoves (place-and-shove (do-linemoves gamestate
                                               player
                                               (first move))
                                 player
                                 (advance-line (second move)))
                player
                (third move)))

(defn move-comparison-trial
  "Requires players to be awesome. No, wait."
  [sear1 heur1 sear2 heur2]
  (loop [gamestate (new-gamestate :normal)
         player 1
         counter 0]
    (ond :move-newlines
         (println))
    (ond :move-numbers
         (println (str "Move #: " counter "; Player " player)))
    (if (lost? gamestate player)
      (do
        (ond :match-result
             (println "MCT \"winner\":" (negate player)))
        (negate player))
      (let [m1 (do (sear1 heur1 player)
                 (compound-ai-move gamestate player))
            g1 (mct-effect-move gamestate player m1)
            m2 (do (sear2 heur2 player)
                 (compound-ai-move gamestate player))
            g2 (mct-effect-move gamestate player m2)]
        (println "ai +1:" m1)
        (println "ai -1:" m2)
        (if (> player 0)
          (recur g1 (negate player) (inc counter))
          (recur g2 (negate player) (inc counter)))))))



(let [dmrf (fn [& args]
             (fn [^Search s ^long p] (apply setup-move-ranking-func! p s args)))
      cab-light
      (dmrf cls-ab-search 3 negative-infinity positive-infinity)
      cab-transp-light
      (dmrf cls-ab-transp-search 3 negative-infinity positive-infinity)
      cab-hist-light
      (dmrf cls-ab-hist-search 3 negative-infinity positive-infinity)
      qthab-light
      (dmrf qab-hist-transp simple-quiet 3 6 2 negative-infinity positive-infinity)
      thab-light
      (dmrf cab-transp-hist 3 negative-infinity positive-infinity)
      
      qthab-deep
      (dmrf qab-hist-transp simple-quiet 6 10 2 negative-infinity positive-infinity)
      thab-deep
      (dmrf cab-transp-hist 6 negative-infinity positive-infinity)      
      cab-hist-deep
      (dmrf cls-ab-hist-search 5 negative-infinity positive-infinity)
      cab-transp-deep
      (dmrf cls-ab-transp-search 6 negative-infinity positive-infinity)
      
      idrnh-light
      (dmrf idrn-ab-h 6 2 10000 negative-infinity positive-infinity)
      
      aspire-simple
      (dmrf aspiration 80 5 (:eval rank-board-hybrid))
      mtdf-simple
      (dmrf mtd-f 5 (:eval rank-board-hybrid))
      aspire-deep
      (dmrf aspiration 80 5 (:eval rank-board-hybrid))
      mtdf-deep
      (dmrf mtd-f 5 (:eval rank-board-hybrid))]
  
  

  (defn setup-ai!
    []
    (cab-hist-light rank-gf1 1)
    (cab-transp-light rank-board-hybrid -1))
 
  (defn simulate
    [mode type]
    (println)
    (case type
      :mct
      (move-comparison-trial idrnh-light rank-gf1
                             idrnh-light rank-board-hybrid)
      :comp
      (do
        (idrnh-light rank-gf1 1)
        (idrnh-light rank-board-hybrid -1)
        (loop [count 0  win1 0 win2 0]
          (if (>= count number-of-trials)
            (println "Winner ratio: 1:" win1 "-1:" win2)
            (let [r (run-match mode)]
              (if (= r 1)
                (recur (inc count) (inc win1) win2)
                (recur (inc count)  win1 (inc win2))))))))))
