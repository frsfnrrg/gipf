(in-ns 'gipf.core)

;; TODO eventually; test if, at fixed depth, inf time, 
;; idr-ab and idr actually give the same results.
;; they should.... if it's coded right

;; This file should contain all the interface between the logic
;; and the rest of the game... Yeah. Right.
(defn setup-ai!
  []
  (setup-move-ranking-func! 1 rank-board-hybrid idr-ab-ranking 6 50)
  (setup-move-ranking-func! -1 rank-tactical idr-ab-ranking 6 50))

(let [dia (atom {:move-newlines false
                 :move-numbers false
                 :match-result true
                 :total-time false
                 :incremental-time false
                 :moves-available false
                 :reserve-status false
                 :board-snapshot true
                 :rank-value false
                 :pre-rank-value false
                 :screen-display true
                 :evaluation-count true
                 :equals-moves false})]
  (defn set-diagnostic-level!
    [key ^Boolean on]
    (swap! dia (fn [a] (assoc a key on))))
  (defn get-diagnostic-level
    [key]
    (get @dia key))
 (defmacro ond
    [key & actions]
    `(when (get-diagnostic-level ~key)
       ~@actions)))

(defn compound-ai-move
  [board ^long player ^Reserves reserves adv-phase]

  (use-move-ranking-func! player)
  
  ;; we assume the opening strategy ignores the gipfiness when in
  ;; :filling mode. Should we? I do not think so..
  (swap! ranks-count (constantly 0)) 
  (let [pieces-left (get-reserves reserves player)
        possible-moves (shuffle
                        (list-possible-moves-and-board board reserves player))
        ngipfs (count-over-hex-array board (* 2 player))
        degree (if (and (= adv-phase :filling) (< ngipfs 4)) 2 1)
        current-rank (move-ranking-func* (->GameState board reserves) player)
        _ (ond :pre-rank-value (println "Starting rank:" current-rank))
        optimal (timev (rand-best
                       (fn [[move [board res]]]
                         (let [rank
                               (timev (move-ranking-func*
                                      (->GameState board res)
                                      player)
                                      (get-diagnostic-level :incremental-time))]
                           (ond :rank-value
                                (println "Rank:" rank))
                           (ond :screen-display
                                (direct-visualize-ai-ranking (second move) (- rank current-rank)))
                           rank))
                       nil -100000 possible-moves
                       (get-diagnostic-level :equal-moves))
                       (get-diagnostic-level :total-time)
                       )
        [c1 m c2] (first (or optimal (rand-nth possible-moves)))]
    (ond :evaluation-count
         (println "Nodes evaluated:" @ranks-count))
    ;; best would be, until mouse is moved...
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
    :basic (->Reserves 3 1)
    :advanced (->Reserves 18 0)
    :normal (->Reserves 15 3)))

(defn lost?
  [board reserves player mode advm]
  ;; TODO: make the GameState advm phase aware...; 
  ;; then allow gipfs to be placed under advm
  (let [res (incrementally-list-state-continuations (->GameState board reserves) player)]
    (ond :moves-available
         (println "Moves available:" (count res)))
    (empty? res)))

(defn next-move
  [gamestate player md]
  (let [move (compound-ai-move (game-state-board gamestate)
                               player
                               (game-state-reserves gamestate)
                               md)]
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
    (loop [gamestate (->GameState (new-board mode) (new-reserves mode))
           player 1
           counter 0]
      (ond :move-newlines
           (println))
      (ond :move-numbers
           (println (str "Move #: " counter "; Player " player)))
      (if (lost? (game-state-board gamestate)
                 (game-state-reserves gamestate)
                 player mode md)
        (do
          (ond :match-result
            (println "Player" (negate player) "won"))
          (negate player))
        (let [ng (next-move gamestate player md)]
          (ond :reserve-status
               (println "->" (game-state-reserves gamestate)))
          (ond :board-snapshot
               (print-board (game-state-board gamestate)))
          (recur ng (negate player) (inc-1 counter)))))))

(def number-of-trials 1)

(defn simulate
  [mode]
  (setup-move-ranking-func! 1 rank-board-simple idr-ab-ranking 6 30)
  (setup-move-ranking-func! -1 rank-board-simple idr-ab-ranking 6 30)
  (println)
  ;; we could run 1000 matches..
  (loop [count 0  win1 0 win2 0]
    (if (>= count number-of-trials)
      (println "Winner ratio: 1:" win1 "-1:" win2)
      (let [r (run-match mode)]
        (if (= r 1)
          (recur (inc count) (inc win1) win2)
          (recur (inc count)  win1 (inc win2)))))))
