(ns gipf.core
  (:import (gipfj Geometry MathUtil Board GameState Reserves Line
             IDRNode GameCalc GeneralizedPointWeighting
             Ranking Counter Entry))
  (:gen-class))

(set! *warn-on-reflection* true)

;;;
;;; The purpose of this core file
;;; is to hold all of the ugly coordination
;;; between the gui and the data, as well
;;; as the main loop. Other files are to
;;; provide the needed logic, graphics,
;;; or abstractions
;;;

;; everything should be explicitly passed into these

(defmacro llload
  [name]
  `(do (load ~name)
       (println "Loaded <" ~name ">")))

(defn player->index
  [^long player]
  (if (neg? player) 0 1))

;; what happens if we get a cyclical dependency??

;; best way: specify dependencies;
;; have a function that calls llload in the right order.
;; ex: (load-all
;; [math util geo reserves graphics game-aux ranking ai game]
;;  (game ai ranking game-aux graphics)
;; (ranking geo reserves util)
;; (graphics geo util)

(llload "settings")
(llload "math")
(llload "util")
(llload "geo")
(llload "reserves")
(llload "graphics")
(llload "field")
(llload "ranking")
(llload "ai")
(llload "game")
(llload "window")

;; NEXT on the TODO list;
;;
;; Improve ai.... It can't beat me yet.
;; Better move sorting, quiescent search, transp tables, easily
;; enabled/disabled incremental ranking in a gamestate. (ie, by using
;; new classes that extend gamestate, add some info.) Maker funcs
;; are easy to swap, doable in :setup of a Heuristic
;;


;; "things"

(def mode* :normal)
(def board* (new-board mode*))
(def reserve-pieces* (new-reserves mode*))

(def current-player* 1) ; -1 or 1
(def selected* nil)
;; why don't we just default this to -1? well, at least nil fails loudly
(def hovered* "Cell over which the mouse is hovering" nil)
(def lines* "List of lines that the human player can remove" (list))
(def already-removed-lines* "List of lines that were removed; tournament mode.." (list))
(def rings* "List of gipf-piece positions that will not be taken" (list))

(def game-phase* :placing)
(def adv-phase* [:playing :playing]) ; this may cause problems...
(def player-types* [:ai :human])
(def placed-cell-value* 0)

(defn ai-player?
  [player]
  (= :ai (get player-types* (player->index player))))

(defn human-player?
  [player]
  (= :human (get player-types* (player->index player))))

(defn set-player-type!
  [player tp]
  (def player-types*
    (atv player-types* (player->index player) (constantly tp))))

(defn get-pieces-left
  [player]
  (get-pieces-in-reserve reserve-pieces* player))

(defn draw-pieces-left!
  [player]
  (let [i (player->index player)
        c (get piece-colors i)
        v (get-pieces-in-reserve reserve-pieces* player)]
    (if (= i 0)
      (draw-text-centered-at! (xy 100 700) (str v) c)
      (draw-text-centered-at! (xy 700 700) (str v) c))))

(defn change-reserves!
  [player d-rpieces d-bpieces d-gipfs]
  (let [n (reserve-delta reserve-pieces* player d-rpieces d-bpieces d-gipfs)]
    (def reserve-pieces* n))
  (draw-pieces-left! player))

(defn pack-gamestate
  [board reserves advphasev]
  (->GameState board reserves
               (= :filling (get advphasev (player->index 1)))
               (= :filling (get advphasev (player->index -1)))))


(defn draw-lines-at-loc!
  [loc]
  (doseq [line lines*]
    (when (on-line? loc line)
      (draw-line! line)))
  (when (some #(pt= loc %) rings*)
    (draw-ring! loc)))

(defn filter-out-already-used-lines
  "Takes a list of lines. ;-)"
  [lol]
  (filter (fn [l]
            (not (some (fn [k] (line= k l)) already-removed-lines*)))
          lol))

(defn draw-lines!
  []
  (doseq [line lines*]
    (draw-line! line))
  (doseq [ring rings*]
    (draw-ring! ring)))

(defn redraw-loc!
  [loc]
  (let [^java.awt.Graphics2D g game-graphics]
    (.setColor g java.awt.Color/WHITE)
    (.fill g (hex-tile-at loc))
    (.setColor g java.awt.Color/BLACK)
    (if (= 4 (pt-radius loc))
      (if (and (not (nil? selected*)) (pt= loc selected*))
        (draw-piece-at-loc! loc placed-cell-value*)
        (.fill g (circle-at loc 10)))
      (let [f (get-hex-array board* loc)]
        (if (= 0 f)
          (.fill g (circle-at loc 5))
          (draw-piece-at-loc! loc f))))))

(defn redraw-loc-disk!
  [loc]
  (doseq [i (list loc
                  (pt+ loc (pt 1 0 0))
                  (pt+ loc (pt 0 1 0))
                  (pt+ loc (pt 0 0 1))
                  (pt+ loc (pt -1 0 0))
                  (pt+ loc (pt 0 -1 0))
                  (pt+ loc (pt 0 0 -1)))]
    (when (<= (pt-radius i) 4)
      (redraw-loc! i))))

(defn draw-base!
  []
  (let [^java.awt.Graphics2D g game-graphics]
    (doto g
      (.setColor java.awt.Color/WHITE)
      (.fillRect 0 0 800 800)
      (.setColor java.awt.Color/BLACK))
    
    (doseq [p (range (hexagonal-number 5))]
      (redraw-loc! p))
    (.setFont ^java.awt.Graphics2D g numfont)
    (draw-pieces-left! -1)
    (draw-pieces-left! 1)))

(defn redraw-all!
  []
  (draw-base!)
  (if selected*
    (draw-highlight! selected* true))
  (draw-player-indicator! current-player*)
  (repaint!))

(defn protected?
  [loc]
  (some #(pt= loc %) rings*))

(defn empty-line!
  [line]
  (println "Emptying" line)
  (let [e1p (pt- (line-start line) (line-delta line))
        llp (get-line-limit-point (line-start line) (line-delta line))
        e2p (pt+ llp
                 (line-delta line))]
    (clear-line! e1p e2p)
    (redraw-loc! e1p)
    (redraw-loc! e2p)
    (loop [cur (line-start line)]
      (let [val (get-hex-array board* cur)]
        (when-not (or (= val 0) (protected? cur))
          (if (same-sign? val current-player*)
            (if (= 2 (abs val))
                (change-reserves! current-player* 2 0 -1)
                (change-reserves! current-player* 1 -1 0))
            (if (= 2 (abs val))
              (change-reserves! (- current-player*) 0 0 -1)
              (change-reserves! (- current-player*) 0 -1 0)))
          
          (def board* (change-hex-array board* cur 0)))
        (redraw-loc! cur)
        (when (and (not (nil? hovered*)) (pt= cur hovered*))
          (draw-highlight! cur))
        (when-not (pt= cur llp)
          (recur (pt+ cur (line-delta line))))))))

(defn undraw-line!
  [line]
  (let [e1p (pt- (line-start line) (line-delta line))
        e2p (pt+ (get-line-limit-point
                  (line-start line) (line-delta line))
                 (line-delta line))]
    (clear-line! e1p e2p)
    (loop [cur e1p]
      (redraw-loc! cur)
      (when (pt= cur hovered*)
        (draw-highlight! cur))
      (when-not (pt= cur e2p)
        (recur (pt+ cur (line-delta line)))))))

(defn place-piece!
  [loc player]
  (def selected* loc)
  (def placed-cell-value* player))

(defn move-piece!
  [loc shove shovevalue]
  (let [[newgs updated]
        (do-move (pack-gamestate board* reserve-pieces* adv-phase*)
                 shovevalue loc shove)]
    (def board* (game-state-board newgs))
    ;; set the reserves as well?

    (def placed-cell-value* 0)
    (redraw-loc-disk! (pt- loc shove))
    
    (doseq [p updated]
      (redraw-loc! p)
      (when (and (not (nil? hovered*)) (pt= hovered* p))
        (draw-highlight! hovered*)))))

;; should really extract the (partial owns-line? player) pattern...
(defn owns-line?
  [player line]
  (same-sign? (line-sig line) player))

(defn toggle-piece!
  "Toggle the state of the piece as or as not a GIPF-potential."
  [loc]
  (let [target (if (odd? placed-cell-value*)
                 (* placed-cell-value* 2)
                 (/ placed-cell-value* 2))]
    
    (if (= (abs target) 2)
      (change-reserves! (sign placed-cell-value*) -1 -1 1)
      (change-reserves! (sign placed-cell-value*) 1 1 -1))

    (def placed-cell-value* target)
    
    (redraw-loc! loc)
    (draw-highlight! loc)
    (draw-selector! loc board*)
    (repaint!)))

(defn get-adv-phase
  []
  (get adv-phase* (player->index current-player*)))

(defn set-adv-phase!
  [value]
  (def adv-phase* (atv adv-phase*
                       (player->index current-player*)
                       (constantly value))))


;; THREADING START

(def update-game) ; declare

(let [ai-action-threads (atom {})
      add-ai-action-thread! (fn [key val]
                              (swap! ai-action-threads
                                     #(assoc % key [val true])))
      remove-ai-action-thread! (fn [key]
                                 (swap! ai-action-threads
                                        #(dissoc % key)))
      check-ai-action-thread (fn [key]
                               (second (get @ai-action-threads key)))
      end-ai-action-thread! (fn [key]
                              (swap! ai-action-threads
                                     (fn [u]
                                       (let [^java.lang.Thread t (first (get u key))]
                                         (.interrupt t)
                                         (assoc u key [t false])))))
      get-next-key-num (let [kn (atom 0)]
                         (fn []
                           (swap! kn inc)
                           @kn))]

  (defn start-compound-ai-move!
    []
    ;; one question remains: how does one cut/interrupt compound-ai-move ?
    (let [p current-player*
          key (get-next-key-num)
          gs (pack-gamestate board* reserve-pieces* adv-phase*)
          thrd (proxy [java.lang.Thread] []
                 (run []
                   (try
                     (let [action (compound-ai-move gs p)]
                       (busy-doing-important-stuff 1.0)
                       (if (check-ai-action-thread key)
                         (on-swing-thread
                           (update-game (list (cons :caimove action))))
                         (println "### Aborting move:" key)))
                     (catch java.lang.Exception e
                       (.printStackTrace e)))
                   (remove-ai-action-thread! key)))]
      (add-ai-action-thread!
       key
       (doto thrd (.start)))))

  (defn interrupt-ai-threads!
    []
    (doseq [[key _] @ai-action-threads]
      (println "sending EOL")
      (end-ai-action-thread! key))))

;; THREADING OVER

(defn switch-players!
  []
  (def current-player* (- current-player*))
  ;; cleanup after the line removal action...
  (draw-player-indicator! current-player*)
  (def already-removed-lines* (list)))

(defn set-lines!
  "Sets the lines and deals with the rings..."
  [all owner]
  (let [r (filter (partial owns-line? owner) all)]
    (def lines* r)
    (def rings* (reduce concat (map
                                (partial get-own-gipf-potentials-in-line board* owner)
                                r)))
    (draw-lines!)))

(defn game-over!?
  "Example use: (when-not (game-over!?) (proceed) (finalize)"
  []
  ;; warning: this fails when ai has a clear left, but 0 in reserve
  ;; _the best loss check is the lack of any moves to make_
  ;; - for that, use magic.
  
  (if (lost? board* reserve-pieces* current-player* mode* (get-adv-phase))
    (do
      (println "Game over!")
      (def game-phase* :gameover)
      (draw-game-over! (- current-player*))
      true)
    false))

(defn setup-new-game!
  []
  (interrupt-ai-threads!)
  (def board* (new-board mode*))
  (def reserve-pieces* (new-reserves mode*))
  (def current-player* 1)
  (def selected* nil)
  (def lines* (list))
  (def rings* (list))
  (setup-ai!)  

  (if (= mode* :advanced)
    (def adv-phase* [:filling :filling])
    (def adv-phase* [:playing :playing]))
  (if (human-player? current-player*)
    (def game-phase* :placing)
    (do
      (def game-phase* :waiting-for-ai)
      (start-compound-ai-move!)))
  
  (redraw-all!))

(defn update-hover!
  [hoverpt]
  (cond (nil? hovered*)
        (do
          (draw-highlight! hoverpt)
          (repaint!)
          (def hovered* hoverpt))
        (pt= hoverpt hovered*)
        :nothing-to-do-here
        :else
        (do
          (redraw-loc! hovered*)
          (when (and (not (nil? selected*))
                     (<= (pt-dist hovered* selected*) 1)) 
            (draw-selector! selected* board*))
          (draw-lines-at-loc! hovered*)
          
          (draw-highlight! hoverpt)
          (repaint!)
          (def hovered* hoverpt))))

(defn toggle-ring!
  [clickpt]
  (if (protected? clickpt)
    (def rings* (remove #(pt= clickpt %) rings*))
    (do
      (def rings* (cons clickpt rings*))
      (draw-ring! clickpt)))
  (redraw-loc! clickpt)
  (draw-lines-at-loc! clickpt)                     
  (when (pt= clickpt hovered*)
    (draw-highlight! hovered*))
  (repaint!))

(defn start-next-move!
  []
  (switch-players!)
  (when-not (game-over!?)
    (if (ai-player? current-player*)
      (do
        (def game-phase* :waiting-for-ai)
        (start-compound-ai-move!))
      ;; human must clear...
      (let [found (filter (partial owns-line? current-player*)
                          (get-lines-of-four board*))]
        (if (seq found)
          (do
            (def game-phase* :removing-pre)
            (set-lines! found current-player*))
          (def game-phase* :placing))))))

(defn post-human-move!
  []
  (let [found (filter (partial owns-line? current-player*)
                      (filter-out-already-used-lines
                       (get-lines-of-four board*)))]
    (if (seq found)
      ;; keep on removing...
      (do
        (def game-phase* :removing-post)
        (set-lines! found current-player*))
      (if (= game-phase* :removing-pre)
        (def game-phase* :placing)
        (start-next-move!)))))

(defn try-row-clearing!
  [clickpt]

  (doseq [line lines*] 
    (when (on-line? clickpt line)
      (empty-line! line)
      (def already-removed-lines*
        (cons line already-removed-lines*)))
    (undraw-line! line))
  (def lines* (list))
  (def rings* (list))

  (post-human-move!)
  
  (repaint!))

(defn shove-piece!
  [clickpt delvec]
  (when (and (= mode* :advanced)
             (= (get-adv-phase) :filling))
    (cond (= 1 placed-cell-value*)
          (set-adv-phase! :playing)
          ;; can't afford another gipf
          (= 1 (get-pieces-left current-player*))
          (do
            (set-adv-phase! :playing)
            (def placed-cell-value* current-player*))))
  (def selected* nil)
  (move-piece! clickpt delvec placed-cell-value*)

  (post-human-move!)

  (repaint!))

(defn human-add-piece!
  [loc]

  (if (and (= mode* :advanced) (= (get-adv-phase) :filling))
    (do (place-piece! loc (* 2 current-player*))
        (change-reserves! current-player* -2 0 1))
    (do
      (place-piece! loc current-player*)
      (change-reserves! current-player* -1 1 0)))
  (def selected* loc)
  (redraw-loc! loc)
  (draw-highlight! loc)
  (draw-selector! loc board*)
  (repaint!)
  (def game-phase* :moving))

(defn effect-compound-ai-move!
  [clear1 move clear2]

  ;; temporary - clear ai gunk
  (draw-base!)
  
  ;; clear  
  (def rings* (ffirst clear1))
  (doseq [line (rest clear1)]
    (empty-line! line))
  (def rings* (list))

  ;; shove
  (let [degree (line-sig move)
        advline (advance-line move)]
    (when (and (= (get-adv-phase) :filling) (= degree 1))
      (set-adv-phase! :playing))
    (if (= degree 2)
      (change-reserves! current-player* -2 0 1)
      (change-reserves! current-player* -1 1 0))
    (move-piece! (line-start advline) (line-delta advline) (* current-player* degree)))

  ;; clear
  (def rings* (ffirst clear2))
  (doseq [line (rest clear2)]
    (empty-line! line))
  (def rings* (list))
  
  (start-next-move!)
  
  (repaint!))

;; this function must be called on the swing thread
(defn update-game
  "Updates the game given list of inputs.
   Input is of the form [:click x y b] 
   [:state :new :basic/:advanced/:tournament]
   [:hover x y].
   Returns if any one of the inputs changed something"
  [input-list]
  (doseq [input input-list]
    (case (first input)
      :state
      (case (second input)
        :new       (setup-new-game!)
        :basic     (def mode* :basic)
        :normal    (def mode* :normal)
        :advanced  (def mode* :advanced)
        :player-ai (set-player-type! (third input)
                                     (if (fourth input) :ai :human)))
      :caimove
      (effect-compound-ai-move! (second input) (third input) (fourth input))

      :click
      (cond (= (fourth input) 1)
            (let [clickpt (screenpx-to-loc (xy (second input) (third input)))
                  rad (pt-radius clickpt)]
              ;; The human player goes through a four-step move
              ;;
              ;; removing-pre   - clears own lines from opp's move
              ;; placing        - places a piece
              ;; moving         - shoves it
              ;; removing-post  - clears own lines from own move
              ;;
              (when (<= rad 4)
;;                (println clickpt game-phase*)
                (case game-phase*
                  :placing
                  (when (and (= rad 4)
                             (> (get-pieces-left current-player*) 0)
                             (place-point-open? board* clickpt))
                    (human-add-piece! clickpt))
                  :moving
                  (when (<= (pt-dist clickpt selected*) 1)
                    (cond (= rad 3)
                          (let [delvec (pt- clickpt selected*)]
                            (when-not (row-full? board* clickpt delvec)
                              (shove-piece! clickpt delvec)))

                          (and (= mode* :advanced)
                               (= (get-adv-phase) :filling)
                               (pt= clickpt selected*))
                          (toggle-piece! clickpt)))
                  (:removing-pre :removing-post)
                  (cond (= 4 rad)
                        (try-row-clearing! clickpt)         
                        (= 2 (abs (get-hex-array board* clickpt)))
                        (toggle-ring! clickpt))
                  :waiting-for-ai
                  nil
                  :gameover
                  (println "Game is over; can't interact.")))))
      :hover
      (let [hoverpt (screenpx-to-loc (xy (second input) (third input)))]
        (when (and (<= (pt-radius hoverpt) 4) (not= game-phase* :gameover))
          (update-hover! hoverpt))))))

;; how do we know if this has been run before?
(defn runSimulation
  [mode type]
  (start-thread
   (simulate mode type)))

(defn -main
  "See \"GIPF: I play the game\" for details."
  [& args]
  (prn "recieved args:" args)
  (if (some #(.equals "--sim" %) args)
      (runSimulation :normal :mct)
      (runGUI))
  "MAIN")

(println "Loaded < core >")
