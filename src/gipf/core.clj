(ns gipf.core
  (:gen-class))

;;;
;;; The purpose of this core file
;;; is to hold all of the ugly coordination
;;; between the gui and the data, as well
;;; as the main loop. Other files are to
;;; provide the needed logic, graphics,
;;; or abstractions
;;;


; everything should be explicitly passed into these

(load "math") ; free
(load "util") ; free
(load "geo") ; free
(load "game") ; free
(load "graphics") ;; game-panel initialized here...

;; "things"

(def mode* :normal)
(def board* (new-board mode*))
(def reserve-pieces* (new-reserves mode*))

(def current-player* (- (* 2 (rand-int 2)) 1)) ; -1 or 1
(def removing-player* nil)
(def selected* nil)
(def hovered* "Cell over which the mouse is hovering" nil)
(def lines* "List of lines that the human player can remove" (list))
(def already-removed-lines* "List of lines that were removed; tournament mode.." (list))
(def rings* "List of gipf-piece positions that will not be taken" (list))

(def game-phase* :placing)
(def adv-phase* [:playing :playing]) ; this may cause problems...

(defn get-pieces-left
  [player]
  (get reserve-pieces* (if (= player -1) 0 1)))

(defn draw-pieces-left!
  [player]
  (let [i (if (= player -1) 0 1)
        c (get piece-colors (- 1 i))
        v (get reserve-pieces* i)]
    (if (= i 0)
      (draw-text-centered-at! (xy 100 700) (str v) c)
      (draw-text-centered-at! (xy 700 700) (str v) c))))

(defn dec-pieces-left!
  "Decreases the number of pieces left for the player by 1."
  [player]
  (let [n (atv reserve-pieces* (if (= player -1) 0 1) dec)]
    (def reserve-pieces* n))
  (draw-pieces-left! player))

(defn inc-pieces-left!
  [player]
  "Increases the number of pieces left for the player by 1"
  (let [n (atv reserve-pieces* (if (= player -1) 0 1) inc)]
    (def reserve-pieces* n))
  (draw-pieces-left! player))


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
  (.setColor game-graphics java.awt.Color/WHITE)
  (.fill game-graphics (hex-tile-at loc))
  (.setColor game-graphics java.awt.Color/BLACK)
  (let [f (get-hex-array board* loc)]
    (if (= 0 f)
      (if (= 4 (pt-radius loc))
        (.fill game-graphics (circle-at loc 10))
        (.fill game-graphics (circle-at loc 5)))
      (do

        (if (> f 0)
          (.setColor game-graphics (get piece-colors 0))
          (.setColor game-graphics (get piece-colors 1)))
        (.fill game-graphics (circle-at loc 30))
        (when (= 2 (abs f))
          (.setColor game-graphics (scale-color (.getColor game-graphics) 0.5))
          (.fill game-graphics (circle-at loc 25)))))))


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
  (.setColor game-graphics java.awt.Color/WHITE)
  (.fillRect game-graphics 0 0 800 800)
  (.setColor game-graphics java.awt.Color/BLACK)
  
  (doseq [p (apply concat (map get-ring-of-hex-uv-points (range 5)))]
    (redraw-loc! p))
  (.setFont game-graphics numfont)
  (draw-pieces-left! -1)
  (draw-pieces-left! 1))

(defn redraw-all!
  []
  (draw-base!)
  (if selected*
    (draw-highlight! selected* true))
  (repaint!))

(defn protected?
  [loc]
  (some #(pt= loc %) rings*))

(defn empty-line!
  [line]
  (let [e1p (pt- (second line) (third line))
        e2p (pt+ (get-line-limit-point
                  (second line) (third line))
                 (third line))]
    (clear-line! e1p e2p)
    (loop [cur e1p]
      (let [val (get-hex-array board* cur)]
        (when-not (or (= val 0) (protected? cur))
          (when (same-sign? val removing-player*)
            (when (= 2 (abs val))
              (inc-pieces-left! removing-player*))
            (inc-pieces-left! removing-player*))
          (def board* (change-board-cell board* cur 0)))
        (redraw-loc! cur)
        (when (pt= cur hovered*)
          (draw-highlight! cur))
        (when-not (pt= cur e2p)
          (recur (pt+ cur (third line))))))))

(defn undraw-line!
  [line]
  (let [e1p (pt- (second line) (third line))
        e2p (pt+ (get-line-limit-point
                  (second line) (third line))
                 (third line))]
    (clear-line! e1p e2p)
    (loop [cur e1p]
      (redraw-loc! cur)
      (when (pt= cur hovered*)
        (draw-highlight! cur))
      (when-not (pt= cur e2p)
        (recur (pt+ cur (third line)))))))

(defn place-piece!
  [loc player]
  (let [newboard (map-hex-array (fn [p c] (if (pt= p loc) player c)) board*)]
    (def board* newboard)))

(defn move-piece!
  [loc shove]
  (let [[newboard updated] (do-move board* current-player* loc shove)]
    (def board* newboard)
    (redraw-loc-disk! (pt- loc shove))
    (doseq [p updated]
      (redraw-loc! p)
      (when (pt= hovered* p)
        (draw-highlight! hovered*)))))

;; should really extract the (partial owns-line? player) pattern...
(defn owns-line?
  [player line]
  (same-sign? (first line) player))

(defn toggle-piece!
  "Toggle the state of the piece as or as not a GIPF-potential."
  [loc]
  (let [v (get-hex-array board* loc)
        target (if (odd? v) (* v 2) (/ v 2))]
    
    (if (= (abs target) 2)
      (do
        (inc-pieces-left! (sign v)))
      (do
        (dec-pieces-left! (sign v))))
    
    (def board* (change-board-cell board* loc target))

    (redraw-loc! loc)
    (draw-highlight! loc)
    (draw-selector! loc board*)
    (repaint!)))

(def update-game) ; declare

(def ai-action-thread* nil)
(defn start-ai-clear!
  [found]
  (let [b board*
        r removing-player*]
    (def ai-action-thread*
      (start-thread
       (let [action (ai-clear b r found)]
         (on-swing-thread
          (update-game 
           (list (cons :aiclear action)))))))))

(defn get-adv-phase
  []
  (get adv-phase* (if (= current-player* 1) 1 0)))

(defn set-adv-phase!
  [value]
  (def adv-phase* (atv adv-phase*
                       (if (= current-player* -1) 0 1)
                       (constantly value))))

(defn start-ai-move!
  []
  (let [b board*
        p current-player*
        rp reserve-pieces*]
    (def ai-action-thread*
      (start-thread
       (let [action (ai-move board* current-player*
                             reserve-pieces* (get-adv-phase))]
         (on-swing-thread
          (update-game (list (cons :aimove action)))))))))

(defn switch-players!
  []
  (def current-player* (- current-player*))
                                        ; cleanup after the line removal action...
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
  (if (lost?  board* reserve-pieces* current-player* mode* (get-adv-phase))
    (do (def game-phase* :gameover)
        (draw-game-over! (- current-player*))
        true)
    false))

(defn setup-new-game!
  []
  (when-not (nil? ai-action-thread*)
    (.interrupt ai-action-thread*))
  (def board* (new-board mode*))
  (def reserve-pieces* (new-reserves mode*))
  (def current-player* (- current-player*))
  (def selected* nil)
  (def lines* (list))
  (def rings* (list))
  (def game-phase* :placing)
  (if (= mode* :advanced)
    (def adv-phase* [:filling :filling])
    (def adv-phase* [:playing :playing]))
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

(defn try-row-clearing!
  [clickpt]
                  
  (doseq [line lines*] 
    (when (on-line? clickpt line)
      (empty-line! line)
      (def already-removed-lines*
        (cons line already-removed-lines*)))
    (undraw-line! line))
  
  (let [found (filter-out-already-used-lines
               (get-lines-of-four board*))]
    ;; removing-player is human...
    (set-lines! found removing-player*)
    (if (seq found)
      (when (empty? lines*)
        ;; was human turn; it cleaned; now ai cleans
        (do ;; switch to ai removal
          (def game-phase* :waiting-for-ai)
          (def removing-player* (- current-player*))
          (start-ai-clear! found)))
      (if (= current-player* removing-player*)
        ;; human player cleaned rest; ai turn
        (do 
          (switch-players!)
          (when-not (game-over!?)
            (def game-phase* :waiting-for-ai)
            (start-ai-move!)))
        (do ;; ai turn is over now; human player cleaned up
          (switch-players!)
          (def game-phase* :placing))))
    (repaint!)))

(defn shove-piece!
  [clickpt delvec]

  (when (and (= mode* :advanced)
             (= (get-adv-phase) :filling)
             (= 1 (abs (get-hex-array board* selected*))))
    (set-adv-phase! :playing))
  (move-piece! clickpt delvec)
  (def selected* nil)
  
  (let [found (get-lines-of-four board*)]
    (if (seq found)
      (if (some (partial owns-line? current-player*) found)
        (do ; self has stuff to remove first
          (def game-phase* :removing-rows)
          (def removing-player* current-player*)
          (set-lines! found removing-player*))
        (do ; only the ai is clearing
          (def game-phase* :waiting-for-ai)
          (def removing-player* (- current-player*))
          (start-ai-clear! found)))
      (do 
        (switch-players!)
        (when-not (game-over!?)
          (def game-phase* :waiting-for-ai)
          (start-ai-move!)))))
  (when (not= game-phase* :gameover)
    (draw-highlight! clickpt))
  (repaint!))

(defn human-add-piece!
  [loc]

  (if (and (= mode* :advanced) (= (get-adv-phase) :filling))
    (place-piece! loc (* 2 current-player*))
    (do
      (place-piece! loc current-player*)
      (dec-pieces-left! current-player*)))
  (def selected* loc)
  (redraw-loc! loc)
  (draw-highlight! loc)
  (draw-selector! loc board*)
  (repaint!)
  (def game-phase* :moving))

(defn effect-ai-clearing!
  [keep line]
  ;; removing player is the AI;
  ;; if currentplayer is the same, removing player is finalizing its move;
  ;; otherwise, it is a reaction
  (def rings* keep)
  (empty-line! line)
  (def rings* (list))
  (def already-removed-lines* (cons line already-removed-lines*))
  
  (let [found (filter-out-already-used-lines
               (get-lines-of-four board*))]
    (if (seq found)
      (if (some (partial owns-line? removing-player*) found)
        (do ;; AI continues
          (start-ai-clear! found))
        (do ;; nothing left for the ai; player is next
          (def game-phase* :removing-rows)
          (def removing-player* (- current-player*))
          (set-lines! found removing-player*)))
      
      ;; nothing left at all:
      (if (= removing-player* current-player*)
        (do ; ai is done; player has nothing to clear
          (switch-players!)
          (when-not (game-over!?)
            (def game-phase* :placing)))
        (do ; player had finished; ai is done clearing; on to its turn
          (def game-phase* :waiting-for-ai)
          (switch-players!)
          (start-ai-move!))))
    (repaint!)))

(defn effect-ai-move!
  [place shove degree]

  (when (and (= (get-adv-phase) :filling) (= degree 1))
    (set-adv-phase! :playing))
      
  (place-piece! place (* degree current-player*))
  (dec-pieces-left! current-player*)
  (move-piece! (pt+ place shove) shove)
  (when (pt= place hovered*)
    (draw-highlight! hovered*))
  
  (let [found (get-lines-of-four board*)]
    (if (seq found)
      (if (some (partial owns-line? current-player*) found)
        (do
          (def removing-player* current-player*)
          (start-ai-clear! found))
        (do
          ;; this is still the ai players turn; but none belong to it
          (def removing-player* (- current-player*))
          (set-lines! found removing-player*)
          (draw-lines!)
          (def game-phase* :removing-rows)))
      (do
        (switch-players!)
        (when-not (game-over!?)
          (def game-phase* :placing)))))
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
      (cond (= (second input) :new)
            (setup-new-game!)
            (= (second input) :basic)
            (def mode* :basic)
            (= (second input) :normal)
            (def mode* :normal)
            (= (second input) :advanced)
            (def mode* :advanced))
      :aimove
      (effect-ai-move! (second input) (third input) (fourth input))
      :aiclear
      (effect-ai-clearing! (third input) (second input))
      :click
      (cond (= (fourth input) 1)
            (let [clickpt (screenpx-to-loc (xy (second input) (third input)))
                  rad (pt-radius clickpt)]
              (println :click clickpt rad)
              (if (<= rad 4)
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
                  :removing-rows
                  (cond (= 4 rad)
                        (try-row-clearing! clickpt)         
                        (= 2 (abs (get-hex-array board* clickpt)))
                        (toggle-ring!))
                  :waiting-for-ai
                  (println "Waiting for AI" (get-hex-array board* clickpt))
                  :gameover
                  (println "Game is over; can't interact.")))))
      :hover
      (let [hoverpt (screenpx-to-loc (xy (second input) (third input)))]
        (when (and (<= (pt-radius hoverpt) 4) (not= game-phase* :gameover))
          (update-hover! hoverpt)))
      
      )) ; case/doseq tail

  true)

(defn -main
  "See \"GIPF: I play the game\" for details."
  [& args]
  (let [window (javax.swing.JFrame. "GIPF")
        menubar (javax.swing.JMenuBar.)
        mode-basic (javax.swing.JRadioButtonMenuItem. "Basic")
        mode-normal (javax.swing.JRadioButtonMenuItem. "Normal")
        mode-advanced (javax.swing.JRadioButtonMenuItem. "Advanced")
        
        button-new (javax.swing.JMenuItem. "New")
        button-quit (javax.swing.JMenuItem. "Quit")
        ]

    ;; we call a new game;
    (update-game (list [:state :new]))
    
    (.setSelected (case mode*
                    :basic  mode-basic
                    :normal mode-normal
                    :advanced mode-advanced) true)
    
    (set-on-button-select! mode-basic
                           (fn []
                             (update-game (list [:state :basic] [:state :new]))))
    (set-on-button-select! mode-normal
                           (fn []
                             (update-game (list [:state :normal] [:state :new]))))
    (set-on-button-select! mode-advanced
                           (fn []
                             (update-game (list [:state :advanced] [:state :new]))))
    
    (doto (javax.swing.ButtonGroup.)
      (.add mode-basic)
      (.add mode-normal)
      (.add mode-advanced))
    
    (set-on-button-click! button-quit
                          (fn [] (doto window
                                  (.setVisible false)
                                  (.dispose))))
    
    (set-on-button-click! button-new
                          (fn [] (update-game (list [:state :new]))))
    
    (doto menubar
      ;; selector menu... use radiobutton?
      (.add (doto (javax.swing.JMenu. "Mode")
              (.add mode-basic)
              (.add mode-normal)
              (.add mode-advanced)))
      (.add (doto (javax.swing.JMenu. "Game")
              (.add button-new)
              (.add button-quit))))
    
    (doto game-panel
      (.setMinimumSize (java.awt.Dimension. 800 800))
      (.setMaximumSize (java.awt.Dimension. 800 800))
      (.setPreferredSize (java.awt.Dimension. 800 800))
      (.addMouseListener (proxy [java.awt.event.MouseListener] []
                           (mouseClicked [^java.awt.event.MouseEvent e]
                             (update-game (list [:click (.getX e) (.getY e) (.getButton e)])))
                           (mouseEntered [^java.awt.event.MouseEvent e] nil)
                           (mouseExited [^java.awt.event.MouseEvent e] nil)
                           (mousePressed [^java.awt.event.MouseEvent e] nil)
                           (mouseReleased [^java.awt.event.MouseEvent e] nil)))
      (.addMouseMotionListener (proxy [java.awt.event.MouseMotionListener] []
                                 (mouseDragged [^java.awt.event.MouseEvent e] nil)
                                 (mouseMoved [^java.awt.event.MouseEvent e] 
                                   (update-game (list [:hover (.getX e) (.getY e)]))))))
    
    (doto window
      (.setJMenuBar menubar)
      (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
      (.setContentPane game-panel)
      (.addKeyListener (proxy [java.awt.event.KeyListener] []
                         (keyPressed [e] nil)
                         (keyTyped [e] nil)
                         (keyReleased [e]
                           (when (= java.awt.event.KeyEvent/VK_ESCAPE (.getKeyCode e))
                             (println "Quitting on ESC key! Yay!")
                             (.setVisible window false)
                             (.dispose window)))))
      (.pack)
      (.setResizable false)
      (.setVisible true))
    
    true))
