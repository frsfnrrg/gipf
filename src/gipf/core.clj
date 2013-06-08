(ns gipf.core
  (:gen-class))

;; use load, or use namespaces??

(load "math")
(load "util")
(load "geo")
(load "game")

;; constants

(def piece-colors (vector (java.awt.Color. 255 0 0) (java.awt.Color. 0 200 200)))
(def segment-length 76) ; px
(def board-center (xy 400 400)) ; px
(def color-bg (java.awt.Color. 255 255 255))
(def numfont (java.awt.Font. "Angleterre Book" java.awt.Font/PLAIN 70))

;; "things"

                                        ; these will be re-deffed
(def mode* :normal)
(def board* (new-board mode*))
(def reserve-pieces* (new-reserves mode*))
(def current-player* (- (* 2 (rand-int 2)) 1)) ; -1 or 1
(def removing-player* nil)
(def selected* nil)
(def hovered* "Cell over which the mouse is hovering" nil)
(def lines* "List of lines that the human player can remove" (list))
(def rings* "List of gipf-piece positions that will not be taken" (list))
(def game-phase* :placing)

;; constant.  blah
(def game-img (make-img 800 800))
(def game-graphics (.getGraphics game-img))
(def game-panel (proxy [javax.swing.JPanel] []
                  (paint [^java.awt.Graphics g]
                    (.drawImage g game-img 0 0 game-panel))))

(defn repaint!
  []
  (.repaint game-panel))


;; Drawing stuff


(defn loc-to-screenpx
  [loc]
  (xy+ board-center
       (xy* segment-length
            (pt->xy loc))))

(defn screenpx-to-loc
  [screenpx]
  (pt-int (xy->pt (xy* (/ segment-length) 
                       (xy- screenpx board-center)))))

(defn hex-tile-at
  [p]
  (let [[cx cy] (loc-to-screenpx p)
        xd (* (sqrt 3) (/ segment-length 4))
        yd (/ segment-length 4)
        ym (/ segment-length 2)]
    
    
    (java.awt.Polygon.
     (into-array Integer/TYPE [cx (- cx xd) (- cx xd) cx (+ cx xd) (+ cx xd)])
     (into-array Integer/TYPE [(+ cy ym) (+ cy yd) (- cy yd) (- cy ym) (- cy yd) (+ cy yd)])
     6)))

(defn circle-at
  [p rad]
  (let [[cx cy] (loc-to-screenpx p)]
    (java.awt.geom.Ellipse2D$Float. (- cx rad) (- cy rad) (* 2 rad) (* 2 rad))))


(defn get-pieces-left
  [player]
  (get reserve-pieces* (if (= player -1) 0 1)))

(defn get-gipfs-left
  [player]
  (get reserve-pieces* (if (= player -1) 2 3)))

(defn draw-text-centered-at!
  [xy text color]
                                        ; horizontally, it is fine.... The y coord corresponds to the 
  (let [[x y] xy
        fmetric (.getFontMetrics game-graphics)
        b (.getStringBounds fmetric text game-graphics)
        [hx hy] (map #(int (/ % 2)) [(.getWidth b) (.getHeight b)])]
    (doto game-graphics
      (.setColor color-bg)
                                        ; 4x; we will never drop more than 1 digit...
      (.fillRect (- x (* 2 hx)) (+ y (int (.getY b))) (* 4 hx) (int  (.getHeight b)))
      (.setColor color)
      (.drawString text (- x hx) y))))

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

(defn draw-gipfs-left!
  [player]
  (let [i (if (= player -1) 0 1)
        c (get piece-colors (- 1 i))
        v (get reserve-pieces* (+ 2 i))]
    (if (= i 0)
      (draw-text-centered-at! (xy 100 125) (str v) c)
      (draw-text-centered-at! (xy 700 125) (str v) c))))

(defn dec-gipfs-left!
  "Decreases the number of pieces left for the player by 1."
  [player]
  (let [n (atv reserve-pieces* (if (= player -1) 2 3) dec)]
    (def reserve-pieces* n))
  (draw-gipfs-left! player))

(defn inc-gipfs-left!
  [player]
  "Increases the number of pieces left for the player by 1"
  (let [n (atv reserve-pieces* (if (= player -1) 2 3) inc)]
    (def reserve-pieces* n))
  (draw-gipfs-left! player))



(defn draw-highlight!
  [loc]
  (.setColor game-graphics (java.awt.Color. 255 255 0 200))
  (.fill game-graphics (hex-tile-at loc)))

(defn draw-selector!
  [loc]
  (.setColor game-graphics (java.awt.Color. 0 0 255 200))
  (.fill game-graphics (circle-at loc 20))
  (let [[cx cy] (loc-to-screenpx loc)]
    ;; star shape
    (doseq [[px py] [[(* 40 (sqrt 3)) 40] [(* -40 (sqrt 3)) 40] [0 80]]]
      (.drawLine game-graphics (- cx px) (- cy py) (+ cx px) (+ cy py)) )))


(defn draw-line!
  [line]
  (let [end1 (pt- (second line) (third line))
        end2 (pt+ (get-line-limit-point (second line) (third line)) (third line))]
    (doto game-graphics
      (.setColor  java.awt.Color/GREEN)
      (.fill (circle-at end1 25))
      (.fill (circle-at end2 25))
      (.setColor  java.awt.Color/BLACK)
      (.fill (circle-at end1 15))
      (.fill (circle-at end2 15))
      (.setStroke (java.awt.BasicStroke. 15)))
    (let [[e1x e1y] (loc-to-screenpx end1)
          [e2x e2y] (loc-to-screenpx end2)]
      (.drawLine game-graphics  e1x e1y e2x e2y))
    (.setStroke game-graphics (java.awt.BasicStroke. 1))))


(defn semicircle-at
  [x y rad top?]
  (let [q (java.awt.geom.Arc2D$Float. java.awt.geom.Arc2D/OPEN)]
    (if top?
      (.setArcByCenter q x y rad 0 180 java.awt.geom.Arc2D/OPEN)
      (.setArcByCenter q x y rad 180 180 java.awt.geom.Arc2D/OPEN))
    q))

(defn semiring-at
  [x y inner outer top?]
  (let [k (java.awt.geom.Path2D$Float. java.awt.geom.Path2D/WIND_EVEN_ODD)
        inner-ring (semicircle-at x y inner top?)
        outer-ring (semicircle-at x y outer top?)
        ]
    (doto k
      (.append inner-ring true)
      (.append outer-ring true)                             
      (.closePath))))

(defn draw-ring!
  "Draws a black ring around a position"
  [loc]
  (println "drawing ring @" loc)
  (let [[x y] (loc-to-screenpx loc)
        s1 (semiring-at x y 15 30 true)
        s2 (semiring-at x y 15 30 false)]
    (doto game-graphics
      (.setColor java.awt.Color/BLACK)
      (.fill s1)
      (.fill s2))))

(defn draw-lines-at-loc!
  [loc]
  (doseq [line lines*]
    (when (on-line? loc line)
      (draw-line! line)))
  (when (some #(pt= loc %) rings*)
    (draw-ring! loc)))

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
  (draw-pieces-left! 1)
  (when (not= mode* :basic)
    (println "drawing gipfs...")
    (draw-gipfs-left! -1)
    (draw-gipfs-left! 1)))

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
  ;; undo line marks
  (println "empty-line" line)
  (let [e1p (pt- (second line) (third line))
        e2p (pt+ (get-line-limit-point (second line) (third line)) (third line)) 
        [e1x e1y] (loc-to-screenpx e1p)
        [e2x e2y] (loc-to-screenpx e2p)]
    (doto game-graphics
      (.setColor java.awt.Color/WHITE)
      (.setStroke (java.awt.BasicStroke. 15))
      (.drawLine e1x e1y e2x e2y)
      (.setStroke (java.awt.BasicStroke. 1)))
    (loop [cur e1p]
      (let [val (get-hex-array board* cur)]
        (when-not (or (= val 0) (protected? cur))
          (when (same-sign? val current-player*)
            (if (= 1 (abs val))
              (inc-pieces-left! current-player*)
              (inc-gipfs-left! current-player*)))
          (def board* (change-board-cell board* cur 0)))
        (redraw-loc! cur)
        (when (pt= cur hovered*)
          (draw-highlight! cur))
        (when-not (pt= cur e2p)
          (recur (pt+ cur (third line))))))))

(defn undraw-line!
  [line]
  (let [e1p (pt- (second line) (third line))
        e2p (pt+ (get-line-limit-point (second line) (third line)) (third line)) 
        [e1x e1y] (loc-to-screenpx e1p)
        [e2x e2y] (loc-to-screenpx e2p)]
    (doto game-graphics
      (.setColor java.awt.Color/WHITE)
      (.setStroke (java.awt.BasicStroke. 15))
      (.drawLine e1x e1y e2x e2y)
      (.setStroke (java.awt.BasicStroke. 1)))
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

(defn toggle-piece!
  "Toggle the state of the piece as or as not a GIPF-potential.
   Returns true if operation succeeded (enough pieces were owned)"
  [loc]
  (let [v (get-hex-array board* loc)
        target (if (odd? v) (* v 2) (/ v 2))]
    (println 'target target (+ -2 (* 2 (abs target)) (if (same-sign? v 1) 1 0)))
    (if (>  (get reserve-pieces* (+ -2 (* 2 (abs target)) (if (same-sign? v 1) 1 0))) 0)
      (do
        (if (= (abs target) 2)
          (do
            (inc-pieces-left! (sign v))
            (dec-gipfs-left! (sign v)))
          (do
            (inc-gipfs-left! (sign v))
            (dec-pieces-left! (sign v))))
        
        (def board* (change-board-cell board* loc target))        
        true)
      false

      )))

(defn move-piece!
  [loc shove]
  (let [[newboard updated] (do-move board* current-player* loc shove)]
    (def board* newboard)
    (redraw-loc-disk! (pt- loc shove))
    (doseq [p updated]
      (redraw-loc! p)
      (when (pt= hovered* p)
        (draw-highlight! hovered*)))))

(defn color-fade
  [color]
  (let [h (fn [v] (if (even? v) (+ 128 (/ v 2)) (+ 128 (/ (- v 1) 2))))]
    (java.awt.Color. (h (.getRed color)) (h (.getGreen color)) (h (.getBlue color)) 127)))

(defn draw-game-over!
  [winner]
  (let [wincolor (color-fade (get piece-colors (if (> winner 0) 0 1)))]
    (doto game-graphics
      (.setColor wincolor)
      (.fillRect 0 0 800 800))))

;; should really extract the (partial owns-line? player) pattern...
(defn owns-line?
  [player line]
  (same-sign? (first line) player))

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

(defn start-ai-move!
  []
  (let [b board*
        p current-player*
        rp reserve-pieces*]
    (def ai-action-thread*
      (start-thread
       (let [action (ai-move board* current-player* reserve-pieces*)]
         (on-swing-thread
          (update-game (list (cons :aimove action)))))))))

(defn switch-players!
  []
  (def current-player* (- current-player*)))

(defn set-lines!
  "Sets the lines and deals with the rings..."
  [all owner]
  (let [r (filter (partial owns-line? owner) all)]
    (def lines* r)
    (def rings* (reduce concat (map
                                (partial get-own-gipf-potentials-in-line board* owner)
                                r)))
    (draw-lines!)))

(defn update-game
  "Updates the game given list of inputs.
 Input is of the form [:mouse x y b] 
 [:state :new :basic/:advanced/:tournament].
 Returns if any one of the inputs changed something"
  [input-list]
  (doseq [input input-list]
    (when-not (= (first input) :hover)
      (println (first input) game-phase* (rest input) reserve-pieces*))
    (case (first input)
      :state
      (do
        (println input)
        (cond (= (second input) :new)
              (do
                (when-not (nil? ai-action-thread*)
                  (.interrupt ai-action-thread*))
                (def board* (new-board mode*))
                (def reserve-pieces* (new-reserves mode*))
                (def current-player* (- current-player*))
                (def selected* nil)
                (def lines* (list))
                (def rings* (list))
                (def game-phase* :placing)
                (redraw-all!))
              (= (second input) :basic)
              (def mode* :basic)
              (= (second input) :normal)
              (def mode* :normal)
              (= (second input) :advanced)
              (def mode* :advanced)
              ))
      
      :aimove
      (let [place (second input)
            shove (third input)
            shoveto (pt+ place shove)]
        ;; ai should call on swing thread
        
        (place-piece! place current-player*)
        (dec-pieces-left! current-player*)
        (move-piece! shoveto shove)
        (when (pt= place hovered*)
          (draw-highlight! hovered*))

        (let [found (get-lines-of-four board*)]
          (if (seq found)
            (if (some (partial owns-line? current-player*) found)
              (do
                (def removing-player* current-player*)
                (start-ai-clear! found))
              (do
                                        ; this is still the ai players turn; but none belong to it
                (def removing-player* (- current-player*))
                (set-lines! found removing-player*)
                (draw-lines!)
                (def game-phase* :removing-rows)))
            (do
              (def current-player* (- current-player*))
              (if (lost? board* reserve-pieces* current-player* mode*)
                (do (def game-phase* :gameover)
                    (draw-game-over! (- current-player*)))
                (def game-phase* :placing)))))
        (repaint!))
      
      
      ;; removing player is the AI;
      ;; if currentplayer is the same, removing player is finalizing its move;
      ;; otherwise, it is a reaction
      :aiclear
      (do
        (def rings* (third input))
        (empty-line! (second input))
        (def rings* (list))
        (let [found (get-lines-of-four board*)]
          ;; 
          (if (seq found)
            
            (if (some (partial owns-line? removing-player*) found)
              (do ;; AI continues
                (start-ai-clear! found))
              (do ;; nothing left for the ai; player is next
                (def game-phase* :removing-rows)
                (def removing-player* (- current-player*))
                (set-lines! found removing-player*)))
            
            ;; nothing left at all:
            (do
              (if (= removing-player* current-player*)
                (do ; ai is done; player has nothing to clear
                  (switch-players!)
                  (if (lost? board* reserve-pieces* current-player* mode*)
                    (do (def game-phase* :gameover)
                        (draw-game-over! (- current-player*)))
                    (def game-phase* :placing)))
                (do ; player had finished; ai is done clearing; on to its turn
                  (def game-phase* :waiting-for-ai)
                  (switch-players!)
                  (start-ai-move!)))))
          (repaint!)))


      :click
      (cond (= (nth input 3) 1)
            (let [clickpt (screenpx-to-loc (xy (second input) (third input)))]
              (let [rad (pt-radius clickpt)]
                (if (<= rad 4)
                  (case game-phase*
                    :placing
                                        ; place a piece
                    (when (and (= rad 4)
                               (> (get-pieces-left current-player*) 0)
                               (place-point-open? board* clickpt))
                      (place-piece! clickpt current-player*)
                      (dec-pieces-left! current-player*)
                      (def selected* clickpt)
                      (redraw-loc! clickpt)
                      (draw-highlight! clickpt)
                      (draw-selector! clickpt)
                      (repaint!)
                      (def game-phase* :moving))
                    :moving
                    (when (<= (pt-dist clickpt selected*) 1)
                      (cond (= rad 3)
                            (let [delvec (pt- clickpt selected*)]
                              ;; we have a valid move - if that row is clear
                              (when-not (row-full? board* clickpt delvec)
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
                                      (if (lost? board* reserve-pieces* current-player* mode*)
                                        (do (def game-phase* :gameover)
                                            (draw-game-over! (- current-player*)))
                                        (do
                                          (def game-phase* :waiting-for-ai)
                                          (start-ai-move!))))))
                                (when (not= game-phase* :gameover)
                                  (draw-highlight! clickpt))
                                (repaint!)))
                            (and (not= mode* :basic) (pt= clickpt selected*))
                            (do
                              (println "hit it!")
                              (when (toggle-piece! clickpt)
                                (redraw-loc! clickpt)
                                (draw-highlight! clickpt)
                                (draw-selector! clickpt)
                                (repaint!)
                                ))))
                    
                    :removing-rows
                    (cond (= 4 rad)
                          (do                  
                            (doseq [line lines*] 
                              (if (on-line? clickpt line)
                                (empty-line! line)
                                (undraw-line! line)))
                            
                            (let [found (get-lines-of-four board*)]
                              (set-lines! found removing-player*)
                              (if (seq found)
                                (if (seq lines*)
                                  ;; was human turn; it cleaned; now ai cleans
                                  (do ;; switch to ai removal
                                    (def game-phase* :waiting-for-ai)
                                    (def removing-player* (- current-player*))
                                    (start-ai-clear! found)))
                                (if (= current-player* removing-player*)
                                  ;; human player cleaned rest; ai turn
                                  (do 
                                    (switch-players!)
                                    (if (lost? board* reserve-pieces* current-player* mode*)
                                      (do (def game-phase* :gameover)
                                          (draw-game-over! (- current-player*)))
                                      (do
                                        (def game-phase* :waiting-for-ai)
                                        (start-ai-move!))))
                                  (do ;; ai turn is over now; human player cleaned up
                                    (switch-players!)
                                    (def game-phase* :placing))))
                              (repaint!)))
                          (= 2 (abs (get-hex-array board* clickpt)))
                          (do
                            (println "We hit a removable!")
                            (if (protected? clickpt)
                              (def rings* (remove #(pt= clickpt %) rings*))
                              (do
                                (def rings* (cons clickpt rings*))
                                (draw-ring! clickpt)))
                            (redraw-loc! clickpt)
                            (draw-lines-at-loc! clickpt)                     
                            (if (pt= clickpt hovered*)
                              (draw-highlight! hovered*)
                              )
                            (repaint!))
                          )
                          
                          
                    
                    ;; once chosen,set a flag, allow the ai to remove
                    
                    :waiting-for-ai
                    (do
                      (println "Waiting for AI" (get-hex-array board* clickpt)))
                    
                    :gameover
                    (println "Game is over; can't interact.")
                    
                    )))))
      :hover
      (let [hoverpt (screenpx-to-loc (xy (second input) (third input)))]
        (when (and (<= (pt-radius hoverpt) 4) (not= game-phase* :gameover))
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
                  (when (and (not (nil? selected*)) (<= (pt-dist hovered* selected*) 1)) 
                    (draw-selector! selected*))
                  (draw-lines-at-loc! hovered*)

                  (draw-highlight! hoverpt)
                  (repaint!)
                  (def hovered* hoverpt))))) 
      ))
  
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

    (redraw-all!)
    
    (.setSelected mode-basic true)
    
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
