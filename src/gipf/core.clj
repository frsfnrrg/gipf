(ns gipf.core
  (:gen-class))

(load "math")

(load "util")
(load "geo")
(load "game")

;; constants

(def piece-colors (vector java.awt.Color/RED java.awt.Color/CYAN))
(def segment-length 76) ; px
(def board-center (xy 400 400)) ; px
(def color-bg (new java.awt.Color 255 255 255))

;; "things"

;; logical stuff needs thread-independence
(def mode (ref :basic))
(def board (ref (new-board)))
(def reserve-pieces (ref (new-reserves)))
(def current-player (ref (- (* 2 (rand-int 2)) 1))) ; -1 or 1
(def removing-player nil)

;; but this is all swing-thread reactive
(def game-img (make-img 800 800))
(def game-graphics (.getGraphics game-img))
(def selected nil)
(def hovered nil)
(def lines (list))
(def panel nil)
(def game-phase :placing)

(defn repaint!
  []
  (.repaint panel))


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
  (let [[cx cy] (xy+ board-center
                  (xy* segment-length
                    (pt->xy p)))
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
  (get @reserve-pieces (if (= player -1) 0 1)))
  
(defn dec-pieces-left!
  [player]
  (let [n (atv @reserve-pieces (if (= player -1) 0 1) dec)]
    (dosync (ref-set reserve-pieces n))))

(defn inc-pieces-left!
  [player]
  (let [n (atv @reserve-pieces (if (= player -1) 0 1) inc)]
    (dosync (ref-set reserve-pieces n))))

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

(defn draw-lines-at-loc!
  [loc]
  (doseq [line lines]
    (when (on-line? loc line)
      (draw-line! line))))
  
(defn draw-lines!
  []
  (doseq [line lines]
    (draw-line! line)))

(defn redraw-loc!
  [loc]
  (.setColor game-graphics java.awt.Color/WHITE)
  (.fill game-graphics (hex-tile-at loc))
  (.setColor game-graphics java.awt.Color/BLACK)
  (let [f (get-hex-array @board loc)]
    (if (= 0 f)
      (if (= 4 (pt-radius loc))
        (.fill game-graphics (circle-at loc 10))
        (.fill game-graphics (circle-at loc 5)))
      (do
        (if (> f 0)
            (.setColor game-graphics (get piece-colors 0))
            (.setColor game-graphics (get piece-colors 1)))
        (.fill game-graphics (circle-at loc 30))))))
 

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

(def edge-points  (get-ring-of-hex-uv-points 4))
(def center-points (concat (list (pt 0 0 0)) (get-ring-of-hex-uv-points 1) (get-ring-of-hex-uv-points 2) (get-ring-of-hex-uv-points 3)))

(defn draw-base!
  []
  (.setColor game-graphics java.awt.Color/WHITE)
  (.fillRect game-graphics 0 0 800 800)
  (.setColor game-graphics java.awt.Color/BLACK)
  
  (doseq [p (apply concat (map get-ring-of-hex-uv-points (range 5)))]
    (redraw-loc! p)))

(defn redraw-all!
  []
  (draw-base!)
  (if selected
      (draw-highlight! selected true))
  (repaint!))

(defn empty-line!
  [line]
  ;; undo line marks
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
      (let [val (get-hex-array @board cur)]
        (when-not (= val 0)
          (when (= val @current-player)
            (inc-pieces-left! @current-player))
          (dosync (ref-set board (change-board-cell @board cur 0))))
        (redraw-loc! cur)
        (when (pt= cur hovered)
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
      (when (pt= cur hovered)
        (draw-highlight! cur))
      (when-not (pt= cur e2p)
        (recur (pt+ cur (third line)))))))

(defn place-piece!
  [loc player]
  (let [newboard (map-hex-array (fn [p c] (if (pt= p loc) player c)) @board)]
    (dosync (ref-set board newboard))))

(defn move-piece!
  [loc shove]
  (let [[newboard updated] (do-move @board @current-player loc shove)]
    (dosync (ref-set board newboard))
    (redraw-loc-disk! (pt- loc shove))
    (doseq [p updated]
      (redraw-loc! p)
      (when (pt= hovered p)
        (draw-highlight! hovered)))))

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

;; the selector:
;;
;;  -ring on edge of piece. Arrows pointing in various directions.
;;  -redraw if on selector arrow, or dist <= 1
;;
;;
;;


(defn update-game
  "Updates the game given list of inputs.
 Input is of the form [:mouse x y b] 
 [:state :new :basic/:advanced/:tournament].
 Returns if any one of the inputs changed something"
  [input-list]
  (doseq [input input-list]
    (when-not (= (first input) :hover)
      (println (first input) game-phase (rest input) @reserve-pieces))
    (case (first input)
      :state
      (do
        (println input)
        (cond (= (second input) :new)
          (let [nb (new-board)
                nr (new-reserves)
                np (- @current-player)]
            (dosync (ref-set board nb)
              (ref-set reserve-pieces nr)
              (ref-set current-player np))
            (def selected nil)
            (def lines (list))
            (def game-phase :placing)
            (redraw-all!))
        ))
      
      :aimove
      (let [place (second input)
            shove (third input)
            shoveto (pt+ place shove)]
        ;; ai should call on swing thread
        (place-piece! place @current-player)
        (dec-pieces-left! @current-player)
        (move-piece! shoveto shove)

        (let [found (get-lines-of-four @board)]
          (if (seq found)
            (if (some #(= (first %) @current-player) found)
              (do
                (def removing-player @current-player)
                (start-thread
                  (let [action (ai-clear @board removing-player found)]
                    (on-swing-thread
                      (update-game 
                        (list (cons :aiclear action)))))))
              (do
                ; this is still the ai players turn; but none belong to it
                (def removing-player (- @current-player))
                (def lines (filter #(= (first %) removing-player) found))
                (draw-lines!)
                (def game-phase :removing-rows)))
            (do
              (dosync (ref-set current-player (- @current-player)))
              (if (= (get-pieces-left @current-player) 0)
                  (do (def game-phase :gameover)
                    (draw-game-over! (- @current-player))
                    
                    )
                  (def game-phase :placing)))))
        (repaint!))
      
      
      ;; removing player is the AI;
      ;; if currentplayer is the same, removing player is finalizing its move;
      ;; otherwise, it is a reaction
      :aiclear
      (do
        (println "AI CLEAR" input)
        (empty-line! (rest input))
        (let [found (get-lines-of-four @board)]
          ;; 
          (if (seq found)
            
            (if (some #(= (first %) removing-player) found)
              (do ;; AI continues
                (println lines "should be empty")
                (start-thread
                  (let [action (ai-clear @board @current-player found)]
                    (on-swing-thread
                      (update-game 
                        (list (cons :aiclear action)))))))
              (do ;; nothing left for the ai; player is next
                (def game-phase :removing-rows)
                (def removing-player (- @current-player))
                (def lines (filter #(= (first %) removing-player)))
                (draw-lines!)))
            
            ;; nothing left at all:
            (do
              (println "AI is done clearing;" @current-player removing-player)
              
              (if (= removing-player @current-player)
                (do ; ai is done; player has nothing to clear
                  (dosync (ref-set current-player (- @current-player)))
                  (if (= (get-pieces-left @current-player) 0)
                    (do (def game-phase :gameover)
                      (draw-game-over! (- @current-player)))
                    (def game-phase :placing)))
                (do ; player had finished; ai is done clearing; on to its turn
                  (def game-phase :waiting-for-ai)
                  (dosync (ref-set current-player (- @current-player)))
                  (start-thread
                    (let [action (ai-move @board @current-player @reserve-pieces)]
                      (on-swing-thread
                        (update-game 
                          (list (cons :aimove action))))))))))
          (repaint!)))


      :click
      (cond (= (nth input 3) 1)
        (let [clickpt (screenpx-to-loc (xy (second input) (third input)))]
          (let [rad (pt-radius clickpt)]
            (if (<= rad 4)
              (case game-phase
                :placing
                ; place a piece
                (when (and (= rad 4)
                      (> (get-pieces-left @current-player) 0)
                      (place-point-open? @board clickpt))
                  (println "placing piece") 
                  (place-piece! clickpt @current-player)
                  (dec-pieces-left! @current-player)
                  (def selected clickpt)
                  (redraw-loc! clickpt)
                  (draw-highlight! clickpt)
                  (draw-selector! clickpt)
                  (repaint!)
                  (def game-phase :moving))
                :moving
                (when (and (= (pt-dist clickpt selected) 1)
                        (= (pt-radius clickpt) 3))
                  (let [delvec (pt- clickpt selected)]
                    ;; we have a valid move - if that row is clear
                    (when-not (row-full? @board clickpt delvec)
                      (move-piece! clickpt delvec)
                      (def selected nil)

                      (let [found (get-lines-of-four @board)]
                        (if (seq found)
                          (if (some #(= (first %) @current-player) found)
                            (do ; self has stuff to remove first
                              (def game-phase :removing-rows)
                              (println "clearing own lines" found)
                              (def removing-player @current-player)
                              (def lines (filter #(= (first %) removing-player) found))
                              (draw-lines!))
                            (do ; only the ai is clearing
                              (def game-phase :waiting-for-ai)
                              (println "only ai lines to clear" found)
                              (def removing-player (- @current-player))
                              (start-thread
                                (let [action (ai-clear @board removing-player found)]
                                  (on-swing-thread
                                    (update-game 
                                      (list (cons :aiclear action))))))))
                          
                          (do 
                            (dosync (ref-set current-player (- @current-player)))
                            (if (= (get-pieces-left @current-player) 0)
                              (do (def game-phase :gameover)
                                (draw-game-over! (- @current-player)))
                              (do
                                (def game-phase :waiting-for-ai)
                                (start-thread
                                  (let [action (ai-move @board @current-player @reserve-pieces)]
                                    (on-swing-thread
                                      (update-game 
                                        (list (cons :aimove action)))))))))))
                      (draw-highlight! clickpt)
                      (repaint!))))
                
                :removing-rows
                (when (= 4 (pt-radius clickpt))
                  (doseq [line lines] 
                    (if (on-line? clickpt line)
                      (empty-line! line)
                      (undraw-line! line)))
                      
                  (let [found (get-lines-of-four @board)]
                    (println 'f found removing-player)
                    (def lines (filter #(= (first %) removing-player) found))
                    (if (seq found)
                      (if (seq lines)
                        ;; still some lines left
                        (draw-lines!)
                        ;; was human turn; it cleaned; now ai cleans
                        (do ;; switch to ai removal
                          (def game-phase :waiting-for-ai)
                          (println "only ai lines to clear" found)
                          (def removing-player (- @current-player))
                          (start-thread
                            (let [action (ai-clear @board removing-player found)]
                              (on-swing-thread
                                (update-game 
                                  (list (cons :aiclear action))))))))
                      (if (= @current-player removing-player)
                        ;; human player cleaned rest; ai turn
                        (do 

                          (println "nothing left: see" found)
                          (dosync (ref-set current-player (- @current-player)))
                          (if (= (get-pieces-left @current-player) 0)
                            (do (def game-phase :gameover)
                              (draw-game-over! (- @current-player)))
                            (do
                              (def game-phase :waiting-for-ai)
                              (start-thread
                                (let [action (ai-move @board @current-player @reserve-pieces)]
                                  (on-swing-thread
                                    (update-game (list (cons :aimove action)))))))))
                        (do ;; ai turn is over now; human player cleaned up
                          (dosync (ref-set current-player (- @current-player)))
                          (def game-phase :placing))))
                    (repaint!)))
                
                ;; once chosen,set a flag, allow the ai to remove
                
                :waiting-for-ai
                (do

                  
                  (println "Waiting for AI" (get-hex-array @board clickpt))
                  
                  )
                
                :gameover
                (println "Game is over; can't interact.")
            
            )))))
      :hover
      (let [hoverpt (screenpx-to-loc (xy (second input) (third input)))]
        (if (and (<= (pt-radius hoverpt) 4) (not= game-phase :gameover))
          (cond (nil? hovered)
                (do
                  (draw-highlight! hoverpt)
                  (repaint!)
                  (def hovered hoverpt))
                (pt= hoverpt hovered)
                :nothing-to-do-here
                :else
                (do
                  (redraw-loc! hovered)
                  (when (and (not (nil? selected)) (<= (pt-dist hovered selected) 1)) 
                        (draw-selector! selected))
                  (draw-lines-at-loc! hovered)

                  (draw-highlight! hoverpt)
                  (repaint!)
                  (def hovered hoverpt)))
          ;; highlight is tracked)
          :nothing-to-do-here)) 
      ))
        
      true)

;; I really, really like the button approach of quad2.

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
    
    (def panel (proxy [javax.swing.JPanel] []
                 (paint [^java.awt.Graphics g]
                   (.drawImage g game-img 0 0 window))))
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
    
    (doto panel
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
      (.setContentPane panel)
      (.pack)
      (.setResizable false)
      (.setVisible true))
    
    true))
