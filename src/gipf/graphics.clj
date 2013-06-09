(ns gipf.core)

;; graphics. here go all painting things; the panel, repaint, blah
;; blah blah. No coordination here

; SETUP

(def piece-colors (vector (java.awt.Color. 255 0 0) (java.awt.Color. 0 200 200)))
(def segment-length 76) ; px
(def board-center (xy 400 400)) ; px
(def color-bg (java.awt.Color. 255 255 255))
(def numfont (java.awt.Font. "Angleterre Book" java.awt.Font/PLAIN 70))

(def game-img (make-img 800 800))
(def game-graphics (.getGraphics game-img))
(def game-panel (proxy [javax.swing.JPanel] []
                  (paint [^java.awt.Graphics g]
                    (.drawImage g game-img 0 0 game-panel))))

(defn repaint!
  []
  (.repaint game-panel))


;; DRAWING STUFF

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


(defn draw-text-centered-at!
  [xy text color]
  ;; horizontally, it is fine.... The y coord corresponds to the 
  (let [[x y] xy
        fmetric (.getFontMetrics game-graphics)
        b (.getStringBounds fmetric text game-graphics)
        [hx hy] (map #(int (/ % 2)) [(.getWidth b) (.getHeight b)])]
    (doto game-graphics
      (.setColor color-bg)
      ;; 4x; we will never drop more than 1 digit...
      (.fillRect
       (- x (* 2 hx)) (+ y (int (.getY b)))
       (* 4 hx) (int  (.getHeight b)))
      (.setColor color)
      (.drawString text (- x hx) y))))

(defn draw-highlight!
  [loc]
  (.setColor game-graphics (java.awt.Color. 255 255 0 200))
  (.fill game-graphics (hex-tile-at loc)))

(defn draw-selector!
  [loc board]
  ;; TODO make there be arrows, pointing only
  ;; in the available directions
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

(defn clear-line!
  "Undo line marks"
  [p1 p2]
  (let [[e1x e1y] (loc-to-screenpx p1)
        [e2x e2y] (loc-to-screenpx p2)]
    (doto game-graphics
      (.setColor java.awt.Color/WHITE)
      (.setStroke (java.awt.BasicStroke. 15))
      (.drawLine e1x e1y e2x e2y)
      (.setStroke (java.awt.BasicStroke. 1)))))


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
