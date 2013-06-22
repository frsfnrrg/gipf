(ns gipf.core)

;;; POINTS

(definline pt-radius [a] `( Geometry/pradius ~a))
(definline pt-neg [a]`( Geometry/pnegate ~a))
(definline pt-div-4 [a]`( Geometry/pdivide4 ~a))
(definline pt-rot+60 [a]`( Geometry/protp60 ~a))
(definline pt-rot-60 [a]`( Geometry/protm60 ~a))

(definline pt-sub [a b]`( Geometry/psubtract ~a ~b))
(definline pt= [a b]`( Geometry/pequals ~a ~b))
(definline pt-dist [a b]`( Geometry/pdistance ~a ~b))
(definline pt+ [a b]`( Geometry/padd ~a ~b))
(definline pt* [a b]`( Geometry/pmultiply ~a ~b))
(definline pt-div [a b]`( Geometry/pdivide ~a ~b))

(definline pt [a b c]`( Geometry/pmakePt ~a ~b ~c))

(defn pt-
  ([p] (pt-neg p))
  ([p q] (pt-sub p q)))

(defn xy [x y]
  (list x y))
(defn xy+ [& xys]
  (apply map + xys))
(defn xy- [& xys]
  (apply map - xys))
(defn xy* [factor xy]
  (map * (repeat factor) xy))

(defn xyc->xyl
  [^gipfj.XYPoint xyc]
  (list (.getX xyc) (.getY xyc)))

(defn xyl->xyc
  [xyl]
  (gipfj.XYPoint. (first xyl) (second xyl)))

(defn pt->xy
  [p]
  (xyc->xyl (Geometry/convertPtToXY p)))

(defn xy->pt-int
  [z]
  (Geometry/convertXYToPt (xyl->xyc z)))

(defn interpolate-list
  [point-a point-b ^long count]
  (let [delta (pt-div count (pt- point-b point-a))]
    (loop [made (list) c 0]
      (if (= c count)
        made
        (recur (cons (pt+ point-a (pt* c delta)) made) (inc c))))))

;;; LINES

(definline ->SignedLine [a b c] `(Line/makeSignedLine ~a ~b ~c))

(definline get-line-limit-point [a b] `( Geometry/lend ~a ~b))
(definline sign-line [a b] `( Line/sign ~a ~b))
(definline ->Line [a b] `( Line/makeLine ~a ~b))
(definline on-line? [a b] `( Line/onLine ~a ~b))
(definline line= [a b] `( Line/equals ~a ~b))

(definline line-start [a]  `( Line/getStart ~a))
(definline line-delta [a]`( Line/getDelta ~a))
(definline line-sig [a]`( Line/getSig ~a))
(definline advance-line [a]`( Line/advanceLine ~a))

;;; BOARD

(definline change-hex-array [a b c] `(Board/change ~a ~b ~c))

(definline get-hex-array [a b] `( Board/get ~a ~b))
(definline count-over-hex-array [a b] `( Board/countItem ~a ~b))

(definline make-hex-array [] `(Board/makeBoard))

(def unit-ring-points-cycled
  (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1) (pt -1 0 0) (pt 0 -1 0) (pt 0 0 -1) (pt 1 0 0)))

(def unit-ring-points
  (list (pt 1 0 0)
        (pt 0 1 0)
        (pt 0 0 1)
        (pt -1 0 0)
        (pt 0 -1 0)
        (pt 0 0 -1)))

(defn
  get-ring-of-hex-uv-points
  [^long radius]
  
  (if (= 0 radius)
    (list (pt 0 0 0))
  
    (loop [made (list) pointsleft unit-ring-points-cycled]
      (if (empty? (rest pointsleft))
        made
        (recur (concat made
                       (reverse (interpolate-list
                                 (pt* radius (first pointsleft))
                                 (pt* radius (second pointsleft))
                                 radius)) )
               (rest pointsleft))))))

