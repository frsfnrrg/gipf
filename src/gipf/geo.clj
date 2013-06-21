(ns gipf.core
  (:import (gipfj Geometry MathUtil Line)))

;;; POINTS

(defrename pt-radius `Geometry/pradius 1)
(defrename pt= `Geometry/pequals 2)
(defrename pt-dist `Geometry/pdistance 2)
(defrename pt-neg `Geometry/pnegate 1)
(defrename pt-sub `Geometry/psubtract 2)
(defrename pt+ `Geometry/padd 2)
(defrename pt* `Geometry/pmultiply 2)
(defrename pt-div `Geometry/pdivide 2)
(defrename pt-div-4 `Geometry/pdivide4 1)
(defrename pt-rot+60 `Geometry/protp60 1)
(defrename pt-rot-60 `Geometry/protm60 1)
(defrename pt `Geometry/pmakePt 3)

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

(defrename ->Line `Line/makeLine 2)
(defrename ->SignedLine `Line/makeSignedLine 3)
(defrename on-line? `Line/onLine 2)
(defrename line= `Line/equals 2)
(defrename line-start `Line/getStart 1)
(defrename line-delta `Line/getDelta 1)
(defrename line-sig `Line/getSig 1)

(defrename get-line-limit-point `Geometry/lend 2)

(defrename advance-line `Line/advanceLine 1)
(defrename sign-line `Line/sign 2)

(defrename make-hex-array `Board/makeBoard 0)
(defrename get-hex-array `Board/get 2)
(defrename change-hex-array `Board/change 3)
(defrename count-over-hex-array `Board/countItem 2)

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

