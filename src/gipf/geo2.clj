(ns gipf.core
  (:import (gipfj Geometry MathUtil)))

;;; POINTS

(Geometry/loadTables)

;; I wish I could remove the numargs...
(defmacro defrename
  [new old numargs]
  (let [args (map #(gensym (str "expr" %)) (range numargs))]
  `(defmacro ~new [~@args]
     `(~~old ~~@args))))

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

(defrecord SignedLine [^long sig ^long start ^long delta])
(defrecord Line [^long start ^long delta])

(defn on-line?
  [loc line]
  (let [delta (pt- loc (:start line))
        dist (pt-radius delta)
        approx (pt* dist (:delta line))] 
    (or (pt= delta approx)
      (pt= delta (pt- approx)))))

(defn line=
  [linea lineb]
  (and (on-line? (:start linea) lineb)
       (or (pt= (:delta linea) (:delta lineb))
           (pt= (pt- (:delta linea)) (:delta lineb)))))

(defrename get-line-limit-point `Geometry/lend 2)