(ns gipf.core)
;; tools for hex geometry

;; see ?redblobgames; hex coordinate grids.
;; the best normalize is (= 0 (+ u v w)). distance is (reduce sum map abs map -)

(defrecord UV [^int u ^int v])
(defrecord XY [x y])


(defn xy
  [x y]
  "Create a cartesian coordinate pont.

    y
    |
    |
    X--x
"
  (list x y))

;; or use deftype? since pt= != plain =, and we want those to be true..
(defn pt
  "Create a hexagonal coordinate point.


      u
      |  v
      | / 
      |/
      X
       \\
        \\
         w
"
  [^long u ^long v ^long w]
  (UV. (- u w) (+ v w)))

(def xy-u (xy 0 -1))
(def xy-v (xy (/ (Math/sqrt 3) 2) -0.5))
(def xy-w (xy (/ (Math/sqrt 3) 2) 0.5))

(defn pt=
  "Are two points equal?"
  [^UV pta ^UV ptb]
  (= pta ptb))


(defn getU [pt] (:u pt))
(defn getV [pt] (:v pt))

(defn pt+
  ([] (UV. 0 0))
  ([^UV pt] pt)
  ([^UV a ^UV b] (UV. (unchecked-add (:u a) (:u b))
              (unchecked-add (:v a) (:v b))))
  ([^UV a ^UV b & pts]
     ;; need some optimized boolean reduce.... Check core.reducers.
      (UV. (apply + (:u a) (:u b) (map getU pts))
           (apply + (:v a) (:v b) (map getV pts)))))

(defn pt-
  ([] (UV. 0 0))
  ([^UV p] (UV. (unchecked-negate (:u p)) (unchecked-negate (:v p))))
  ([^UV p ^UV q] (UV. (unchecked-subtract (:u p) (:u q))
              (unchecked-subtract (:v p) (:v q))))
  ([^UV p ^UV q & rest] (pt- p (apply pt+ q rest))))

(defn pt* [factor ^UV p]
  (UV. (* factor (:u p)) (* factor (:v p))))

(defn xy+ [& xys]
  (apply map + xys))
(defn xy- [& xys]
  (apply map - xys))
(defn xy* [factor xy]
  (map * (repeat factor) xy))

(defn pt->xy
  "Converts a pt-point to a xy-point."
  [^UV pt]
  (xy+ (xy* (:u pt) xy-u) (xy* (:v pt) xy-v)))

;; converts to a (u v 0). We coerce coords to int
(defn xy->pt-int
  "Converts an xy-point to a pt-point, with coords rounded to nearest int."
  [z]
  (pt (round-int (- (/ (first z) 2) (second z))) 0 (round-int (* (first z) sqrt3o3t2))))

(defn interpolate-list
  [^UV point-a ^UV point-b ^long count]
  (let [delta (pt* (/ count) (pt- point-b point-a))]
    (loop [made (list) c 0]
      (if (= c count)
        made
        (recur (cons (pt+ point-a (pt* c delta)) made) (inc c))))))

(def pt-origin (UV. 0 0))

;; Use testing implies that 9 is the greatest radius output
;; for pt-radius. For safety, we test on 6

(defmacro pt-lookup-table-optimize
  "Optimizing macro. Read the source."
  [srd func]
  `(let [qrd# (int ~srd)
         qrn# (int (- ~srd))
         qrl# (int (+ 1 (* 2 ~srd)))
         qra# (int (* (inc qrl#) qrd#))
         table# (make-array java.lang.Long/TYPE (* qrl# qrl#))]

     (prep-table table# qrn# qrd# qra# qrl# ~func)
     
     (fn [^UV p#]
       (aget-long table# ;(.clojure.lang.Numbers longs table#)
             (unchecked-add
              (unchecked-add (:v p#) qra#)
              (unchecked-multiply (:u p#) qrl#))))))


(def ptrl-table-sqrd (int 9))
(def neg-ptrl-table-sqrd (int (- ptrl-table-sqrd)))
(def ptrl-table-span (int (inc (* 2 ptrl-table-sqrd))))
(def ptrl-table-add (int (* (inc ptrl-table-span) ptrl-table-sqrd)))

(def pt-radius-slow
  (fn [^UV p]
    (if (pt= p pt-origin)
      0
      (let [u (:u p)
            v (:v p)]
        (cond (= u v 0) 0
              (and (= v 0) (> u 0)) u
              (and (= u (- v)) (> u 0)) u
              (and (= u 0) (< v 0))  (- v)
              (and (= v 0) (< u 0))  (- u)
              (and (= (- u) v) (< u 0)) v
              (and (= u 0) (> v 0)) v
              
              (> u (- v) 0)          u
              (> (- v) u 0)       (- v)
              (and (< u 0) (< v 0))      (- (+ u v))
              (> (- u) v 0)      (- u)
              (> v (- u) 0)          v
              (and (> u 0) (> v 0))   (+ u v))))))

(defn make-ptrl-table
  "Returns an array of values"
  [sqrd]
  (let [ra (make-array java.lang.Long/TYPE (* ptrl-table-span ptrl-table-span))]
    (doseq [u (range neg-ptrl-table-sqrd (inc ptrl-table-sqrd) 1)
            v (range neg-ptrl-table-sqrd (inc ptrl-table-sqrd) 1)]
      (aset-long ra
                 (+ (+ v ptrl-table-add) 
                    (* u ptrl-table-span))
                 (pt-radius-slow (UV. u v)))
      )
    ra))

(def ptrl-table
  (make-ptrl-table ptrl-table-sqrd))

;; still just as fast as aget..
(defn pt-radius-fast1
  "somehow this is much faster than the
 (should be) equivalent pt-radius-fast2"
  [^UV p]
  (aget ^longs ptrl-table
        (unchecked-add
         (unchecked-add (:v p) ptrl-table-add)
         (unchecked-multiply (:u p) ptrl-table-span))) )

(def pt-radius-fast2
  (pt-lookup-table-optimize
   9
   pt-radius-slow))

(def pt-radius-memo
  (memoize pt-radius-slow))

(def pt-radius pt-radius-fast1)

(def pt-dist
  (fn
    [^UV pta ^UV  ptb]
    (pt-radius (pt- pta ptb))))

(defn pt-rot+60
  [^UV p]
  (pt (:v p) 0 (- (:u p))))

(defn pt-rot-60
  [^UV p]
  (pt 0 (:u p) (:v p)))
  
  
  
  
  
  
  
  
  


