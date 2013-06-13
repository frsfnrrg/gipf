(ns gipf.core)

(defrecord SignedLine [^long sig ^UV start ^UV delta])
(defrecord Line [^UV start ^UV delta])

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

