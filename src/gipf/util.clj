(ns gipf.core) 

(defn clickableButton
  "Generates a button, that, when clicked, displays a popup with a message"
  [text displayed]
  (let [b (javax.swing.JButton. text)]
    (.addActionListener b
     (proxy [java.awt.event.ActionListener] []
       (actionPerformed [evt]
         (javax.swing.JOptionPane/showMessageDialog  nil,
            displayed))))
    b))

(defn set-on-button-select!
  "When button of a ButtonGroup is selected (state change), call thunk."
  [button thunk]
  (let [prev (atom (.isSelected button))] 
    (.addChangeListener button
      (proxy [javax.swing.event.ChangeListener] []
        (stateChanged [evt]
          (let [new-state (.isSelected button)]
            (when-not (= new-state @prev)
              (swap! prev (constantly new-state))
              (if new-state (thunk)))))))))

(defn set-on-button-click!
  "When button is clicked, funcall thunk"
  [button thunk]
  (.addActionListener button
    (proxy [java.awt.event.ActionListener] []
           (actionPerformed [evt]
             (thunk)))))


(defn make-img [x y]
  (java.awt.image.BufferedImage. x y java.awt.image.BufferedImage/TYPE_3BYTE_BGR))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn applyto-repeatedly
  [func obj & argsets]
  (if (empty? argsets)
       obj
       (recur func (apply func obj (first argsets)) (rest argsets))))

(defn mapvc "maps a function onto one or more vectors, passing in the location int as first arg" [func v & vecs]
  (into [] (apply map func (range) v vecs)))

(defn mapc "maps a function onto a single list, passing in the location int as first arg" [func list]
  (let [length (count vec)]
    (defn subrec [nv p source]
      (if (< p length)
        (recur (cons (func p (get source p)) nv) (+ p 1) source)
        nv))
    (subrec (list) 0 vec)))

(defn subtract-at "Decrease the value in a vector at position c by one." [vec c]
  (mapvc (fn [i v]
           (if (= i c)
             (- v 1)
             v))
    vec))

(defn getxy "Get value of 2d vector array at position (x, y)" [arr x y]
  (get (get arr x) y))
(defn getdxy [b pos]
  (getxy b (first pos) (second pos)))


(defn log
  "Takes, prints, and returns the single parameter."
  [v]
  (println v)
  v)



(defn copy-vector
  "Fills a vector of size n with an object.
 Does <i>not</i> deepcopy"
  [val size]
  ;; use alambda (w/ self call)
  ;; ex. (defn cv [val size] ((alambda [c b] (if (>= c size) b (self (+ c 1) (conj b (.clone val)))))))
  (defn yuck [c b]
    (if (>=  c size)
        b
        (recur (+ c 1) (conj b val))))
  (yuck 0 (vector)))

(defn merge-two
  "Merges two collections. Order may not be preserved."
  [alpha beta]
  (if (coll? alpha)
      (if (coll? beta)
          (if (empty? beta)
            alpha
            (apply conj alpha beta))
          (conj alpha beta))
      (if (coll? beta)
          (conj beta alpha)
          (list alpha beta))))

(defn combinations2
  "Returns a list of combinations of an iterable"
  [iter]
  (if (>= (count iter) 2)
    (do
      (defn subcombs [count cap arr itail pair]
        (if (or (empty? itail) (>= count cap))
          arr
          (recur (inc count) cap (cons (list pair (first itail)) arr) (rest itail) pair)))
      (reduce merge-two (mapvc
                           (fn [c pair] (subcombs 0 c (list) iter pair))
                           (vec iter))))
    '()))


(defn sleep
  [ms]
  (try
    (java.lang.Thread/sleep ms)
    (catch java.lang.InterruptedException bobby
      :ignore-bobby)))

(defn busy-doing-important-stuff
  "Feign activity"
  [sec]
  (println "Commencing...")
  (dotimes [k (int sec)]
    (println "Calculating...")
    (sleep 1000))
  (sleep (int (* 1000 (mod sec 1.0))))
  (println "Done!"))

(defn fill-vector [thunk size]
  ((fn [c b]
    (if (>=  c size)
        b
        (recur (+ c 1) (conj b (thunk)))))  0 (vector)))

(defn fill-list [thunk size]
  ((fn [c b]
    (if (>= c size)
        b
        (recur (+ c 1) (cons (thunk) b))))  0 (list)))

(defn vector-update [v pos func]
  (mapvc (fn [c val] (if (= c pos) (func val) val)) v))

(defn list-update [v pos func]
  (mapc (fn [c val] (if (= c pos) (func val) val)) v))



(defn flatten-level [structure level]
  (cond (<= level 0)
      structure
      (coll? structure)
      (reduce merge-two
        (map (fn [s] (flatten-level s (dec level)))
          structure))
      :else
      structure))

(defn rot90 "Rotates a vector by +90 degrees." [point]
  (list (- (second point)) (first point)))

(defn or-coll "Returns the first truthy value in the collection, else false." [coll]
  ;; aif
  (cond
      (empty? coll)
      false
      (first coll)
      (first coll)
      :else
      (recur (rest coll))))

(defn pairwise-and
  [func coll]
  (loop [r coll]
    (cond (empty? (rest r))
          true
          (not (func (first r) (second r)))
          false
          :else
          (recur (rest r)))))

(defn third [coll] (first (rest (rest coll))))
  

(defmacro start-thread
  "Starts a thread that does the operations
 specified in exprs. The thread object is bound to selfname.
 (proxy-super ...) still works.

 Example:

 (start-thread thread
   (busy-doing-important-stuff 2)
   (println \"Running under\" thread)
   (busy-doing-important-stuff 5)
 )
" 
  [& exprs]
  `(doto (proxy [java.lang.Thread] []
                    (run [] ~@exprs))
     (.start))) 

(defmacro
  on-swing-thread
  [& exprs]
  `(javax.swing.SwingUtilities/invokeLater
     (proxy [java.lang.Runnable] []
       (run [] ~@exprs))))

(defn atv
  [vect pos func]
  (mapvc
    (fn [c v]
      (if (= c pos)
        (func v)
        v))
    vect))

;;
;; Idea
;;
;;
;;  (let [b (parallel (heavycalc))]
;;    (local (needs-swing b)))
;;
;;