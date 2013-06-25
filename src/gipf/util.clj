(ns gipf.core) 

(defn clickableButton
  "Generates a button, that, when clicked, displays a popup with a message"
  [text displayed]
  (let [b (javax.swing.JButton. ^java.lang.String text)]
    (.addActionListener b
                        (proxy [java.awt.event.ActionListener] []
                          (actionPerformed [evt]
                            (javax.swing.JOptionPane/showMessageDialog  nil,
                                                                        displayed))))
    b))

;; eq to (set-on-state-change! b (fn [s] (when s (thunk))))
(defn set-on-button-select!
  "When button of a ButtonGroup is selected (state change), call thunk."
  [^javax.swing.AbstractButton button thunk]
  (let [prev (atom (.isSelected button))] 
    (.addChangeListener button
                        (proxy [javax.swing.event.ChangeListener] []
                          (stateChanged [^javax.swing.event.ChangeEvent evt]
                            (let [new-state (.isSelected button)]
                              (when-not (= new-state @prev)
                                (swap! prev (constantly new-state))
                                (if new-state (thunk)))))))))

(defn set-on-state-change!
  "When a checkbox is selected or deselected, call funk of pressedness."
  [^javax.swing.AbstractButton checkbox func]
  (let [prev (atom (.isSelected checkbox))] 
    (.addChangeListener checkbox
                        (proxy [javax.swing.event.ChangeListener] []
                          (stateChanged [evt]
                            (let [new-state (.isSelected checkbox)]
                              (when-not (= new-state @prev)
                                (swap! prev (constantly new-state))
                                (func new-state))))))))

(defn set-on-button-click!
  "When button is clicked, funcall thunk"
  [^javax.swing.AbstractButton button thunk]
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


(defn from-iterator
  [^java.util.Iterator i]
  (if (.hasNext i)
    (cons (.next i) (lazy-seq (from-iterator i)))))


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

(def silent** true)
(defn busy-doing-important-stuff
  "Feign activity"
  [sec]
  (when-not silent**
    (println "Commencing..."))
  (dotimes [k (int sec)]
    (when-not silent**
      (println "Calculating..."))
    (sleep 1000))
  (sleep (int (* 1000 (mod sec 1.0))))
  (when-not silent**
    (println "Done!")))

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
(defn fourth [coll] (first (rest (rest (rest coll)))))

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

(defn rand-best
  "Takes a function returning a number;
 a default value to return, and a collection
 to apply the function to. If the function's
 value is maximized at an element, return that element."
  [test default-obj default-val coll disp]
  (let [best
        (loop [rcoll coll recv default-val reco (list default-obj)]
          (if (empty? rcoll)
            reco
            (let [f (first rcoll)
                  t (test f)]
              (cond (> t recv)
                    (recur (rest rcoll) t (list f))
                    (= t recv)
                    (recur (rest rcoll) t (cons f reco))
                    :else
                    (recur (rest rcoll) recv reco)))))]
    (when disp
      (println (count best)))
    (rand-nth best)))

(defn same-sign?
  "Do all arguments have the same sign? (+,0,-)"
  [a b]
  (or (and (pos? a) (pos? b))
      (and (zero? a) (zero? b))
      (and (neg? a) (neg? b))))

(defn different-sign?
  [a b]
  (neg? (unchecked-multiply a b)))

(defn color->list
  [^java.awt.Color c]
  (list (.getRed c) (.getBlue c) (.getGreen c)))

(defn list->color
  [l]
  (java.awt.Color. (int (first l)) (int (second l)) (int (third l))))

(defn scale-color
  [col v]
  (list->color (map int (map #(* v %1) (color->list col)))))

(defn summap
  [func & colls]
  (reduce + (apply map func colls)))

(defn microbench
  [func iters]
  (time (doseq [i (range iters)]
          (func))))

(defn mbench
  [func iters]
  (let [r1 (. System (nanoTime))
        n (fn [])]
    (doseq [i (range iters)] (func))
    (let [r2 (. System (nanoTime))]
      (doseq [i (range iters)] (n))
      (let [r3 (. System (nanoTime))]
        (print (str "Elapsed time: " (/ (double (- (- r2 r1) (- r3 r2))) 1000000.0) " msecs" ))))))


(defn print-max-output
  [func]
  (let [max (atom 0)]
    (fn [& args]
      (let [r (apply func args)]
        (when (> r @max)
          (swap! max (constantly r))
          (println r))
        r))))

(defn logf
  [func]
  (fn [& args]
    (log (apply func args))))

(defn increment-count
  [func]
  (let [num (atom 0)]
    (fn [& args]
      (swap! num (constantly (inc @num)))
      (println @num)
      (apply func args))))


(defn array?
  "See http://clj-me.cgrand.net/2009/10/15/multidim-arrays/ for source."
  [x]
  (-> x class .isArray))
(defn see
  "See http://clj-me.cgrand.net/2009/10/15/multidim-arrays/ for source."
  [x]
  (if (array? x) (map see x) x))

(defmacro deep-aget
  "See http://clj-me.cgrand.net/2009/10/15/multidim-arrays/ for source."
  ([hint array idx]
     `(aget ~(vary-meta array assoc :tag hint) ~idx))
  ([hint array idx & idxs]
     `(let [a# (aget ~(vary-meta array assoc :tag 'objects) ~idx)]
        (deep-aget ~hint a# ~@idxs))))

(defmacro deep-aset [hint array & idxsv]
  "See http://clj-me.cgrand.net/2009/10/15/multidim-arrays/ for source."
  (let [hints '{doubles double ints int longs long} ; writing a comprehensive map is left as an exercise to the reader
        [v idx & sxdi] (reverse idxsv)
        idxs (reverse sxdi)
        v (if-let [h (hints hint)] (list h v) v)
        nested-array (if (seq idxs)
                       `(deep-aget ~'objects ~array ~@idxs)
                       array)
        a-sym (with-meta (gensym "a") {:tag hint})]
    `(let [~a-sym ~nested-array]
       (aset ~a-sym ~idx ~v))))

(defmacro log-action
  [expr]
  (let [funcname (first expr)]
    `(let [q# ~expr]
       (println ": " '~funcname)
       q#)))

(defmacro expand
  " Example time!

 >> (expand (juxt square cube triple) (range 2))

 (0 0 0 1 1 3)
 
 >> (expand #(range %) [1 3 5])

 (0 0 1 2 0 1 2 3 4)

 >> (for-all-funcs-and-seqs? func seq
      (= (expand func seq) (reduce concat (map func seq))))

 true
"
  [func seq]
  `(reduce concat (map ~func ~seq)))

(defmacro msquare
  [expr]
  `(let [b# ~expr]
     (multiply b# b#)))

(defmacro fastempty?
  "Assumes nil is not a legal first arg"
  [seq]
  `(nil? (first ~seq)))


(def timing** false)
(defmacro
  timec
  [expr]
  (if timing**
    `(time ~expr)
    expr))

(defmacro
  timev
  [expr y]
  `(if ~y (time ~expr) ~expr))


(defmacro past-time?
  [time]
  `(let [newtime# (System/nanoTime)]
     (greater newtime# ~time)))

(defn nested-map
  [func coll]
  ((if (vector? coll)
     mapv
     map)
   (fn [elem] (if (coll? elem)
                   (nested-map func elem)
                   (func elem)))
       coll))

(defn alternating
  [& colls]
  (let [size (count colls)]
    (loop [built [] ind (long 0) rugula (vec colls)]
      (let [t (get rugula ind)]
        (if (empty? t)
          built
          (let [ick (let [ikk (inc-1 ind)]
                      (if (= ikk size)
                        0
                        ikk))]
            (recur (conj built (first t))
                   ick
                   (assoc rugula ind (rest t)))))))))

(defn cph
  "Return the "
  [ind reps target]
  (let [repmap (apply hash-map (flatten (map
                                   (fn [k] [(first k) (nth k ind)]) reps)))]
    ;; return the target with replacements a la repmap
    (cons `do
          (nested-map
           (fn [symb]
             (let [g (get repmap symb)]
               (if (nil? g)
                 symb
                 g)))
           target))))

(defmacro case-pattern
  "Example:

  (case-pattern [foo true false]
                [fig 1 2
                 dog 2 3
                 funk 3 4]
    (* fig (+ dog (* func func))))

  goes to

  (case foo
    true
    (do
       (* 1 (+ 2 (* 3 3))))
    false
    (do
       (* 2 (+ 3 (* 4 4)))))

  WARNING: macro is dumb; it does not recognize
  let or other macros."
  [[det & options] [& bindings] & block]
  (let [size (inc (count options))
        bblocks (partition size bindings)
        caseitems (map #(cph % bblocks block) (range 1 size))
        mg (alternating options caseitems)]
    `(case ~det ~@mg)))


;;
;; Idea 1
;;
;;
;;  (let [b (parallel (heavycalc))]
;;    (local (needs-swing b)))
;;
;;
;; Idea 2
;;
;; (defn foofah [x] (* x x))
;; 
;; (let [a (compile (foofah 2))] // inline
;;    (foofah a)) // funcall
;;
;; Problem: side effects, closures
;; could have a (defcompilable; which stores 
;; both the source and the func; calling a defcompilable
;; yields the function; source is metadata...
