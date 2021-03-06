(ns gipf.core
  (:import (gipfj Geometry MathUtil Board GameState Reserves Line
                  IDRNode GameCalc GeneralizedPointWeighting
                  Ranking HistoryTable MoveSignedIGC
                  Counter Compression Ident ChildList STable ThreadBuffer
                  UCTNode Const)))

(definline place-and-shove [a b c] `(GameCalc/placeAndShove ~a ~b ~c))
(definline ->GameState [a b c1 c2] `(GameState/makeGameState ~a ~b ~c1 ~c2))
(definline game-state-board [gs] `(GameState/getBoard ~gs))
(definline game-state-reserves [gs] `(GameState/getReserves ~gs))
(definline game-state-changebr [gs bo re] `(GameState/changeBR ~gs ~bo ~re))
(definline game-state-gipf? [gs player] `(GameState/isGipfing ~gs ~player))

(definline row-full? [b s e] `(GameCalc/lineFull ~b ~s ~e))

(definline make-idr-node [gs playa rank] `(IDRNode/makeIDRNode ~gs ~playa ~rank))
(definline idr-node-update [node rank kids] `( IDRNode/updateIDRNode ~node ~rank ~kids))
(definline idr-node-gamestate [a] `( IDRNode/getGameState ~a))
(definline idr-node-player [a] `( IDRNode/getPlayer ~a))
(definline idr-node-rank [a] `( IDRNode/getRank ~a))
(definline idr-node-children [a] `( IDRNode/getChildren ~a))

(definline add-weight-arrays [aa aw ba bw]
  `(GeneralizedPointWeighting/mergeWeights ~aa ~aw ~ba ~bw))
(definline diag-weight-array [mg mp op og c d e]
  `(GeneralizedPointWeighting/diagWeights ~mg ~mp ~op ~og ~c ~d ~e))
(definline radial-weight-array [mg mp op og r0 r1 r2 r3]
  `(GeneralizedPointWeighting/radiusWeights ~mg ~mp ~op ~og ~r0 ~r1 ~r2 ~r3))
(definline apply-weight-array [b p ar]
  `(GeneralizedPointWeighting/calcVal ~b ~p ~ar))
(definline total-weight-sub-array [center r1 star2 mid2 star3 mid3]
  `(GeneralizedPointWeighting/totalControlSubLevel ~center ~r1 ~star2 ~mid2 ~star3 ~mid3))
(definline total-weight-array [pg pp np ng]
  `(GeneralizedPointWeighting/totalControl ~pg ~pp ~np ~ng))

(definline weighted-add-2 [va ca vb cb]
  `(Ranking/weightedAdd ~va ~ca ~vb ~cb))
(definline weighted-add-3 [va ca vb cb vc cc]
  `(Ranking/weightedAdd ~va ~ca ~vb ~cb ~vc ~cc))
(definline weighted-add-4 [va ca vb cb vc cc vd cd]
  `(Ranking/weightedAdd ~va ~ca ~vb ~cb ~vc ~cc ~vd ~cd))

(definline balance-normalize [good bad]
  `(Ranking/balanceNormalize ~good ~bad))
(definline linear-scale [val mni mxi mno mxo]
  `(Ranking/linearScale ~val ~mni ~mxi ~mno ~mxo))

(definline reserve-diff-linear [r p gipfs pieces store]
  `(Ranking/reserveLinearDiff ~r ~p ~gipfs ~pieces ~store))
(definline reserve-diff-quadratic [r p gipfs pieces store]
  `(Ranking/reserveQuadDiff ~r ~p ~gipfs ~pieces ~store))
(definline reserve-diff-cubic [r p gipfs pieces store]
  `(Ranking/reserveCubicDiff ~r ~p ~gipfs ~pieces ~store))


(def default-buffer ThreadBuffer/DEFAULT)
(definline lazy-next-gamestates
  [gamestate player]
  `(from-iterator (MoveSignedIGC/makeIncrementalGameCalc default-buffer ~gamestate ~player)))

(definline hist-ordering [^HistoryTable table buffer]
  `(HistoryTable/hordering ~table ~buffer))
(definline make-hist-table []
  `(HistoryTable/hmake))
(definline hist-add! [^HistoryTable table depth mnum]
  `(HistoryTable/hadd ~table ~depth ~mnum))
(definline hist-clear! [table]
  `(do (ond :hist-analysis
            (HistoryTable/hanalyze ~table))
       (HistoryTable/hclear ~table)))

(definline dtab-get [table key depth]
  `(STable/sget ~table ~key ~depth))
(definline make-dtab [sz]
  `(STable. ~sz))
(definline dtab-add! [table key level rank]
  `(STable/sadd ~table ~key ~level ~rank))
(definline dtab-change! [table  key level rank]
  `(STable/supdate ~table ~key ~level ~rank))
(definline dtab-clear! [table]
  `(do (ond :transp-analysis
            (STable/sanalyze ~table))
       (STable/sempty ~table)))

(definline signed-gs-move [sgs]
  `(GameState/getMove ~sgs))

(definline read-counter  [counter]
  `(Counter/cget ~counter))
(definline clear-counter [counter]
  `(Counter/cclear ~counter))

(definline compress-sgs  [buffer gamestate player]
  `(Compression/compress ~buffer ~gamestate ~player))
(definline dispose-move-generator! [gen]
  `(MoveSignedIGC/dispose ~gen))

(definline clist-make  []
  `(ChildList/clmake))
(definline clist-add  [cl obj rank]
  `(ChildList/cladd ~cl ~obj ~rank))
(definline clist-pack  [cl desc]
  `(ChildList/clpack ~cl ~desc))

(definline unordered-move-generator [buf gs p]
  `(MoveSignedIGC/makeIncrementalGameCalc ~buf ~gs ~p))
(definline move-generator [buf gs p ordering]
  `(MoveSignedIGC. ~buf ~gs ~p ~ordering))
(definline random-move-generator [buf gs p]
  `(MoveSignedIGC/makeRandomMoveGenerator ~buf ~gs ~p))

(definline get-random-progression [buf gs p]
  `(GameCalc/getRandomProgression ~buf ~gs ~p))

(definline make-thread-buffer [id]
  `(ThreadBuffer/create ~id))

(defn get-player-lines-of-four
  [board player]
  (vec (GameCalc/getBoardLines board player)))

(definline make-uctn [g]  `(UCTNode. ~g))
(definline uctn-select [u] `(UCTNode/uctselect ~u))
(definline uctn-post! [u g w] `(UCTNode/uctpost ~u ~g ~w))
(definline uctn-move [u] `(UCTNode/uctmove ~u))
(definline uctn-terminate! [u g w] `(UCTNode/uctterminate ~u  ~g ~w))
(definline uctn-grow! [u c] `(UCTNode/uctgrow ~u ~c))
(definline uctn-rank [u g w] `(UCTNode/uctrank ~u ~g ~w))
(definline uctn-final [u] `(UCTNode/uctfinal ~u))
(definline uctn-untried [u] `(UCTNode/uctunvisited ~u))
(definline uctn-children? [u] `(UCTNode/uctchilded ~u))

(definline set-uct-constant! [flotty]
  `(UCTNode/setUCTLevel ~flotty))

(definline apply-move [buf gs p m]
  `(GameCalc/applyMove ~buf ~gs ~p ~m))

(definline get-player-lints-of-four [board player]
  `(vec (GameCalc/getFilteredBoardLines default-buffer ~board ~player)))

(definline lint-to-line [lint]
  `(Line/lintToLine ~lint))
(definline line-to-lint [line]
  `(Line/lineToLint ~line))

;; predicates/extraction

(def list-of-move-lines
  (doall (reduce concat
                 (map #(let [inside-neighbors (filter
                                               (fn [p] (= (pt-radius p) 3))
                                               (map (fn [q] (pt+ q %))
                                                    (get-ring-of-hex-uv-points 1)))]
                         (map (fn [ne] (->Line % (pt- ne %)))
                              inside-neighbors))
                      (get-ring-of-hex-uv-points 4)))))

(defn get-open-moves
  "Returns a list of the available moves..."
  [board]
  ;; does it work
  (filter #(not (row-full? board (pt+ (line-start %) (line-delta %)) (line-delta %)))  
          list-of-move-lines))

(def axial-points
  (map #(pt* 4 %) unit-ring-points))

(def place-point-open?
  "Can a point be placed here?"
  (fn [board loc]
    (and (= 4 (pt-radius loc))
         (if (some #(pt= % loc) axial-points)
           (let [dekl (pt- (pt-div-4 loc))]
             (not (row-full? board (pt+ loc dekl) dekl)))
           (let [pts (filter #(= (pt-radius %) 3)
                             (map #(pt+ % loc) unit-ring-points))]
             (some #(not (row-full? board % (pt- % loc))) pts))))))


(defn impacted-cells
  [board loc shove]
  (vec (GameCalc/getImpactedCells board loc shove)))

(defn do-move
  "Does not update properly"
  [gamestate value loc shove]
  [ (place-and-shove gamestate value (->Line loc shove))
    (impacted-cells (game-state-board gamestate) loc shove)])

(def list-of-split-lines (mapv #(mapv vec (vec %)) (vec Const/listOfSplitLinePoints)))

(defn reduce-portion
  "Returns [protect newboard newreserves]"
  [points protect board reserves player]
  (loop [points points bpr protect board board brr reserves]
    (if (empty? points)
      [bpr board brr]
      (let [loc (first points)
            val (int (multiply
                       (get-hex-array board loc)
                       player))]
        (case val
          -2
          (recur (rest points)
                 bpr
                 (change-hex-array board loc 0)
                 (reserve-delta brr (negate player)
                                0 0 -1))
          -1 ;; opponent
          (recur (rest points)
                 bpr
                 (change-hex-array board loc 0)
                 (reserve-delta brr (negate player)
                                0 -1 0))
          0 ;; stop - can't take past an empty square
          [bpr board brr]
          1
          (recur (rest points)
                 bpr
                 (change-hex-array board loc 0)
                 (reserve-delta brr player 1 -1 0))
          2 ;; save own gipfs
          (do
            (recur (rest points)
                   (conj bpr loc)
                   board brr)))))))


(defn do-linemoves
  "Used in game, to easily and simply effect a line move"
  [gs player move]
  (let [prot (ffirst move)]
    (loop [lleft (rest move) gamestate gs]
      (if (empty? lleft)
        gamestate
        (recur
         (rest lleft)
         (let [line (line-to-lint (first lleft))
               points (get list-of-split-lines line)
               board (game-state-board gs)
               reserves (game-state-reserves gs)
               [_ board reserves] (reduce-portion (get points 0) [] board reserves player)
               [_ board reserves] (reduce-portion (get points 1) [] board reserves player)]
           (->GameState board reserves false false)))))))

(defn get-line-taking-orderings
  [gamestate player]
  (list
   (loop [board (game-state-board gamestate)
          reserves (game-state-reserves gamestate)
          taken [] protected []]
     (let [found (filter
                  (fn [l]
                    (not (some (fn [k] (equals k l)) taken)))
                  (get-player-lints-of-four board player))]
       (if (empty? found)
         ;; for some odd reason, lint-to-line fails, even though definline
         ;; should create a function as well...
         [(concat [protected] (map (fn [k] (lint-to-line k)) taken))
          (game-state-changebr gamestate board reserves)]
         ;; this seems to work...
         (let [chosen (first found)
               points (get list-of-split-lines chosen)
               [prot nb nr] (reduce-portion (get points 0) [] board reserves player)
               [prot nb nr] (reduce-portion (get points 1) prot nb nr player)]
           
           (recur nb nr (conj taken chosen) (conj protected prot))))))))

(defn list-possible-moves-and-board
  "Like list-possible-boards, just returns the moves along with the boards.
  ([move board reserves] ... )"
  [gamestate player]
  
  (let [lines-board (get-line-taking-orderings gamestate player)
        actions-board (expand
                       (fn [[lmove gamestate]]
                         ;; return all possible shoves after this,
                         ;; form [[lmove shove]]
                         (expand
                          (fn [shove]
                            (if (game-state-gipf? gamestate player)
                              (list [[lmove (sign-line shove player)]
                                     (place-and-shove gamestate
                                                      player
                                                      (advance-line shove))]
                                    [[lmove (sign-line shove (multiply 2 player))]
                                     (place-and-shove gamestate
                                                      (multiply 2 player)
                                                      (advance-line shove))])
                              (list [[lmove (sign-line shove player)]
                                     (place-and-shove gamestate
                                                      player
                                                      (advance-line shove))])))
                          (get-open-moves (game-state-board gamestate))))
                       lines-board)
        flines-board (expand
                      (fn [[move gamestate]]
                        (map
                         (fn [[lmove gamestate]]
                           [(conj move lmove) gamestate])
                         (get-line-taking-orderings gamestate player)))
                      actions-board)]
    flines-board))

;; GUNK

(let [emr (atom nil)]
  (defn set-emr! [value]
    (reset! emr value))
  (defn get-emr! []
    @emr))
(defrecord Heuristic [setup eval])
(defrecord Search [pre eval post])

(let [mrfs (atom {})]
  (defn setup-move-ranking-func!
    [player lead-heuristic search & sfargs]
    (when (or (nil? (:setup lead-heuristic))
              (nil? (:eval lead-heuristic))
              (nil? (:pre search))
              (nil? (:eval search))
              (nil? (:post search)))
      (throw (java.lang.Exception.
              "Wrong types passed to setup-move-ranking-func!")))
    (swap! mrfs
           #(assoc % player
                   [(fn []
                      ((:setup lead-heuristic))
                      (apply (:pre search) sfargs))
                    (fn []
                      (apply (:post search) sfargs))
                    (fn [buf state player]
                      (apply (:eval search) buf state player
                             (:eval lead-heuristic) sfargs))])))
  (defn get-search
    [player]
    (tget @mrfs player)))


(def ranks-count (Counter/cmake))

(def heuristic-choices {})
(defn dfr-helper
  [name doc setupexprs evalarg1 evalarg2 evalexprs]
  ;; odd behavior: (instance? Heuristic %) only always works if the Heuristic
  ;; was made using the (Heuristic. ) method, not the (->Heuristic
  ;; map) method. Whatever
  (let [sn (str name)]
    `(do
       (def ~name ~doc (Heuristic.
                        (fn [] ~@setupexprs)
                        (fn ~name [~evalarg1 ~evalarg2]
                          (Counter/cinc ranks-count)
                          ~@evalexprs)))
       (def heuristic-choices (assoc heuristic-choices ~sn ~name))
       ~sn)))

(defmacro def-ranking-function
  "Example input:

  (def-ranking-function oogscape
    (:eval [g p] (println \"rankin'\" (* p 20 (random-long -10 10))))
    (:setup [] (initialize-random-float)))
"
  ([name [key1 [& key1args] & key1exprs] [key2 [& key2args] & key2exprs]]
     `(def-ranking-function ~name ""
        (~key1 [~@key1args] ~@key1exprs)
        (~key2 [~@key2args] ~@key2exprs)))
  ([name docstring [key1 [& key1args] & key1exprs] [key2 [& key2args] & key2exprs]]
     (cond (and (= key1 :setup) (= key2 :eval)
                (empty? key1args)
                (= 2 (count key2args)))
           (dfr-helper name docstring key1exprs (first key2args) (second key2args) key2exprs)
           (and (= key2 :setup) (= key1 :eval)
                (empty? key2args)
                (= 2 (count key1args)))
           (dfr-helper name docstring key2exprs (first key1args) (second key1args) key1exprs)
           :else
           (throw (IllegalArgumentException. "wrong clauses to def-ranking-function")))))


(defmacro export
  "Ye gods, how do I long for macrolet.
  This should belong, locally bound, in def-search"
  [thing val]
  `(reset! ~thing ~val))

(def search-choices {})
(def search-args {})
(defn dfsh
  [name docs cw common sargs sbody targs tbody rargs rbody]
  (let [nil-atoms (alternating common (map (fn [_] `(atom nil)) (range)))
        rebounds (reduce concat (map (fn [symb] [symb `(deref ~symb)]) common))
        newname (symbol (str name "-func"))
        empty-atoms (map (fn [symb] `(reset! ~symb nil)) common)
        sn (str name)]
    `(do (let [~@nil-atoms]
           (def ~name ~docs
             (Search.
              (fn [~@sargs] ~@sbody)
              (fn [~@rargs] ;; could do targs collision avoidance... nah..
                (let [~@rebounds]
                  ~@rbody))
              (fn [~@targs]
                (let [~@rebounds]
                  ~@tbody)
                ~@empty-atoms))))
         (def ~newname ~docs (:eval ~name))
         (def search-choices (assoc search-choices ~sn ~name))
         (def search-args (assoc search-args ~sn ~cw))
         ~sn)))

(defn dch
  [n d r a]
  (let [s (str n)]
    `(do
       (def ~n ~d nil)
       (def search-choices (assoc search-choices ~s ~r))
       (def search-args (assoc search-args ~s ~a)))))

(defmacro def-config
  ([name doc derived argmap]
     (dch name doc derived argmap))
  ([name derived argmap]
     (dch name "" derived argmap)))

(defmacro def-search
  ([name callb [& common]
    [key1 [& args1] & body1]
    [key2 [& args2] & body2]
    [key3 [& args3] & body3]]
     (doseq [k [key1 key2 key3]]
       (when-not (or (= k :pre) (= k :post) (= k :eval))
         (throw (java.lang.Exception. (str k " is not a valid member function.")))))
     (let [aba (atom {})]
       (doseq [[k args body] [[key1 args1 body1]
                              [key2 args2 body2]
                              [key3 args3 body3]]]
         (swap! aba #(assoc % k [args body])))
       (let [[sargs sbody] (:pre @aba)
             [targs tbody] (:post @aba)
             [rargs rbody] (:eval @aba)]
         (dfsh name "" callb common sargs sbody targs tbody rargs rbody))))
  ([name docstring callb
    [& common]
    [key1 [& args1] & body1]
    [key2 [& args2] & body2]
    [key3 [& args3] & body3]]
     (dfsh name docstring callb common args1 body1 args2 body2 args3 body3))
  ([name callb [] [key [& args] & body]]
     (dfsh name "" callb [] (list (symbol "&") (symbol "args")) (list)
           (list (symbol "&") (symbol "args")) (list) args body))
  ([name docstring callb [] [key [& args] & body]]
     (dfsh name docstring callb [] (list (symbol "&") (symbol "args")) (list)
           (list (symbol "&") (symbol "args")) (list) args body)))





(defn print-board
  [board]
  ;; I want macrolet ;-)
  (let [enm (str "0123456789" "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        transform
        (str
         "         j            \n"
         "      k     A         \n"
         "   l     7     z      \n"
         "m     8     i     y   \n"
         "   9     1     h      \n"
         "n     2     6     x   \n"
         "   a     0     g      \n"
         "o     3     5     w   \n"
         "   b     4     f      \n"
         "p     c     e     v   \n"
         "   q     d     u      \n"
         "      r     t         \n"
         "         s            \n")
        
        zmap (map list
                  (map str
                       enm)
                  (map
                   #(case (int (get-hex-array board %))
                      -2 "="
                      -1 "-"
                      0 "."
                      1 "+"
                      2 "#")
                   (range 37)))]
    (loop [meat transform rk zmap]
      (if (empty? rk)
        (println meat)
        (let [k (ffirst rk)
              v (second (first rk))]
          (recur (clojure.string/replace meat k v)
                 (rest rk)))))))


;;
;; More ideas: store metadata about the heuristic/search combination.
;; and use an optional display.
;;
;;
;;
;;
;;
;;
