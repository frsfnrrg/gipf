(in-ns 'gipf.core)

(declare update-game)
(declare mode*)
(declare ai-player?)
(declare interrupt-ai-threads!)
(declare read-ai-level)
(declare read-ai-search)
(declare read-ai-heuristic)
(declare update-ai-choices)
(declare init-at-choices)

(def difficulty-labels ["Trivial" "Simple" "Easy" "Middle" "Long"])

(defmacro generate-buttons
  [chosen dataset [name obj button] & block]
  `(let [res# ~chosen
         ^javax.swing.ButtonGroup bgroup# (javax.swing.ButtonGroup.)]
     (vec (map (fn [[^String ~name ~obj]]
                 (let [~button (javax.swing.JRadioButtonMenuItem.
                                (str ~name))]
                   (when (= ~name res#)
                     (.setSelected ~button true))
                   (.add bgroup# ~button)
                   (set-on-button-select! ~button (fn []
                                                    ~@block))
                   ~button))
               ~dataset))))

(let [invd (apply applyto-repeatedly
                  #(assoc %1 %2 %3)
                  {}
                  (map (fn [x y] [x y])
                       difficulty-labels
                       (range)))]
  (defn make-difficulty-choices
    [id]
    (generate-buttons (tget difficulty-labels (read-ai-level id))
                      (map list difficulty-labels (range))
                      [name thing b]
                      (update-ai-choices id :level (tget invd name)))))

(defn make-search-options
  [id]
  (generate-buttons
   (read-ai-search id) search-choices [name thing b]
   (update-ai-choices id :search name)))

(defn make-heuristic-options
  [id]
  (generate-buttons
   (read-ai-heuristic id) heuristic-choices [name thing b]
   (update-ai-choices id :heuristic name)))

(defn make-ai-menu-system
  [key id]
  (let [levels (make-difficulty-choices id)
        search-options (make-search-options id)
        heuristic-options (make-heuristic-options id)
        search (javax.swing.JMenu. "Search Function")
        heuristic (javax.swing.JMenu. "Heuristic")
        menu (doto (javax.swing.JMenu. (str "Player " key " AI"))
                    (.add search)
                    (.add heuristic))]
    (doseq [^javax.swing.JRadioButtonMenuItem ch levels]
      (.add menu ch))
    (doseq [^javax.swing.JRadioButtonMenuItem ch heuristic-options]
      (.add heuristic ch))
    (doseq [^javax.swing.JRadioButtonMenuItem ch search-options]
      (.add search ch))
    menu))

(defn runGUI
  []
  (let [window (javax.swing.JFrame. "GIPF")
        menubar (javax.swing.JMenuBar.)
        mode-basic (javax.swing.JRadioButtonMenuItem. "Basic")
        mode-normal (javax.swing.JRadioButtonMenuItem. "Normal")
        mode-advanced (javax.swing.JRadioButtonMenuItem. "Advanced")
        
        button-new (javax.swing.JMenuItem. "New")
        button-quit (javax.swing.JMenuItem. "Quit")

        iai-one (javax.swing.JCheckBoxMenuItem. "Player 1: AI?")
        iai-two (javax.swing.JCheckBoxMenuItem. "Player 2: AI?")

        ^javax.swing.JMenu menusystem-1 (make-ai-menu-system "1" 1)
        ^javax.swing.JMenu menusystem-2 (make-ai-menu-system "2" -1)]

    ;; we call a new game;
    (init-ai-choices)
    (update-game (list [:state :new]))
    
    (.setSelected (case mode*
                    :basic  mode-basic
                    :normal mode-normal
                    :advanced mode-advanced) true)

    (.setSelected iai-one (ai-player? 1))
    (.setSelected iai-two (ai-player? -1))
    
    (set-on-button-select! mode-basic
                           (fn []
                             (update-game (list [:state :basic]
                                                [:state :new]))))
    (set-on-button-select! mode-normal
                           (fn []
                             (update-game (list [:state :normal]
                                                [:state :new]))))
    (set-on-button-select! mode-advanced
                           (fn []
                             (update-game (list [:state :advanced]
                                                [:state :new]))))

    ;; we do not call a new game, in case the player wants to switch
    ;; ai on
    (set-on-state-change! iai-one
                          (fn [s]
                            (update-game (list [:state :player-ai 1 s]))))
    (set-on-state-change! iai-two
                          (fn [s]
                            (update-game (list [:state :player-ai -1 s]))))
    
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
              (.add button-quit)))
      (.add (doto (javax.swing.JMenu. "Player Types")
              (.add iai-one)
              (.add iai-two)))
      (.add menusystem-1)
      (.add menusystem-2))
    
    (doto ^javax.swing.JPanel game-panel
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
      (.addWindowListener (proxy [java.awt.event.WindowListener] []
                            (windowOpened [_])
                            (windowClosed [_])
                            (windowActivated [_])
                            (windowDeactivated [_])
                            (windowIconified [_])
                            (windowDeiconified [_])
                            (windowClosing [^java.awt.event.WindowEvent _]
                              (interrupt-ai-threads!))))
      (.setContentPane game-panel)
      (.addKeyListener (proxy [java.awt.event.KeyListener] []
                         (keyPressed [^java.awt.event.KeyEvent e] nil)
                         (keyTyped [^java.awt.event.KeyEvent e] nil)
                         (keyReleased [^java.awt.event.KeyEvent e]
                           (when (= java.awt.event.KeyEvent/VK_ESCAPE (.getKeyCode e))
                             (println "Quitting on ESC key! Yay!")
                             (interrupt-ai-threads!)
                             (.setVisible window false)
                             (.dispose ^javax.swing.JFrame window)))))
      (.pack)
      (.setResizable false)
      (.setVisible true))))

