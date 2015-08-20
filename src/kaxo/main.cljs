(ns ^:figwheel-always kaxo.main
    (:require [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (union)]
              [cljsjs.react]
              [kaxo.core :refer (deb
                                 el
                                 svg-point
                                 drag-line
                                 game
                                 w-points
                                 history
                                 initial-history
                                 game-over?
                                 get-status
                                 canonical-line
                                 new-way-points
                                 line-move?
                                 dot-move?
                                 play-line-move
                                 play-dot-move
                                 )]
              [kaxo.ai :as ai]
              ))

(def messages {:yours "Your turn"
               :als   "My turn"
               :as-turn "Blue's turn"
               :bs-turn "Red's turn"
               :you-win "Well done! You won"
               :al-win "Oops! You lost"
               :a-win "Blue won!"
               :b-win "Red won!"
               :draw  "It's a draw!"
               })

(def message-colours {:yours :a
                      :als   :b
                      :as-turn :a
                      :bs-turn :b
                      :you-win :a
                      :al-win :b
                      :a-win :a
                      :b-win :b
                      :draw :draw
                      })

(def viewport-width 320)
(def viewport-height 320)
(def scale-n 9)
(def max-n 5)
(def min-n 3)
(def unit 1)
(def gap 35)
(def al-think-time 2000)
(def click-interval 333)
(def colours {:a "rgb(0, 153, 255)"
              :b "rgb(238, 68, 102)"
              :none "rgb(220,255,220)"
              :draw "rgb(74, 157, 97)"
              })

;;;
;; unit conversions
;;;
(defn units [x] (* x unit))
(defn stinu [x] (/ x unit)) ; inverse units

(defn gaps [x] (* (units (+ x 0.5)) gap))
(defn spag [x] (stinu (- (/ x gap) 0.5))) ; inverse gaps

(defn get-message [status]
  (status messages))

(defn get-fill [status]
  ((status message-colours) colours))

;;;
;; history for undo-redo handling
;;;
(defn reset-history!
  "reset history to start a new game"
  [n]
  (let [ih (assoc-in initial-history [1 0 0 :n] n)]
    (reset! history ih))
  )

(defn push-history!
  "Record game state in history"
  [g wps]
  (let [[n log] @history]
    #_(prn (str "history " n " -> " (inc n)))
    (reset! history [(inc n) (conj log [g wps])])))

(defn undo!
  "restore state of the previous move if it exists"
  []
  (let [[n log] @history
        last-n (if (> n 0) (- n 1) 0)
        state (nth log last-n)]
    (reset! history [last-n log])
    #_(prn "first log")
    #_(prn (first  state))
    (reset! game (first state))
    (reset! w-points (last state))))

(defn redo!
  "restore state of the next move if it exists"
  []
  (let [[n log] @history
        next-n (if (< (inc n) (count log)) (inc n) n)
        state (nth log next-n)]
    (reset! history [next-n log])
    (reset! game (first state))
    (reset! w-points (last state))
    )
  )

;;
;; reset the game initially
;;
(defn resize-game-board! [n]
  (swap! game #(assoc %
                   :a-crosses #{}
                   :b-crosses #{}
                   :a-lines #{}
                   :b-lines #{}
                   :n n))
  (reset! w-points #{})
  (reset-history! n))

(defn up-tap [event]
  "grow the game by 1 unit up to a 5 x 5 square"
  (.stopPropagation event)
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (< old-n max-n) (inc old-n) max-n)]
    (resize-game-board! new-n)))

(defn down-tap [event]
  "shrink the game by 1 unit down to a min-n square"
  (.stopPropagation event)
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (> old-n min-n) (- old-n 1) min-n)]
    (resize-game-board! new-n)))

;;;
;; reset game
;;;
(defn reset-game
  ([]
   (swap! game #(assoc % :player :a
                         :a-crosses #{}
                         :b-crosses #{}
                         :a-lines #{}
                         :b-lines #{}
                         ))
   (reset! w-points #{})
   (reset-history! (:n @game)))
  ([event]
   (.preventDefault event)
   (reset-game))
  ([event _]
   (reset-game event)))

;;;
;; ai turn
;;;
(defn delayed-call
  "Call f, optionally with arguments xs, after ms milliseconds"
  [ms f & xs]
  (js/setTimeout #(apply f xs) ms))

(defn play-ai-turn
  "play next ai move"
  []
  (prn "play computer turn")
  (ai/get-ai-move))

(defn ai-turn?
  "Is it time for the ai to play?. Call this after player switch"
  [g wps pl]
  (and (not (game-over? g wps)) (= 1 (:players g)) (= :b pl)))

(defn switch-player!
  "switch player after a move"
  []
  (let [g @game
        wps @w-points
        new-pl (if (= (:player g) :a) :b :a)]
    (prn "switch player")
    (swap! game #(assoc % :player new-pl))
    (when (ai-turn? g wps new-pl)
      (delayed-call al-think-time (play-ai-turn)))))

;;;
;; ui button events
;;;
(defn change-player-count
  "change to 1-player or 2-player mode"
  [count]
  (swap! game #(assoc %
                 :players count
                 :player :a
                 :a-crosses #{}
                 :b-crosses #{}
                 :a-lines #{}
                 :b-lines #{}))
  (reset! w-points #{}))

(defn one-player [event]
  (.preventDefault event)
  (change-player-count 1))

(defn two-player [event]
  (.preventDefault event)
  (change-player-count 2))

(defn undo
  "undo button handler"
  [event]
  (.preventDefault event)
  (undo!))

(defn redo
  "redo button handler"
  [event]
  (.preventDefault event)
  (redo!))

;;;
;; svg mouse handling
;;;
(defn scale [factor]
  (fn [[x y]] [(* factor x) (* factor y)]))

(defn touchXY
  "get position of first changed touch"
  [event]
  (let [touch (aget (.-changedTouches event) 0)]
    [(.-clientX touch) (.-clientY touch)]))

(defn mouseXY
  "get mouse position"
  [event]
  [(.-clientX event) (.-clientY event)])

(defn eventXY
  "get touch or mouse position"
  [event]
  (let [type (subs (.-type event) 0 5)]
    (condp = type
      "mouse" (mouseXY event)
      "touch" (touchXY event)
      (deb (str "strange event " (.type event)) [0 0]) ; ?? does this happen ??
      )))

(defn mouse->svg
  "browser independent transform from mouse/touch coords to svg viewport"
  [event]
  (let [svg (el "grid")
        pt (if @svg-point
             @svg-point
             (do
               (reset! svg-point (.createSVGPoint svg))
               @svg-point))
        matrix (.inverse (.getScreenCTM svg))
        ;; [x' y'] [(.-clientX event) (.-clientY event)]
        [x y] (eventXY event)
        ]
    ;; (set! (.-x pt) (.-clientX event))
    ;; (set! (.-y pt) (.-clientY event))
    (aset pt "x" x)
    (aset pt "y" y)
    ;(.log js/console (str  "x,y=" (.-x pt) (.-y pt)))
    (reset! svg-point (.matrixTransform pt matrix))
    [(.-x @svg-point) (.-y @svg-point)]))

(defn game->dot
  "game coords to integer game coords"
  [[x y]]
  [(.round js/Math x) (.round js/Math y)])

(defn svg->game
  "transform from svg viewport coords to game coords"
  [g]
  (comp
   (fn [[x y]] [(spag x) (spag y)])
   (scale (/ (:n g) scale-n ))))

(defn mouse->dot
  "find dot under mouse/touch point"
  [event]
  (game->dot ((svg->game @game) (mouse->svg event))))

(defn handle-start-line
  "start dragging a line"
  [event]
  (.preventDefault event)
  (let [svg (mouse->svg event)
        dot (game->dot ((svg->game @game) svg))]
    (reset! drag-line [[dot dot] (.now js/Date)])))

(defn handle-move-line
  "continue dragging a line"
  [event]
  (.preventDefault event)
  (let [svg (mouse->svg event)
        g @game
        [[drag-start _ :as dl] started-at] @drag-line
        drag-end ((svg->game g) svg)]
    (when dl
      (reset! drag-line [[drag-start drag-end] started-at]))))

(defn end-of-turn!
  "Push game-state to history. Do this after a new game move"
  [pl]
  (let [g @game
        wps @w-points
        new-player (if (= :a pl) :b :a)
        ]
    (swap! game #(assoc % :player new-player))
    (push-history! g wps)))

(defn handle-end-line
  "handle end of drag. Convert to a tap if not moved"
  [event]
  (.preventDefault event)
  (let [g @game
        wps @w-points
        [[draw-start _] started-at] @drag-line
        now (.now js/Date)
        dot (mouse->dot event)
        line (canonical-line [draw-start dot])
        new-wps (new-way-points line)
        pl (:player g)
        ]
    (let [game-state (cond

                       ;; handle possible drag-line
                       (line-move? line)
                       (play-line-move [g wps] line)

                       ;; handle possible tap or click
                       (dot-move? dot)
                       (if (< (- now started-at) click-interval)
                         (play-dot-move [g wps] dot)
                         nil)

                       :else nil)]


      (when game-state
        (let [[g' wps'] game-state]
          (reset! game g')
          (reset! w-points wps')
          (push-history! g' wps')))
      )
    (reset! drag-line [nil 0])))


;;;;;;;;;;;;;;;;;

;;;
;; rendering
;;;
(defn cross-colour [g point]
  (if ((:a-crosses g) point)
    (:a colours)
    (if ((:b-crosses g) point)
      (:b colours)
      (:none colours))))

(defn line-colour [g line]
  (if ((:a-lines g) line)
    (:a colours)
    (if ((:b-lines g) line)
      (:b colours)
      (:none colours))))

(r/defc render-drag-line < r/reactive [g]
  (let [[[[x1 y1] [x2 y2] :as [p1 p2]]] (r/react drag-line)]
    ; we don't want to render a line before the mouse moves as
    ; it prevents mouseup/touchend detection on the dot
    ; preventing clicks/taps there.
    (when (not= p1 p2)
      (do
        [:line {:stroke-linecap "round"
                :stroke ((:player g) colours)
                :stroke-width 7
                :style {:cursor "crosshair"}
                :x1 (gaps x1)
                :y1 (gaps y1)
                :x2 (gaps x2)
                :y2 (gaps y2)}]))))

(r/defc render-line [g player [[x1 y1] [x2 y2] :as line]]
  [:line {:stroke-linecap "round"
          :stroke (line-colour g line)
          :stroke-width 7
          :x1 (gaps x1)
          :y1 (gaps y1)
          :x2 (gaps x2)
          :y2 (gaps y2)}])

(r/defc render-lines [g]
  (let [a-lines (:a-lines g)
        b-lines (:b-lines g)
        ]
    [:g
     (map #(render-line g :a %) (vec a-lines))
     (map #(render-line g :b %) (vec b-lines))
     ]))

(r/defc render-cross [g [x y :as point]]
  (let [a 5
        cmd (fn [c x y] (str c " " (+ a x) " " (+ a y)))
        m #(cmd " M" %1 %2)
        l #(cmd " L" %1 %2)
        ]
    [:g {:transform (str "translate(" (- (gaps x) a) "," (- (gaps y) a) ")")}
     [:path {:d (str (m (- a) (- a)) (l a a) (m (- a) a) (l a (- a)))
             :stroke-width (str a)
             :opacity 1.0
             :stroke (cross-colour g point)
             :fill "none"}
            ]]))

(r/defc svg-dot [g n x y fill]
  (let [p [x y]
        the-class #(if ((:a-crosses g) p)  "dot claimed" "dot")]
    [:circle {
              :class (the-class)
              :cx (gaps x)
              :cy (gaps y)
              :r (units 8)
              :fill fill
              :stroke "#cceecc"
              :stroke-width (units  8)
              :id (str "[" x " " y "]")
              :key (str "[" x " " y "]")
              }]))

(r/defc svg-grid < r/reactive [g]
  [:svg {:view-box (str "0 0 " viewport-width " " viewport-height)
         :height "100%"
         :width "100%"
         :key "b3"
         :id "grid"
         :on-mouse-down handle-start-line
         :on-mouse-move handle-move-line
         :on-mouse-up handle-end-line
         :on-touch-start handle-start-line
         :on-touch-move handle-move-line
         :on-touch-end handle-end-line
         }
   (let [n (:n g)]
     [:g {:id "box" :transform (str "scale(" (* 1 (/ scale-n n)) ")")}
      (for [x (range n)]
        (for [y (range n)]
          (if (or ((:a-crosses g) [x y]) ((:b-crosses g) [x y]))
            (render-cross g [x y])
            (if (not ((r/react w-points) [x y]))
              (svg-dot g n x y (cross-colour g [x y])))
            )
          ))
      (render-lines g)
      (render-drag-line g)
      ])])

(r/defc debug-game < r/reactive [g]
  "heads up game state display"
  [:div {:class "debug"}
   [:p {:key "b1"} (str g)]
   [:p {:key "b2"} (str "drag "(r/react drag-line))]
   [:p {:key "b3"} (str "w-points " (r/react w-points))]
   [:p {:key "b5"} (str "hist " (r/react history))]
])

(r/defc tool-bar
  "render top toolbar"
  [g]
  (let [active (fn [g player-count]
                 (if (= player-count (:players g)) "active" ""))]
    [:div {:class "btn-group toolbar"}
     [:button {:type "button"
               :class (str "btn btn-success " (if (> (:n g) min-n) "" "disabled"))
               :key "bu1"
               :on-click down-tap
               :on-touch-end down-tap}
      [:span {:class "fa fa-chevron-down"}]]
     [:button {:type "button"
               :class (str "btn btn-success " (if (< (:n g) max-n) "" "disabled"))
               :key "bu2"
               :on-click up-tap
               :on-touch-end up-tap}
      [:span {:class "fa fa-chevron-up"}]]
     [:button {:type "button" :class (str "btn btn-default " (active g 1))
               :key "bu3"
               :on-click one-player
               :on-touch-end one-player}
      "1 player"]
     [:button {:type "button"
               :class (str "btn btn-default " (active g 2))
               :key "bu4"
               :on-click two-player
               :on-touch-end two-player} "2 player"]
     [:button {:type "button"
               :class "btn btn-info"
               :key "bu5"
               :on-click undo
               :on-touch-end undo}
      [:span {:class "fa fa-undo"}]]
     [:button {:type "button"
               :class "btn btn-info"
               :key "bu6"
               :on-click redo
               :on-touch-end redo}
      [:span {:class "fa fa-repeat"}]]
     ]))

(r/defc status-bar
  "render top status bar"
  [g wps]
  (let [[over-class status] (get-status g wps)]
    [:div
     [:p {:class (str "status " over-class)
          :style {:background-color (get-fill status)} :key "b4"} (get-message status)
      [:button {:type "button"
                :class "btn btn-danger"
                :style {:display "inline"
                        :clear "none"
                        :float "right"
                        :border-bottom-right-radius "35%"
                        }
                :key "bu7"
                :on-click reset-game
                :on-touch-end reset-game}
       [:span {:class "fa fa-refresh"}]]]]))

(r/defc footer
  "render footer with rules and copyright"
  []
  [:section {:id "footer"}
   [:h2
    ;;"To win, avoid moving last"
    ;; "Last player to move loses"
    "Last player able to move loses"
    ]
   [:p
    "To cross out dots, click on them or drag a horizontal, vertical or 45 degree line through them. Lines must not cross."]
   [:p
    "Kaxo game ©Alex Voak 2015. Used with permission."
    [:a {:href "https://github.com/gmp26/kaxo"}
     " Source on github"]]
   ])

(r/defc game-container  < r/reactive
  "the game container mounted onto the html game element"
  []
  (let [g (r/react game)
        wps (r/react w-points)]
    [:section {:id "game-container"}
     [:div {:class "full-width"}
      [:p {:id "header"} "Kaxo"]
      (tool-bar g)
      (status-bar g wps)]
     (svg-grid g)
     (footer)
     (debug-game g)]))

(defn initialise []
  (.initializeTouchEvents js/React true)

  ;; mount main component on html game element
  (r/mount (game-container) (el "game"))

  (reset-game)
  )

(initialise)