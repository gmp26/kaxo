(ns ^:figwheel-always kaxo.core
    (:require [game.gestures :refer [bind-touch] :as gest]
              [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (union)]
              [cljsjs.react]
              ))

(enable-console-print!)

(defn deb ([x]
   "print value as identity for debug use"
   (prn x) x)
  ([s x]
   (prn (str s x)) x))

;; define game state once so it doesn't re-initialise on reload

(defn el [id] (.getElementById js/document id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; state constants
;;

(def initial-state {:n 4
                    :player :a
                    :players 2
                    :a-crosses #{}
                    :b-crosses #{}
                    :a-lines #{}
                    :b-lines #{}
                    })

(def initial-history [0 [[initial-state #{}]]])

(defonce game (atom initial-state))
(defonce drag-line (atom [nil 0]))
(defonce w-points (atom #{}))
(defonce history (atom initial-history))

(defn reset-history!
  "reset history to start a new game"
  []
  (reset! history initial-history)
  )

(defn push-history!
  "record game state in history. Do this after a new game move"
  [g wps]
  (let [[n log] @history]
    (prn (str "history " n " -> " (inc n)))
    (reset! history [(inc n) (conj log [g wps])])))

(defn undo!
  "restore state of the previous move if it exists"
  []
  (let [[n log] @history
        last-n (if (> n 0) (- n 1) 0)
        state (nth log last-n)]
    (reset! history [last-n log])
    (prn "first log")
    (prn (first  state))
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

(defn undo
  "undo button handler"
  [_]
  (undo!)
  )

(defn redo
  "redo button handler"
  [_]
  (redo!)
  )

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
(defn units [x] (* x unit))
(defn stinu [x] (/ x unit)) ; inverse units

(defn gaps [x] (* (units (+ x 0.5)) gap))
(defn spag [x] (stinu (- (/ x gap) 0.5))) ; inverse gaps
(def al-think-time 2000)

(def colours {:a "rgb(0, 153, 255)"
              :b "rgb(238, 68, 102)"
              :none "rgb(220,255,220)"
              :draw "rgb(74, 157, 97)"
              })

;;;;;;;;;;;;;;;;;;;;;
;;
;; Game life cycle
;;
(defn game-drawn? [g] false)

(defn removed-points [wps]
  (filter (fn [[x y]] (even? (* 2 x)) (even? (* 2 y))) wps))

(defn game-over? [g wps]
  "return :a :b or false"
  (let [n (:n g)
        r-max (* n n)
        r-count (inc (count (removed-points wps)))]
    (cond
     (< r-max r-count) (if (= (:player g) :a) :b :a)
     (= r-max r-count) (:player g)
     :else false)
    ))

(defn get-ai-move [player] nil)

(defn computer-turn [g]
  #_(prn "play computer turn")
  (get-ai-move :b)
)

;;
;; reset the game initially
;;
(declare reset-game)

(defn resize-game-board!! [n]
  (swap! game #(assoc %
                   :a-crosses #{}
                   :b-crosses #{}
                   :a-lines #{}
                   :b-lines #{}
                   :n n))
  (reset! w-points #{})
  (reset-history!))

(defn up-tap [event]
  "grow the game by 1 unit up to a 5 x 5 square"
  (.stopPropagation event)
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (< old-n max-n) (inc old-n) max-n)]
    (resize-game-board!! new-n)
    ))

(defn down-tap [event]
  "shrink the game by 1 unit down to a min-n square"
  (.stopPropagation event)
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (> old-n min-n) (- old-n 1) min-n)]
    (resize-game-board!! new-n)
    ))

(defn claim-a-point [cross point]
  (swap! game #(assoc % :player :b :a-crosses (conj cross point)))
  ;
  ; Should we make a w-point here too?
  ;
)

(defn claim-b-point [cross point]
  (swap! game #(assoc % :player :a :b-crosses (conj cross point)))  ;
  ; Should we make a w-point here too?
  ;
)

(defn claim-point [a-crosses b-crosses point player]
  (if (and (not (a-crosses point)) (not (b-crosses point)))
    (if (= player :a)
      (claim-a-point a-crosses point)
      (claim-b-point b-crosses point))))

(declare timeout)

;;
;; events
;;


(defn timeout [ms f & xs]
  "Call f, optionally with arguments xs, after ms milliseconds"
  (js/setTimeout #(apply f xs) ms))

(defn single-player-point [g wps a-crosses b-crosses point]
  (do
    ;todo
    (swap! history #(conj % [g wps]))
    (when (not (or (game-over? g wps) (game-drawn? g)))
      (claim-a-point a-crosses point))
    (let [newg @game]
      (when (not (or (game-over? newg wps) (game-drawn? newg)))
        (timeout al-think-time #(->> newg
                                     (computer-turn)
                                     (claim-b-point b-crosses)))))))

(defn change-players [count]
  (swap! game #(assoc %
                 :players count
                 :player :a
                 :a-crosses #{}
                 :b-crosses #{}
                 :a-lines #{}
                 :b-lines #{}
                 ))
  (reset! w-points #{}))

(defn one-player [event]
  (.preventDefault event)
  (change-players 1))

(defn two-player [event]
  (.preventDefault event)
  (change-players 2))

(defn reset-game
  ([]
   (swap! game #(assoc % :player :a
                         :a-crosses #{}
                         :b-crosses #{}
                         :a-lines #{}
                         :b-lines #{}
                         ))
   (reset! w-points #{})
   (reset-history!))
  ([event]
   (.preventDefault event)
   (reset-game))
  ([event _]
   (reset-game event)))

(defn start-game [] (reset-game))


;;
;; rendering game board
;;
(declare get-status)
(declare get-fill)


(declare good-line?)

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
  (let [[[[x1 y1] [x2 y2] :as [p1 p2 :as line]] _] (r/react drag-line)]
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
                :y2 (gaps y2)}])))
  )

(r/defc render-line [g player [[x1 y1] [x2 y2] :as line]]
  [:line {:stroke-linecap "round"
          :stroke (line-colour g line)
          :stroke-width 7
          :x1 (gaps x1)
          :y1 (gaps y1)
          :x2 (gaps x2)
          :y2 (gaps y2)}]
  )

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

(defn scale [factor]
  (fn [[x y]] [(* factor x) (* factor y)]))

(def svg-point (atom false))

(defn mouse->svg [event]
  (let [svg (el "grid")
        pt (if @svg-point
             @svg-point
             (do
               (reset! svg-point (.createSVGPoint svg))
               @svg-point))
        matrix (.inverse (.getScreenCTM svg))]
    (do
      (set! (.-x pt) (.-clientX event))
      (set! (.-y pt) (.-clientY event))
      ;(.log js/console (str  "x,y=" (.-x pt) (.-y pt)))
      (reset! svg-point (.matrixTransform pt matrix))
      [(.-x @svg-point) (.-y @svg-point)])
))

(defn game->dot [[x y]]
  [(.round js/Math x) (.round js/Math y)])

(defn svg->game [g]
  (comp
   (fn [[x y]] [(spag x) (spag y)])
   (scale (/ (:n g) scale-n ))
   ))

(defn handle-tap [event]
  (let [;p (reader/read-string (.. event -target -id))
        g @game
        svg (mouse->svg event)
        dot (game->dot ((svg->game @game) svg))
        a-crosses (:a-crosses g)
        b-crosses (:b-crosses g)
        pl (:player g)
        new-w-points (union @w-points #{dot})]
    (do
      (.preventDefault event)
      (if (= (:players g) 2)
        (when (not (@w-points dot))
          (do
            (reset! w-points new-w-points)
            (claim-point a-crosses b-crosses dot pl)
            ))
        (when (= pl :a)
          (single-player-point g @w-points a-crosses b-crosses dot)))
      (push-history! @game @w-points))))

(defn handle-start-line [event]
  (.preventDefault event)
  (let [svg (mouse->svg event)
        dot (game->dot ((svg->game @game) svg))
        ]
    (reset! drag-line [[dot dot] (.now js/Date)])
))

(defn handle-move-line [event]
  (.preventDefault event)
  (let [svg (mouse->svg event)
        g @game
        [[drag-start _ :as dl] started-at] @drag-line
        drag-end ((svg->game g) svg)]
    (when dl
      (reset! drag-line [[drag-start drag-end] started-at])
)))

(defn canonical-line [[p1 p2 :as line]]
  (let [[[x1 y1] [x2 y2]] [p1 p2]]
    (if (or (< x1 x2)
            (and (= x1 x2)
                 (< y1 y2))
            )
      line
      [p2 p1])))

(defn add-way-points [[[x1 y1] [x2 y2] :as [p1 p2]] slope-type]
  ;"Given a doubled line segment and a slope type,"
  "find dots traversed by line."
  "Include mid-points btween dots on diagonal lines"
  "A zero slope-type means horizontal or vertical, else it means gradient"
  "Only 0, -1, 1 slope-types are allowed"
  (if (= 0 slope-type)
    (if (= x1 x2)
      (into #{} (for [y (range y1 (inc y2) 1)] [x1 y]))
      (into #{} (for [x (range x1 (inc x2) 1)] [x y1])))
    (into #{} (for [x (range x1 (+ x2 0.4) 0.5)]
                [x (+ y1 (* (- x x1) slope-type))]))
    )
)

(defn new-way-points [[[x1 y1] [x2 y2] :as [p1 p2 :as p]]]
  "way-points that a line crosses or nil for bad gradient or a point-line"
  (if (= p1 p2)
    nil
    (let [dx (- x2 x1)
          dy (- y2 y1)]
      (cond
       (= 0 dx) (add-way-points p 0)
       (= 0 dy) (add-way-points p 0)
       (= dx dy) (add-way-points p 1)
       (= dx (- dy)) (add-way-points p -1)
       :else nil
       ))))


(defn handle-end-line [event]
  (.preventDefault event)
  (let [svg (mouse->svg event)
        g @game
        [[draw-start _] started-at] @drag-line
        now (.now js/Date)
        is-tap (< (- now started-at) 333)
        dot (game->dot ((svg->game g) svg))
        line (canonical-line [draw-start dot])
        old-wps @w-points
        new-wps (new-way-points line)
        ]
    (if is-tap
      (handle-tap event)
      (do
        (when new-wps
          (when (not-any? new-wps old-wps)
            (let [line-key (if (= :a (:player g)) :a-lines :b-lines)
                  updated-lines (conj (line-key g) line)
                  new-player (if (= (:player g) :a) :b :a)]
              (swap! game #(assoc %
                                  line-key updated-lines
                                  :player new-player))
              (reset! w-points (union old-wps new-wps))
              )))
        (push-history! @game @w-points)))
    (reset! drag-line [nil 0])
    ))


(r/defc svg-dot < r/reactive [n x y fill]
  (let [p [x y]
        g (r/react game)
        the-class #(if ((:a-crosses g) p)  "dot claimed" "dot")
        ]
    (do
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
                }])))

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
         :style {:display-mode "inline-block"}
         }
   (let [n (:n g)]
     [:g {:id "box" :transform (str "scale(" (* 1 (/ scale-n n)) ")")}
      (for [x (range n)]
        (for [y (range n)]
          (if (or ((:a-crosses g) [x y]) ((:b-crosses g) [x y]))
            (render-cross g [x y])
            (if (not ((r/react w-points) [x y]))
              (svg-dot n x y (cross-colour g [x y])))
            )
          ))
      (render-lines g)
      (render-drag-line g)
      ])])

(r/defc debug-game < r/reactive [g]
  "heads up game state display"
  [:div {:class "debug"}
   [:p {:key "b1"} (str g)]
   [:p {:key "b2"} (str (r/react drag-line))]
   [:p {:key "b3"} (str (r/react w-points))]
])

(r/defc tool-bar < r/reactive [g]
  (let [active (fn [g player-count]
                 (if (= player-count (:players g)) "active" ""))]
    [:div
     [:div {:class "btn-group toolbar"}
      [:button {:type "button" :class "btn btn-success" :key "bu1" :on-click down-tap :on-touch-end down-tap}
       [:span {:class "fa fa-chevron-down"}]]
      [:button {:type "button" :class "btn btn-success" :key "bu2" :on-click up-tap :on-touch-end up-tap}
       [:span {:class "fa fa-chevron-up"}]]
      [:button {:type "button" :class (str "btn btn-default " (active g 1)) :key "bu3" :disabled "true" :on-click one-player :on-touch-end one-player} "1 player"]
      [:button {:type "button" :class (str "btn btn-default " (active g 2)) :key "bu4" :on-click two-player :on-touch-end two-player} "2 player"]
      [:button {:type "button" :class "btn btn-info" :key "bu5" :on-click undo :on-touch-end undo}
       [:span {:class "fa fa-undo"}]]
      [:button {:type "button" :class "btn btn-info" :key "bu6" :on-click redo :on-touch-end redo}
       [:span {:class "fa fa-repeat"}]]
      ]]))

(defn get-status [g wps]
  (let [pa (= (:player g) :a)
        gover (game-over? g wps)
        over-class (if gover " pulsed" "")]
    (if (game-drawn? g)
      :draw
      (if (= (:players g) 1)
        [over-class (cond
                     (= gover :a) :al-win
                     (= gover :b) :you-win
                     :else (if pa :yours :als))]
        [over-class (cond
                     (= gover :a) :b-win
                     (= gover :b) :a-win
                     :else (if pa :as-turn :bs-turn))]))))

(defn get-message [status]
  (status messages))

(defn get-fill [status]
  ((status message-colours) colours))



(r/defc status-bar < r/reactive [g wps]
  (let [[over-class status] (get-status g wps)]
    [:div
     [:p {:class (str "status " over-class) :style {:background-color (get-fill status)} :key "b4"} (get-message status)
      [:button {:type "button" :class "btn btn-danger" :style {:display "inline" :clear "none" :float "right"} :key "bu7" :on-click reset-game :on-touch-end reset-game}
       [:span {:class "fa fa-refresh"}]]]]))

(r/defc rules []
  [:section {:style {:text-align "center"}}
   [:p {:style {
                :text-align "center"
                :font-size 18
                :color "#ffffff"
                }}
    "Last player able to move loses"]
   [:p {:style {:color "#ffffff"
                :font-size "14px"
                :padding "0px"
                }}
    "Click or drag a line horizontally, vertically or at 45 degrees to cross out dots. Lines must not cross."]
   [:p {:style {:color "#ffffff"
                :font-size "12px"
                :font-style "italic"
                }}
    "Kaxo is ©Alex Voak 2015. Used with permission."]
   ])

(r/defc game-over-modal < r/reactive []
  [:div {:class "modal-container"}
   [:div {:class "game-over-modal" :id "game-over" }
    [:h1 "Game Over"]
    [:p :style "" "Red won"]
    [:div {:class "modal-body"}]
    [:button {:type "button " :class (str "btn btn-lg btn-primary ")} "OK"]
    ]])

(r/defc game-container  < r/reactive []
  (let [g (r/react game)
        wps (r/react w-points)]
    [:section {:id "game-container"}
     [:div {:class "full-width"}
      [:p {:style {:color "white" :font-size "1.8em" :padding-top "8px"}} "Kaxo"]
      (tool-bar g)
      (status-bar g wps)]
     (svg-grid g)
     (rules)
     #_(goal g)
     #_(debug-game g)]))

(defn initialise []
  (.initializeTouchEvents js/React true)

  ;; mount main component on html game element
  (r/mount (game-container) (el "game"))
  )

(initialise)

(defn on-js-reload []

(swap! game update-in [:__figwheel_counter] inc))
