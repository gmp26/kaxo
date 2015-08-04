(ns ^:figwheel-always kaxo.core
    (:require [game.gestures :refer [bind-touch] :as gest] 
              [rum :as r]
              [cljs.reader :as reader]
              [clojure.set :refer (intersection)]
              [cljsjs.react]
              ))

(enable-console-print!)

(defn deb 
  ([x] 
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
(prn "-- Restart --")

(def initial-state {:n 5
                    :player :a
                    :players 1
                    :a-crosses #{}
                    :b-crosses #{}
                    :a-lines #{}
                    :b-lines #{}
                    ;:draw-line nil
                    })

(defonce game (atom initial-state))
(defonce draw-line (atom nil))

(def messages {:yours "Your turn"
               :als   "My turn"
               :as-turn "Player A's turn"
               :bs-turn "Player B's turn"
               :you-win "Well done! You won"
               :al-win "Oops! You lost"
               :a-win "Player A won"
               :b-win "Player B won"
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
(def max-n 9)
(def min-n 3)
(def unit 1)
(def gap 36)
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

(defn game-over? [g] false)

(defn get-ai-move [player] nil)

(defn computer-turn [g]
  #_(prn "play computer turn")
  (get-ai-move :b)
)

;;;;;;;;;;;;;;;;;;;;;
;;
;; Rendering
;;
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
                   :n n)))

(defn up-tap [event]
  "grow the game by 1 unit up to a max-n square"
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
  (swap! game #(assoc % :player :b :a-crosses (conj cross point))))

(defn claim-b-point [cross point]
  (swap! game #(assoc % :player :a :b-crosses (conj cross point))))

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

(defn single-player-point [g a-crosses b-crosses point] 
  (do
    (when (not (or (game-over? g) (game-drawn? g)))
      (claim-a-point a-crosses point))
    (let [newg @game]
      (when (not (or (game-over? newg) (game-drawn? newg)))
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
                 )))

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
                         )))
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

(defn line-data [[p1 p2]]
  "Line path data"
  (let [p2s #(str (gaps (first %)) " " (gaps (second %)))]
    (str "M " (p2s p1)
         " L " (p2s p2))) 
  )



(declare good-line?)

(r/defc render-draw-line < r/reactive [g]
  (let [[[x1 y1] [x2 y2] :as [p1 p2 :as line]] (r/react draw-line)]
    ; we don't want to render a line before the mouse moves as
    ; it prevents mouseup/touchend detection on the dot 
    ; preventing clicks/taps there.
    (when (not= p1 p2)
      [:line {:stroke-linecap "round"
              :opacity 0.5
              :stroke ((:player g) colours)
              :stroke-width 3
              :x1 (gaps x1)
              :y1 (gaps y1)
              :x2 (gaps x2)
              :y2 (gaps y2)}]))
  )

(r/defc render-line [g player [[x1 y1] [x2 y2] :as line]]
  [:line {:stroke-linecap "round"
          :opacity 0.5
          :stroke (line-colour g line)
          :stroke-width 3
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

(defn touchXY [event]
  (let [touch (aget (.-changedTouches event) 0)
        page [(.-pageX touch) (.-pageY touch)]]
    #_(.log js/console (str " page " page))
    page
    ))

(defn mouseXY [event]
  (let [page [(.-pageX event) (.-pageY event)]]
    #_(.log js/console (str " page " page))
    page
))

(defn eventXY [event]
  (if (and 
       (= (subs (.-type event) 0 5) "touch") 
       (nil? (aget (.-touches event) 0) ))
    prn (.-type event))
  (let [type (subs (.-type event) 0 5)
        result (condp = type 
                 "mouse" ["mouse" (mouseXY event)]
                 "touch" ["touch " (touchXY event)]
                 [0 0]
                 )]
    #_(prn result)
    (second result))
)

(defn svg-element-width [el]
  (.. el -width -baseVal -value))

(defn svg-element-height [el]
  (.. el -height -baseVal -value))

;;;;
;;
;; breakdown into individual transforms
;;
(defn scale [factor]
  (fn [[x y]] [(* factor x) (* factor y)]))

(defn translate [offset-left offset-top]
  (fn [[x y]] [(+ offset-left x) (+ offset-top y)]))

(defn rotate [theta]
  (let [c (Math.cos theta)
        s (Math.sin theta)]
    (fn [[x y]] [(- (* x c) (* y s)) (+ (* x s) (* y c))])))

(defn svg-distances [svg-el]
  (let [offset-left (.-offsetLeft svg-el)
        offset-top (.-offsetTop svg-el)        
        offset-width (.-offsetWidth svg-el)
        view-box (.. svg-el -viewBox -animVal)
        x (.-x view-box)
        y (.-y view-box)
        width (.-width view-box)
        height (.-height view-box)
        ]
    [offset-left offset-top offset-width x y width height]))

(defn viewport->mouse [svg-el]
  (let [[offset-left offset-top offset-width x y width height] (svg-distances svg-el)]
    (comp 
     (translate offset-left offset-top)
     (scale (/ offset-width width))
     (translate (- x) (- y))
     )))

(defn mouse->viewport [svg-el]
  (let [[offset-left offset-top offset-width x y width height] (svg-distances svg-el)]
    (comp 
     (translate x y)
     (scale (/ width offset-width))
     (translate (- offset-left) (- offset-top))
     )))

;;;;;;;;


(defn viewport->game [g]
  (comp
   (fn [[x y]] [(spag x) (spag y)])
   (scale (/ (:n g) max-n ))
))

(defn game->viewport [g]
  (comp
   (scale (/ max-n (:n g))) 
   (fn [[x y]] [(gaps x) (gaps y)])))

(defn game->mouse [g svg-el]
  (comp
   (viewport->mouse svg-el)
   (game->viewport g)
   ))

(defn mouse->game [g svg-el]
  (comp
   (viewport->game g)
   (mouse->viewport svg-el)))

(defn mouse->dot [g svg-el]
  (comp
   (fn [[x y]] [(.round js/Math x) (.round js/Math y)])
   (mouse->game g svg-el)
   ))

(defn handle-tap [event]
  (let [p (reader/read-string (.. event -target -id))
        g @game
        a-crosses (:a-crosses g)
        b-crosses (:b-crosses g)
        pl (:player g)]
    (do 
      (.preventDefault event)
      (if (= (:players g) 2)
        (claim-point a-crosses b-crosses p pl)
        (when (= pl :a)
          (single-player-point g a-crosses b-crosses p))))))

(defn handle-start-line [event]
  (let [pointer (eventXY event)
        dot ((mouse->dot @game (el "grid")) pointer)]
    (reset! draw-line [dot dot])
))

(defn handle-move-line [event]
  (let [pointer (eventXY event)
        g @game
        dl @draw-line
        game-pointer ((mouse->game g (el "grid")) pointer)]
    (when dl
      (reset! draw-line [(first dl) game-pointer])
)))

(defn canonical-line [[p1 p2 :as line]]
  (if (or (< (first p1) (first p2))
          (and (= (first p1) (first p2))
               (< (second p1) (second p2)))
          )
    line
    [p2 p1]))

(defn good-slope? [[p1 p2]]
  "Points that a line crosses"
  (let [dx (- (first p1) (first p2))
        dy (- (second p1) (second p2))
        ]
    (or (= 0 dx) (= 0 dy) (= dx dy) (= dx (- dy))))
)

(defn good-line? [[p1 p2 :as line]]
  (and (not= p1 p2) (good-slope? line))
  )

(defn handle-end-line [event]
  (let [pointer (eventXY event)
        g @game
        draw-start (first @draw-line)
        dot ((mouse->dot g (el "grid")) pointer)
        line (canonical-line [draw-start dot])
        ]
    (when (good-line? line)
      (let [line-key (if (= :a (:player g)) :a-lines :b-lines)
            updated-lines (conj (line-key g) line)]
        (swap! game #(assoc % line-key updated-lines))
        ))
    (reset! draw-line nil)
    ))


(r/defc svg-dot < r/reactive [n x y fill]
  (let [p [x y]
        g (r/react game)
        the-class #(if ((:a-crosses g) p)  "dot claimed" "dot")
        ]
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
              :on-click handle-tap
              :on-touch-end handle-tap
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
         :on-touch-end handle-move-line
         :on-touch-move handle-end-line
         :style {:display-mode "inline-block"}
         }
   (let [n (:n g)]
     [:g 
      [:rect {:stroke "black" 
              :stroke-width 1 
              :fill "none" 
              :x 0 :y 0 
              :width viewport-width 
              :height viewport-height}]
      [:g {:id "box" :transform (str "scale(" (* 1 (/ max-n n)) ")")}
       (for [x (range n)]
         (for [y (range n)]
           (if (or ((:a-crosses g) [x y]) ((:b-crosses g) [x y]))
             (render-cross g [x y])                
             (svg-dot n x y (cross-colour g [x y])) 
             )
           ))
       (render-lines g)
       (render-draw-line g)
       ]])])

(r/defc debug-game < r/reactive [g]
  "heads up game state display"
  [:div
   [:p {:key "b1"} (str (dissoc g :squares))]
   [:p {:key "b2"} (str (r/react draw-line))]])



(r/defc tool-bar < r/reactive [g]
  (let [active (fn [g player-count]
                 (if (= player-count (:players g)) "active" ""))]

    [:div
     [:div {:class "btn-group toolbar"}
      [:button {:type "button" :class "btn btn-warning" :key "bu1" :on-click down-tap :on-touch-end down-tap} 
       [:span {:class "fa fa-chevron-down"}]]
      [:button {:type "button" :class "btn btn-warning" :key "bu2" :on-click up-tap :on-touch-end up-tap} 
       [:span {:class "fa fa-chevron-up"}]]
      [:button {:type "button" :class (str "btn btn-default " (active g 1)) :key "bu4" :on-click one-player :on-touch-end one-player} "1 player"]
      [:button {:type "button" :class (str "btn btn-default " (active g 2)) :key "bu5" :on-click two-player :on-touch-end two-player} "2 player"]
      [:button {:type "button" :class "btn btn-danger" :key "bu3" :on-click reset-game :on-touch-end reset-game} 
       [:span {:class "fa fa-refresh"}]]]]))

(defn get-status [g]
  (let [pa (= (:player g) :a)
        gover (game-over? g)]
    (if (game-drawn? g)
      :draw
      (if (= (:players g) 1)
        (if gover
          (if pa :al-win :you-win)
          (if pa :yours :als))
        (if gover
          (if pa :b-win :a-win)
          (if pa :as-turn :bs-turn))))))

(defn get-message [status]
  (status messages))

(defn get-fill [status]
  ((status message-colours) colours))

(r/defc status-bar < r/reactive [g]
  (let [status (get-status g)]
    [:p {:class "status" :style {:background-color (get-fill status)} :key "b4"} (get-message status)]))

(r/defc rules []
  [:p {:style {
               :text-align "center"
               :font-size 24
               :color "#888"
               }}
   "Last player able to move wins"])

(defn random-dark-colour []
  (let [dark #(+ 100 (rand-int 70))]
    (str "rgb(" (dark) "," (dark) "," (dark) ")"))) 

(def dark-rgb (random-dark-colour))

#_(r/defc goal [g]
  (let [n (:n g)]
    [:p {:style {
                 :text-align "center"
                 :font-size 24
                 :color "#fff"
                 :padding "5px"
                 :background-color dark-rgb 
                 }}
     (condp = n
         3 "Can blue lose?"
         4 "Can blue lose in 4 moves?"
         5 "Can blue always win?"
         6 "Can blue force a draw?"
         7 "Can blue always lose?"
         8 "Can blue always win?"
         9 "Can blue always win on an inifinite board?"
         )
     ]))

(r/defc game-container  < r/reactive []
  (let [g (r/react game)]
    [:section
     [:div {:class "full-width"}
      [:h1 "Kaxo!"]
      (tool-bar g)
      (status-bar g)]
     (svg-grid g)
     (rules)
     #_(goal g)
     (debug-game g)]))

(defn initialise []
  (.initializeTouchEvents js/React true)

  ;; mount main component on html game element
  (r/mount (game-container) (el "game"))
  )

(initialise)

;;
;; optionally do something on game reload
;;
(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc))
