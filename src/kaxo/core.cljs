(ns ^:figwheel-always kaxo.core
    (:require [rum :as r]
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
                    :a-first true
                    :as #{}
                    :bs #{}
                    })

(defonce game (atom initial-state))

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

(def bound-width 320)
(def bound-height 320)
(def max-n 9)
(def min-n 3)
(def unit 1)
(def gap 36)
(defn units [x] (* x unit))
(defn gaps [x] (* (units (+ x 0.5)) gap))
(def tick-interval 1000)
(def al-think-time 2000)


(def player-colours {:a "rgb(0, 153, 255)"
                     :b "rgb(238, 68, 102)"
                     :none "rgb(220,255,220)"
                     :draw "rgb(74, 157, 97)"
                     })

(def player-class {:a "blue"
                   :b "red"
                   :none "grey"
                   :draw "draw"
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
(defn fill-color [g p]
  (if ((:as g) p) 
    (:a player-colours)
    (if ((:bs g) p)
      (:b player-colours)
      (:none player-colours))))

(defn dot-sep [n]
  (Math.floor (/ 300 n)))

(defn px [n len]
  (str (* len (dot-sep n)) "px"))

;;
;; reset the game initially
;;
(declare reset-game)

(defn up-tap [event]
  "grow the game by 1 unit up to a max-n square"
  (.stopPropagation event)
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (< old-n max-n) (inc old-n) max-n)]
    (swap! game #(assoc % :as #{} :bs #{}
                         :n new-n))))

(defn down-tap [event]
  "shrink the game by 1 unit down to a min-n square"
  (.stopPropagation event)
  (.preventDefault event)
  (let [old-n (:n @game)
        new-n (if (> old-n min-n) (- old-n 1) min-n)]
    (swap! game #(assoc % :as #{} :bs #{}
                         :n new-n))))

(defn claim-a-point [as point]
  (swap! game #(assoc % :player :b :as (conj as point))))

(defn claim-b-point [bs point]
  (swap! game #(assoc % :player :a :bs (conj bs point))))

(defn claim-point [as bs point player]
  (if (and (not (as point)) (not (bs point)))
    (if (= player :a)
      (claim-a-point as point)
      (claim-b-point bs point))))

(declare timeout)

#_(defn single-player-point [g as bs point] 
  (when (not (game-over? g)) (claim-a-point as point))
  (when (not (game-over? @game))
    (timeout al-think-time #(->> @game
                                 (computer-turn)
                                 (claim-b-point bs))))
)


;;
;; events
;;

(defn timeout [ms f & xs]
  "Call f, optionally with arguments xs, after ms milliseconds"
  (js/setTimeout #(apply f xs) ms))

(defn single-player-point [g as bs point] 
  (do
    (when (not (or (game-over? g) (game-drawn? g)))
      (claim-a-point as point))
    (let [newg @game]
      (when (not (or (game-over? newg) (game-drawn? newg)))
        (timeout al-think-time #(->> newg
                                     (computer-turn)
                                     (claim-b-point bs)))))))

(defn handle-tap [event]
  (let [p (reader/read-string (.. event -target -id))
        g @game
        as (:as g)
        bs (:bs g)
        pl (:player g)]
    (do 
      (.stopPropagation event)
      (.preventDefault event)
      (if (= (:players g) 2)
        (claim-point as bs p pl)
        (when (= pl :a)
          (single-player-point g as bs p))))))

(defn one-player [event]
  (.stopPropagation event)
  (.preventDefault event)
  (swap! game #(assoc % :players 1 :player :a :as #{} :bs #{})))

(defn two-player [event]
  (.stopPropagation event)
  (.preventDefault event)
  (swap! game #(assoc % :players 2 :player :a :as #{} :bs #{})))

(defn reset-game 
  ([]
   (swap! game #(assoc % :player :a
                         :as #{}
                         :bs #{})))
  ([event] 
   (.preventDefault event) 
   (reset-game))
  ([event _]
   (reset-game event)))

(defn start-game [] (reset-game))


;;
;; rendering game board
;;
(r/defc svg-dot < r/reactive [n x y fill]
  (let [p [x y]
        g (r/react game)
        the-class #(if (or ((:as g) p) ((:bs g) p)) "dot claimed" "dot")
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

(declare get-status)
(declare get-fill)

(defn drawn-line [g]
  "Line path data"
  (let [p2s #(str (gaps (first %)) " " (gaps (second %)))]
    (str "M " (p2s [0 0])
         " L " (p2s [1 1]))) 
  )


(r/defc svg-grid < r/reactive [g]
  [:section {:key "b3" :style {:height "60%"}}
   [:svg {:view-box (str "0 0 " bound-width " " bound-height) 
          :height "100%"
          :width "100%"
          :key "b3"}
    (let [n (:n g)] 
      [:g {:id "box" :transform (str "scale(" (* 1 (/ max-n n)) ")")}
       (for [x (range n)]
         (for [y (range n)]
           (svg-dot n x y (fill-color g [x y])) ))
       (when (and (game-over? g) (not (game-drawn? g))) 
         [:g
          [:path {
                  :d (drawn-line g)
                  :fill "none" ;;(get-fill (get-status g))
                  :opacity 0.5
                  :stroke (get-fill (get-status g))
                  :stroke-width "3"
                  }]
          ]
         )
       ])]])

(r/defc debug-game < r/reactive [g]
  "heads up game state display"
  [:p {:key "b1"} (str (dissoc g :squares))])



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
  ((status message-colours) player-colours))

(r/defc status-bar < r/reactive [g]
  (let [status (get-status g)]
    [:p {:class "status" :style {:background-color (get-fill status)} :key "b4"} (get-message status)]))

(r/defc rules []
  [:p {:style {
               :text-align "center"
               :font-size 24
               :color "#888"
               }}
   "Claim all 4 corners of a square to win"])

(defn random-dark-colour []
  (let [dark #(+ 100 (rand-int 70))]
    (str "rgb(" (dark) "," (dark) "," (dark) ")"))) 

(def dark-rgb (random-dark-colour))

(r/defc goal [g]
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
     (goal g)
     #_(debug-game g)]))


;;
;; mount main component on html game element
;;
(r/mount (game-container) (el "game"))


;;
;; optionally do something on game reload
;;
(defn on-js-reload []
  (swap! game update-in [:__figwheel_counter] inc))
