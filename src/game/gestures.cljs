(ns ^:figwheel-always game.gestures
  (:require 
   [cljs.core.async :as async
    :refer [<! >! chan close! put! alts!]]
   [clojure.string :refer [join blank? replace-first]]
   )
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

;;
;; TODO
;; * Use React-wrapped events rather than jayq
;; * remove any crate or jayq dependencies
;; * coordinate transforms: device pointer -> board space
;; * create higher-level signals from raw events
;;
(enable-console-print!)

(println "Game gestures loaded.")

(def touch-chan (chan))


;; document.elementFromPoint(100,100)


(defn bind-touch [event]
  (.preventDefault event)
  (let [touch (aget (.. event -touches) 0)
        target (.-target event)
        clientX (.. touch -clientX)
        clientY (.. touch -clientY)
        pageX (.. touch -pageX)
        pageY (.. touch -pageY)
        offset-top (.. target -offsetTop)
        offset-left (.. target -offsetLeft)
        offset-width (.. target -offsetWidth)
        offset-height (.. target -offsetHeight)
        el (.elementFromPoint js/document clientX clientY)
        ]
    (prn el)
    (println "clientX:" clientX "clientY:" clientY)
    (println "offsets: " [offset-top offset-left offset-width offset-height])))

(defn containerXY [[offset-top offset-left offset-height offset-width] [x y]]
  [(/ (- x offset-left) offset-width) (/ (- y offset-top) offset-height)])


(defn select-chan [pred chans]
  (go (loop []
        (let [[value ch] (alts! chans)]
          (if (pred value) value (recur))))))

(defn get-drawing [input-chan out-chan]
  (go (loop [msg (<! input-chan)]
        (put! out-chan msg)
        (if (= (first msg) :draw)
          (recur (<! input-chan))))))

(def drawstart-chan (chan))
(def drawend-chan (chan))
(def drawer-chan (chan))

(defn draw-chan [selector]
  (let [input-chan (chan)
        out-chan   (chan)]
    (drawstart-chan input-chan selector)
    (drawend-chan   input-chan selector)
    (drawer-chan    input-chan selector)
    (go (loop [[msg-name _ :as msg] (<! input-chan)]
          (when (= msg-name :drawstart)
            (put! out-chan msg)
            (<! (get-drawing input-chan out-chan)))
          (recur (<! input-chan))))
    out-chan))

#_(defn click-chan [selector msg-name]
  (let [rc (chan)
        handler (fn [e] (jq/prevent e) (put! rc [msg-name]))]
    (on ($ "body") :click selector {} handler)
    (on ($ "body") "touchend" selector {} handler)
    rc))

#_(defn mouseevent-chan [rc selector event msg-name]
  (bind ($ selector) event
        #(do
           (put! rc [msg-name {:x (.-pageX %) :y (.-pageY %)}]))))

#_(defn touchevent-chan [rc selector event msg-name]
  (bind ($ selector) event
        #(let [touch (aget (.-touches (.-originalEvent %)) 0)]
           (put! rc [msg-name {:x (.-pageX touch) :y (.-pageY touch)}]))))

#_(defn drawstart-chan [ichan selector]
  (mouseevent-chan ichan selector "mousedown" :drawstart)
  (touchevent-chan ichan selector "touchstart" :drawstart))

#_(defn drawend-chan [ichan selector]
  (mouseevent-chan ichan selector "mouseup" :drawend)
  (mouseevent-chan ichan selector "touchend" :drawend))

#_(defn drawer-chan [ichan selector]
  (mouseevent-chan ichan selector "mousemove" :draw)
  (touchevent-chan ichan selector "touchmove" :draw))


