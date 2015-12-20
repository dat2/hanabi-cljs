(ns hanabi-cljs.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state
  (atom
    {:text "Hello world!"
     :players [
      {:name "a" :cards [{:number 1 :colour "red"} {:number 4 :colour "blue"}]}
      {:name "b" :cards [{:number 4 :colour "white"} {:number 2 :colour "red"}]}
      {:name "c" :cards [{:number 2 :colour "yellow"} {:number 5 :colour "green"}]}
      {:name "d" :cards [{:number 3 :colour "green"} {:number 1 :colour "red"}]}
     ]
     :center []
     :discard []
     }))

(defn index-list [xs]
  (map-indexed (fn [k item] (assoc item :key k)) xs))

(defn cardclick [number colour]
  (fn [e]
    (println number colour)))

;; each card as a colour, and a number
(defn <card> [{ :keys [number, colour] }]
  [:div.card {:class colour :on-click (cardclick number colour)}
    [:div.flex-column
      [:p.big-text number]
      [:p.big-text colour]
    ]
  ])

;; each player has some cards
(defn <player> [{ :keys [cards name] }]
  [:div.player
    [:div.flex-column
      [:p name]
      [:div.cards
        (for [card (index-list cards)]
          [<card> card])]
    ]
  ])

(defn <hanabi> []
  [:div.game
    [:h1 (:text @app-state)]
    (for [player (index-list (:players @app-state))]
      [<player> player]
      )
  ])

(reagent/render-component [<hanabi>]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
