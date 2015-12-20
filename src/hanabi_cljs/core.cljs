(ns hanabi-cljs.core
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.reader :as reader]))

(enable-console-print!)

;; for react's complaining
(defn react-key-index [xs]
  (map-indexed (fn [k item] (assoc item :key k)) xs))

(defn find-pred [pred coll]
  (first (filter pred coll)))

;; from http://stackoverflow.com/questions/25324082/index-of-vector-in-clojurescript
(defn index-of [coll v]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll))
            (= v (last coll)))
      i)))

(defn index-of-similar [coll v]
  (index-of (map #(select-keys % (keys v)) coll) v))

;; need to take into account weighting
(defn generate-deck [colors ns]
  ;; first make a deck
  ;; then shuffle it
  (let [deck (for [c colors [x n] ns]
                  (for [_ (range n)] {:number x :colour c}))]
    (shuffle (flatten deck))))

(defn make-stacks [colours]
  (for [colour colours]
    {:colour colour :cards []}))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state
  (atom
    {:players {
      :player-1 {:name "Player 1" :cards [] :position "top left"}
      :player-2 {:name "Player 2" :cards [] :position "top right"}
      :player-3 {:name "Player 3" :cards [] :position "bottom left"}
      :player-4 {:name "Player 4" :cards [] :position "bottom right"}
     }
     :me :player-3
     :stacks (make-stacks ["red" "blue" "white" "green" "yellow"])
     :discard []
     :deck (generate-deck ["red" "blue" "white" "green" "yellow"] [[1 3] [2 2] [3 2] [4 2] [5 1]])
     }))

(defn find-player-with-name [name]
  (find-pred #(= (:name (second %)) name) (:players @app-state)))

(println )

(defn check-valid-move [card]
  true)

;; update app state, to take the top card off the deck and give it to the player
(defn deal-card-to [player]
  (swap! app-state
    (fn [state] (let [card (peek (:deck @app-state))]
      (update-in (update state :deck pop) [:players player :cards] conj card)))))

;; deal [n] cards to each player
(defn deal-start-cards [n]
  (let [players (keys (:players @app-state))]
    (doseq [_ (range n) player players]
      (deal-card-to player))))

(defn try-play-card [{ :keys [player number colour] }]
  (let [[player-keyword real-player] (find-player-with-name player)
        cardIndex (index-of-similar (:cards real-player) {:number number :colour colour})]
    (swap! app-state
      (fn [state]
        (update-in state [:players player-keyword :cards] #(split-at cardIndex %))))))

;; deal 5 cards to each player
(deal-start-cards 5)

(defn on-card-drag [data]
  (fn [e]
    (do
      (set! (.-effectAllowed (.-dataTransfer e)) "move")
      (.setData (.-dataTransfer e) "card-drag" (str data)))))

;; each card as a colour, and a number
(defn <card> [{ :keys [number colour player know-colour know-number] }]
  [:div.card {:class (str (if know-colour colour colour))
              :draggable true
              :on-drag-start (on-card-drag {:player player :number number :colour colour}) }
    [:div.flex-column.big-text
      [:p {:style { :display (if know-number "" "") } } (if know-number number number)]
    ]
  ])

;; each player has some cards
(defn <player> [{ :keys [cards name position] }]
  [:div.player {:class (str (if (= (:me @app-state) (keyword name)) "hidden" "") position)}
    [:div.flex-column
      [:p name]
      [:div.cards
        (for [card (react-key-index cards)]
          [<card> (assoc card :player name ) ])]
    ]
  ])

;; each stack
(defn <stack> [{ :keys [colour cards] }]
  [:div.stack {:class (str (if (= (:me @app-state) (keyword name)) "hidden" "") position)}
    [:div.flex-column
      [:p colour]
      [:div.cards (map #([<card> %]) (react-key-index cards)) ]
    ]
  ])

(defn on-stack-drop [e]
  (try-play-card (reader/read-string (.getData (.-dataTransfer e) "card-drag"))))

;; the main component
(defn <hanabi> []
  [:div.game
    [:button "Give Colour"]
    [:button "Give Number"]
    [:div.board.flex-column
    ;; decks
      [:div.deck-list
        [:div.deck
          [:p.title "Discard"]
          (for [card (react-key-index (:discard @app-state))]
            [<card> card])
        ]
        [:div.deck
          [:p.title "Deck"]
          (for [card (react-key-index (:deck @app-state))]
            [<card> card])
        ]
      ]
    ;; stacks
    [:div.stacks { :on-drag-over #(.preventDefault %) :on-drop on-stack-drop }
      (for [stack (react-key-index (:stacks @app-state))]
        [<stack> stack])
    ]
    ;; player
      (for [player (react-key-index (map second (seq (:players @app-state))))]
        [<player> player])
    ]
  ])

(reagent/render-component [<hanabi>]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
