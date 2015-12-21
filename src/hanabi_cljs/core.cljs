(ns hanabi-cljs.core
  (:require [cljs.reader :as reader]
            [clojure.string :refer [split]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

;; A realization: This will eventually be live multiplayer, using socket.io
;; So app state will not be here, but on the server
;; However, we should be able to move it over directly to a clojure server
;; we just need to update the front-end to call events

;; for react's complaining
(defn react-key-index [xs]
  (map-indexed (fn [i item] (assoc item :key i)) xs))

(defn index-cards [cards]
  (react-key-index (map #(assoc {} :card % :know-colour false :know-number false) cards)))

;; from http://stackoverflow.com/questions/25324082/index-of-vector-in-clojurescript
(defn index-of [coll v]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll))
            (= v (last coll)))
      i)))

(defn index-of-similar [coll v]
  (index-of (map #(select-keys % (keys v)) coll) v))

(defn colour-number-to-card [colour number]
  (keyword (str (name colour) "-" number)))

(defn card-to-colour-number [card]
  (let [[colour number] (split (name card) #"-")]
    {:colour (keyword colour) :number (int number)}))

(defn colour-of-card [card]
  (:colour (card-to-colour-number card)))

(defn number-of-card [card]
  (:number (card-to-colour-number card)))

;; need to take into account weighting
(defn generate-deck [colors ns]
  ;; first make a deck
  ;; then shuffle it
  (let [deck (for [c colors [x n] ns]
                  (for [_ (range n)] (colour-number-to-card c x)))]
    (shuffle (flatten deck))))

(defn make-stacks [colours]
  (reduce #(assoc %1 %2 []) {} colours))

(def colours [:red :blue :white :green :yellow])
(def number-weights [[1 3] [2 2] [3 2] [4 2] [5 1]])

;; define your app data so that it doesn't get over-written on reload
(defonce app-state
  (atom
    ;;players
    {:players {
      :player-1 {:name "Player 1" :cards [] :known-colours #{} :known-numbers #{} :position "top left"}
      :player-2 {:name "Player 2" :cards [] :known-colours #{} :known-numbers #{} :position "top right"}
      :player-3 {:name "Player 3" :cards [] :known-colours #{} :known-numbers #{} :position "bottom left"}
      :player-4 {:name "Player 4" :cards [] :known-colours #{} :known-numbers #{} :position "bottom right"}
     }
     :name-to-id {
      "Player 1" :player-1
      "Player 2" :player-2
      "Player 3" :player-3
      "Player 4" :player-4
     }
     :me :player-1
     ;; simple variables
     :lives 3
     :game-over false
     ;; info
     :info 8
     :info-mode :neither
     ;; cards
     :stacks (make-stacks colours)
     :discard []
     :deck (generate-deck colours number-weights)
     }
  ))

;; getters
(defn get-id-from-name [name]
  (get-in @app-state [:name-to-id name]))

;; check if it is valid to play this card
(defn valid-play? [card]
  (let [colour (colour-of-card card)
        number (number-of-card card)
        stack (get-in @app-state [:stacks colour])]
      (or
        ;; the stack is empty, and we're adding a 1
        (and (= number 1) (empty? stack))
        ;; or the number on the stack is one less than the card we are playing
        (and (not (empty? stack)) (= (dec number) (number-of-card (peek stack)))))
      ))

(defn get-info-mode []
  (:info-mode @app-state))

(defn me? [player-id]
  (= (:me @app-state) player-id))

(defn player-knows-colour? [player-id card]
  (contains? (get-in @app-state [:players player-id :known-colours]) (colour-of-card card)))
(defn player-knows-number? [player-id card]
  (contains? (get-in @app-state [:players player-id :known-numbers]) (number-of-card card)))

;; setters
(defn set-player-name [player-id newname]
  (let [oldname (get-in @app-state [:players player-id :name])]
    (swap! app-state
      (fn [state]
        (update
          (assoc-in state [:players player-id :name] newname)
          :name-to-id   #(dissoc (assoc % newname player-id) oldname)
            )))))

;; remove a life
(defn remove-life []
  (swap! app-state
    #(update % :lives dec)))

;; remove an info token
(defn remove-info []
  (swap! app-state
    #(update % :info dec)))

;; add an info token
(defn add-info []
  (swap! app-state
    #(update % :info inc)))

(defn set-info-mode [mode]
  (swap! app-state
    #(assoc % :info-mode mode)))

;; remove a card from play
(defn discard [card]
  (swap! app-state
    #(update % :discard conj card)))

(defn pop-deck []
  (let [card (peek (:deck @app-state))]
    (do
      (swap! app-state #(update % :deck pop))
      card)))

(defn deal-card-to [player]
  (let [card (pop-deck)]
    (swap! app-state
      #(update-in % [:players player :cards] conj card))))

;; deal [n] cards to each player
(defn deal-start-cards [n]
  (let [players (keys (:players @app-state))]
    (doseq [_ (range n) player players]
      (deal-card-to player)
        )))

(defn remove-card-from-player [player card]
  (swap! app-state
    (fn [state]
      (update-in state [:players player :cards] (fn [v] (filterv #(not (= card %)) v)))
        )))

(defn append-card-to-stacks [card]
  (swap! app-state
    #(update-in % [:stacks (colour-of-card card)] conj card)
        ))

(defn play-card [player-id card]
  (do
    (remove-card-from-player player-id card)
    (deal-card-to player-id)
    (if (valid-play? card) (append-card-to-stacks card) (do (remove-life) (discard card)))
  ))

(defn discard-card [player-id card]
  (do
    (remove-card-from-player player-id card)
    (deal-card-to player-id)
    (discard card)
    (add-info)
    ;; check if game over
  ))

;; crap, we need a way to filter out cards that we already have
;; or, we should just give them an option to mark it?
(defn tell-info-to [player card]
  (let [info (:info @app-state)
        mode (:info-mode @app-state)
        info-key (if (= mode :colour) :known-colours :known-numbers)
        info-fn (if (= mode :colour) colour-of-card number-of-card)]
    (if (and (not (me? player)) (> info 0))
      (swap! app-state
        (fn [state]
          (update-in (assoc (update state :info dec) :info-mode :neither) [:players player info-key] conj (info-fn card))))
      '())))

;; deal 5 cards to each player
(defn on-card-drag [data]
  (fn [e]
    (do
      (set! (.-effectAllowed (.-dataTransfer e)) "move")
      (.setData (.-dataTransfer e) "card-drag" (str data)))))

(defn on-card-click [player card]
  (fn [e]
    (tell-info-to player card)))

;; each card as a colour, and a number
(defn card-view [{ :keys [player card know-colour know-number] }]
  (let [colour (colour-of-card card)
        number (number-of-card card)]
    (dom/div #js {:draggable (me? player)
           :onDragStart (on-card-drag [player card])
           :className (str "card " (if know-colour (name colour) "unknown") " no-select")
           :onClick (on-card-click player card)}
      (dom/div #js {:style #js {:position "relative" :height "100%"} :className "flex-column"}
        (dom/p #js {:className "absolute top left big-text"}
          (dom/span #js {:className "number-wrapper"} (if know-number number)))
        (dom/p #js {:className "absolute bottom right big-text"}
          (dom/span #js {:className "number-wrapper" } (if know-number number)))
      )
    )))

;; each player has some cards
(defn player-view [[id { :keys [cards name position known-colours known-numbers] }]]
  (let [not-me (not (me? id))]
    (dom/div #js {:className (str "player " (if (= (:me @app-state) id) "hidden " "") position)}
      (dom/div #js {:className "flex-column"}
        (dom/p nil name)
        (apply dom/div #js {:className "cards"}
          (map
            #(card-view (assoc {} :player id
              :card %
              :know-colour (or (player-knows-colour? id %) not-me)
              :know-number (or (player-knows-number? id %) not-me)))
            cards))
        )
      )
    ))

;; each stack
(defn stacks-to-react []
  (map #(assoc {} :cards (second %) :colour (name (first %))) (:stacks @app-state)))

(defn stack-view [{:keys [cards colour]}]
  (dom/div #js {:className "stack"}
    (dom/div nil
      (dom/p nil (str colour))
      (apply dom/div #js {:className "cards"}
        (map-indexed
          #(dom/div #js {:style #js {:position "absolute" :top (str (* 8 %1) "px")}}
            (card-view (assoc {} :card %2 :know-number true :know-colour true)))
          cards))
    )
  ))

(defn on-stack-drop [e]
  (let [[player card] (reader/read-string (.getData (.-dataTransfer e) "card-drag"))]
    (play-card player card)))

(defn on-click-colour []
  (set-info-mode :colour))

(defn on-click-number []
  (set-info-mode :number))

(defn on-discard-drop [e]
  (let [[player card] (reader/read-string (.getData (.-dataTransfer e) "card-drag"))]
    (discard-card player card)))

;; the main component
(defn hanabi-view [data]
  (dom/div #js {:className "game" }
    (dom/button #js {:onClick on-click-colour :className (if (= (get-info-mode) :colour) "active-mode-button" "mode-button")} "Give Colour")
    (dom/button #js {:onClick on-click-number :className (if (= (get-info-mode) :number) "active-mode-button" "mode-button")} "Give Number")
    (dom/p #js {:className "inline-block"} (str "Lives: " (:lives data)))
    (dom/p #js {:className "inline-block"}  (str "Info: " (:info data)))

    (dom/div #js {:className "board flex-column no-select"}
      ;; deck / discard
      (dom/div #js {:className "deck-list"}
        (dom/div #js {:className "deck" :onDragOver #(.preventDefault %) :onDrop on-discard-drop }
          (dom/p #js {:className "title"} "Discard")
          (apply dom/div #js {:className "container"}
            (map #(card-view (assoc {} :card % :know-number true :know-colour true)) (:discard data)))
        )
        (dom/div #js {:className "deck"}
          (dom/p #js {:className "title"} "Deck")
          (apply dom/div #js {:className "container"}
            (map #(card-view (assoc {} :card % :know-number true :know-colour true)) (:deck data)))
        )
      )

      ;; stacks
      (apply dom/div
        #js {:className "stacks" :onDragOver #(.preventDefault %) :onDrop on-stack-drop }
        (map stack-view (stacks-to-react))
      )

      ;; player
      (apply dom/div nil
        (map player-view (:players data)))
    )
  )
)

(defonce __start__ (deal-start-cards 5))
(om/root
  (fn [data owner]
    (om/component
      (hanabi-view data)
    ))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
