(ns firestone.client.kth.mapper
  (:require [clojure.spec.alpha :as s]
            [firestone.construct :as construct]
            [ysera.test :refer [is]]
            [firestone.client.kth.spec]
            [firestone.definitions :as def]
            [firestone.core :refer [get-attack]]))


(defn check-spec
  [spec value]
  (or (s/valid? spec value)
      (s/explain spec value)))

(defn hero->client-hero
  {:test (fn []
           (let [game (construct/create-game)]
             (is (check-spec :firestone.client.kth.spec/hero
                             (hero->client-hero game
                                                (construct/get-hero game "p1")
                                                "p1")))))}
  [game hero player-id]
  {:armor            0
   :owner-id         player-id
   :entity-type      :hero
   :attack           0
   :can-attack       false
   :health           (- (:health hero) (:damage-taken hero))
   :id               (:id hero)
   :mana             (construct/get-mana game player-id)
   :max-health       30
   :max-mana         (construct/get-max-mana game player-id)
   :name             (:name hero)
   :states           []
   :valid-attack-ids []
   :hero-power       {:name               (:name (:hero-power hero))
                      :description        (:description (:hero-power hero))
                      :entity-type        :hero-power
                      :mana-cost          (:mana-cost (:hero-power hero))
                      :original-mana-cost (construct/get-mana-cost (:name (:hero-power hero)))
                      :can-use            (construct/hero-can-use-power? game player-id hero)
                      :owner-id           (:id (construct/get-hero game player-id))
                      :has-used-your-turn (:has-used-your-turn hero)
                      :valid-target-ids   (construct/get-valid-target-ids game player-id hero)}})

(defn minion->client-minion
  {:test (fn []
           (let [game (construct/create-game)
                 minion (construct/create-minion "Sheep" :id "p")
                 new-game (construct/add-minion-to-board game "p1" minion 0)]

             (is (check-spec :firestone.client.kth.spec/minion
                             (minion->client-minion new-game
                                                    (construct/get-minion new-game "p")
                                                    )))))}
  [game minion]
  {:damage-taken                (:damage-taken minion)
   :entity-type                 :minion
   :name                        (:name minion)
   :added-to-board-time-id      (:added-to-board-time-id minion)
   :attacks-performed-this-turn (:attacks-performed-this-turn minion)
   :id                          (:id minion)
   :sleepy                      (or (:sleepy minion) false)
   :owner-id                    (:owner-id minion)
   :position                    (:position minion)
   :mana-cost                   (construct/get-mana-cost (:name minion))
   :original-attack             (construct/get-original-attack (:name minion))
   :original-health             (construct/get-original-health (:name minion))
   :description                 (:description minion)
   :states                      (if (seq (:states minion))
                                  (vec (remove nil? (:states minion)))
                                  [])
   :effects                     (if (seq (:effects minion))
                                  (vec (remove nil? (:effects minion)))
                                  [])
   :can-attack                  (or (:can-attack minion) false)
   :health                      (- (:health minion) (:damage-taken minion))
   :attack                      (get-attack game (:id minion))
   :valid-attack-ids            (construct/get-attackable-ids game (:owner-id minion))
   :max-health                  (:original-health minion)
   :race                        (:race minion)
   :set                         (:set minion)
   })


(defn hand->client-hand
  [game card]
  {:name                (:name card)
   :entity-type         :card
   :mana-cost           (:mana-cost card)
   :original-mana-cost  (:original-mana-cost card)
   :id                  (:id card)
   :owner-id            (:owner-id card)
   :attack              (:attack card)
   :original-attack     (:original-attack card)
   :health              (:health card)
   :original-health     (:original-health card)
   :playable            (construct/is-card-playable? game (:owner-id card) card)
   :valid-target-ids    (construct/get-valid-target-ids game (:owner-id card) card)
   :durability          (:durability card)
   :original-durability (:original-durability card)
   :description         (or (:description card) "")
   :race                (:race card)
   :type                (:type card)})


(defn secrets->client-secrets
  [game secret]
  (let [definition (def/get-definition secret)]
    {:name               (:name secret)
     :owner-id           (:owner-id secret)
     :class              (:class definition)
     :id                 (:id secret)
     :entity-type        :secret
     :rarity             (:rarity definition)
     :original-mana-cost (:original-mana-cost secret)
     :description        (or (:description secret) "")}))

(defn player->client-player
  {:test (fn []
           (let [game (construct/create-game)]
             (is (check-spec :firestone.client.kth.spec/player
                             (player->client-player game
                                                    (construct/get-player game "p1"))))))}
  [game player]
  (let [player-id (:id player)]
    {:board-entities (map #(minion->client-minion game %) (construct/get-minions game player-id))
     :active-secrets (map #(secrets->client-secrets game %) (construct/get-secrets game player-id))
     :deck-size      (count (construct/get-deck game player-id))
     :hand           (map #(hand->client-hand game %) (construct/get-hand game player-id))
     :hero           (hero->client-hero game
                                        (construct/get-hero game player-id)
                                        player-id)
     :id             player-id}))

(defn game->client-game
  "Converts a game state to a client-friendly format
   Accepts an optional action-index parameter that defaults to 0"
  ([game]
   (game->client-game game 0))
  ([game action-index]
   {:action-index   action-index
    :id             "the-game-id"
    :player-in-turn (construct/get-player-id-in-turn game)
    :players        (->> ["p1" "p2"]
                         (map (fn [player-id]
                                (player->client-player game
                                                       (construct/get-player game player-id)))))}))