(ns firestone.client.kth.mapper
  (:require [clojure.spec.alpha :as s]
            [firestone.construct :as construct]
            [ysera.test :refer [is]]
            [firestone.client.kth.spec]
            [firestone.definitions :as def]
            [firestone.core :refer [get-attack
                                    sleepy?]]))


(defn check-spec
  [spec value]
  (or (s/valid? spec value)
      (s/explain spec value)))

(defn filter-serializable-effects
  "Removes effects that contain functions (not JSON serializable)"
  [effects]
  (when effects
    (->> effects
         (filter (fn [effect]
                   (cond
                     ;; Keep string effects (they're serializable)
                     (string? effect) true

                     ;; Filter out map effects that contain functions
                     (map? effect)
                     (not (some fn? (vals effect)))

                     ;; Keep other types
                     :else true)))
         (vec))))

(defn hero->client-hero
  {:test (fn []
           (let [game (construct/create-game)]
             (is (check-spec :firestone.client.kth.spec/hero
                             (hero->client-hero game
                                                (construct/get-hero game "p1")
                                                "p1")))))}
  [game hero player-id]
  (let [hero-def (def/get-definition (:name hero))
        hero-power-name (:hero-power hero-def)
        hero-power-def (when hero-power-name (def/get-definition hero-power-name))
        hero-power-cost (when hero-power-def (:mana-cost hero-power-def))

        ;; FIXED: Calculate valid targets for hero powers that need them
        hero-power-targets (if (and hero-power-name
                                    (construct/hero-can-use-power? game player-id hero))
                             (cond
                               ;; Fireblast can target any character (like attacks)
                               (= hero-power-name "Fireblast")
                               (construct/get-attackable-ids game player-id)

                               ;; Life Tap doesn't need targets
                               (= hero-power-name "Life Tap")
                               []

                               ;; Default: no targets
                               :else [])
                             [])]

    {:armor            0
     :owner-id         player-id
     :entity-type      :hero
     :attack           0
     :can-attack       false
     :health           (- (or (:health hero) 30) (:damage-taken hero))
     :id               (:id hero)
     :mana             (construct/get-mana game player-id)
     :max-health       30
     :max-mana         (construct/get-max-mana game player-id)
     :name             (:name hero)
     :states           []
     :valid-attack-ids []
     :effects          (filter-serializable-effects (:effects hero))
     :hero-power       {:name               (or hero-power-name "Unknown")
                        :description        (or (:description hero-power-def) "")
                        :entity-type        :hero-power
                        :mana-cost          (or hero-power-cost 2)
                        :original-mana-cost (if hero-power-name
                                              (construct/get-mana-cost hero-power-name)
                                              2)
                        :can-use            (if hero-power-name
                                              (construct/hero-can-use-power? game player-id hero)
                                              false)
                        :owner-id           (:id hero)
                        :has-used-your-turn (or (:has-used-your-turn hero) false)
                        ;; FIXED: This should contain valid targets for targeting hero powers
                        :valid-target-ids   hero-power-targets}}))

(defn compute-can-attack
  "Computes whether a minion can attack based on current game state"
  [game minion]
  (let [minion-id (:id minion)
        owner-id (:owner-id minion)
        current-attack (get-attack game minion-id)
        valid-targets (construct/get-attackable-ids game owner-id)
        is-owner-turn (= (construct/get-player-id-in-turn game) owner-id)]
    (and
      ;; Basic attack requirements
      (> current-attack 0)                           ; Has attack power
      (not (sleepy? game minion-id))                 ; Not sleepy
      (seq valid-targets)                            ; Has valid targets
      is-owner-turn                                  ; Owner's turn
      ;; Haven't attacked too much this turn (assuming 1 attack per turn for most minions)
      (< (:attacks-performed-this-turn minion) 1))))

(defn minion->client-minion
  {:test (fn []
           (let [game (construct/create-game)
                 minion (construct/create-minion "Sheep" :id "p")
                 new-game (construct/add-minion-to-board game "p1" minion 0)]

             (is (check-spec :firestone.client.kth.spec/minion
                             (minion->client-minion new-game
                                                    (construct/get-minion new-game "p"))))))}
  [game minion]
  (let [minion-def (def/get-definition (:name minion))]
    {;; Static properties - read from stored values
     :damage-taken                (:damage-taken minion)
     :entity-type                 :minion
     :name                        (:name minion)
     :added-to-board-time-id      (:added-to-board-time-id minion)
     :attacks-performed-this-turn (:attacks-performed-this-turn minion)
     :id                          (:id minion)
     :owner-id                    (:owner-id minion)
     :position                    (:position minion)
     :overrides                   (:overrides minion)

     ;; Semi-static properties - read from stored values (set occasionally)
     :states                      (if (seq (:states minion))
                                    (vec (remove nil? (:states minion)))
                                    [])

     ;; UPDATED: Filter out functions from effects before sending to client
     :effects                     (filter-serializable-effects (:effects minion))

     ;; Static properties from definition - never change
     :mana-cost                   (:mana-cost minion-def)
     :original-attack             (:attack minion-def)
     :original-health             (:health minion-def)
     :description                 (:description minion-def)
     :max-health                  (:health minion-def)
     :race                        (:race minion-def)
     :set                         (:set minion-def)

     ;; Dynamic properties - computed from current game state
     :can-attack                  (compute-can-attack game minion)
     :health                      (- (:health minion-def) (:damage-taken minion))
     :attack                      (get-attack game (:id minion))
     :valid-attack-ids            (construct/get-attackable-ids game (:owner-id minion))
     :sleepy                      (sleepy? game (:id minion))}))

(defn hand->client-hand
  [game card]
  (let [card-def (def/get-definition (:name card))]
    {:name                (:name card)
     :entity-type         :card
     :mana-cost           (or (:mana-cost card) (:mana-cost card-def))
     :original-mana-cost  (or (:original-mana-cost card) (:mana-cost card-def))
     :id                  (:id card)
     :owner-id            (:owner-id card)
     :attack              (or (:attack card) (:attack card-def))
     :original-attack     (or (:original-attack card) (:attack card-def))
     :health              (or (:health card) (:health card-def))
     :original-health     (or (:original-health card) (:health card-def))
     :playable            (construct/is-card-playable? game (:owner-id card) card)
     :valid-target-ids    (construct/get-valid-target-ids game (:owner-id card) card)
     :durability          (or (:durability card) (:durability card-def))
     :original-durability (or (:original-durability card) (:durability card-def))
     :description         (or (:description card) (:description card-def) "")
     :race                (or (:race card) (:race card-def))
     :type                (or (:type card) (:type card-def))}))


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

(defn populate-default-cards
  "Adds default cards to the game when no game data is provided at the API level.
   Only adds defaults when game-params is nil (no request data), not when it's an empty array."
  [state game-params]
  (if (nil? game-params)
    (let [all-definitions (def/get-definitions)
          card-names (->> all-definitions
                          (filter #(contains? #{:minion :spell} (:type %)))
                          (map :name)
                          (remove #{"Squire" "Boom Bot" "Steward" "Cat in a Hat"})
                          (vec))]
      (-> state
          (construct/add-cards-to-deck "p1" card-names)
          (construct/add-cards-to-deck "p2" card-names)
          (construct/add-cards-to-hand "p1" (take 4 card-names))
          (construct/add-cards-to-hand "p2" (take 4 card-names))))
    ;; If game-params is provided (even if empty []), respect that and don't add defaults
    state))