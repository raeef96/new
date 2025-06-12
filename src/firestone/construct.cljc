(ns firestone.construct
  "A namespace for constructions and basic manipulations of the entities in the state"
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]))


(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Jaina Proudmoore")
                {:name         "Jaina Proudmoore"
                 :entity-type  :hero
                 :damage-taken 0
                 :mana         10
                 :max-mana     10
                 :fatigue      1})
           (is= (create-hero "Jaina Proudmoore" :damage-taken 10)
                {:name         "Jaina Proudmoore"
                 :entity-type  :hero
                 :damage-taken 10
                 :mana         10
                 :max-mana     10
                 :fatigue      1}))}
  ; Variadic functions [https://clojure.org/guides/learn/functions#_variadic_functions]
  [name & kvs]
  (let [hero {:name         name
              :entity-type  :hero
              :damage-taken 0
              :mana         10
              :max-mana     10
              :fatigue      1}]
    (cond
      (empty? kvs) hero
      :else (apply assoc hero kvs))))


(defn create-card
  "Creates a card from its definition by the given card name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-card "Boulderfist Ogre" :id "bo")
                {:id          "bo"
                 :entity-type :card
                 :name        "Boulderfist Ogre"}))}
  [name & kvs]
  (let [card {:name        name
              :entity-type :card}]
    (if (empty? kvs)
      card
      (apply assoc card kvs))))

(defn get-other-player-id
  "Returns the player ID of the player that is not associated with the given player-id.
  If player-id is 'p1', it returns 'p2', and vice versa."
  {:test (fn []
           (is= (get-other-player-id "p1") "p2")
           (is= (get-other-player-id "p2") "p1"))}
  [player-id]
  (if (= player-id "p1") "p2" "p1"))

(defn effects-parser
  "The central function for processing all effects in the game."
  [state characters player-id event & [args]]
  (let [enemy-id (get-other-player-id player-id)
        args (or args {})]
    (reduce
      (fn [state character]
        (let [context {:state state :id (:id character) :player-id player-id
                       :enemy-id enemy-id :target-id (:target-id args) :args args}
              card-definition (get-definition (:name character))
              direct-effect-fn (get card-definition event)]
          (let [state (if (and (contains? card-definition event) (fn? direct-effect-fn))
                        (try (or (direct-effect-fn context) state) (catch Exception e state))
                        state)]
            (reduce
              (fn [state effect]
                (let [context (assoc context :state state)]
                  (if (and (map? effect)
                           (contains? effect event)
                           (fn? (get effect event)))
                    (try
                      (or ((get effect event) context) state)
                      (catch Exception e state))
                    state)))
              state
              (:effects character)))))
      state
      characters)))

(defn get-mana-cost
  "Returns the mana cost of the minion (or card) with the given name."
  {:test (fn []
           (is= (-> (get-mana-cost "Loot Hoarder"))
                2)
           (is= (-> (get-mana-cost "Boulderfist Ogre"))
                6))}
  [name]
  (let [definition (get-definition name)]
    (:mana-cost definition)))

(defn create-minion
  "Creates a minion from its definition by the given minion name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-minion "Sheep"
                               :id "m"
                               :attacks-performed-this-turn 1)
                {:attacks-performed-this-turn 1
                 :damage-taken                0
                 :entity-type                 :minion
                 :name                        "Sheep"
                 :id                          "m"
                 :overrides                   {}}))}
  [name & kvs]
  (let [definition (get-definition name)
        minion {:damage-taken                0
                :entity-type                 :minion
                :name                        name
                :attacks-performed-this-turn 0
                :overrides                   {}}]
    (if (empty? kvs)
      minion
      (apply assoc minion kvs))))


(defn create-empty-state
  "Creates an empty state with the given heroes."
  {:test (fn []

           (is= (create-empty-state [(create-hero "Jaina Proudmoore" :id "r")
                                     (create-hero "Gul'dan")])
                {:player-id-in-turn             "p1"
                 :players                       {"p1" {:id      "p1"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :secrets []
                                                       :hero    {:name         "Jaina Proudmoore"
                                                                 :id           "r"
                                                                 :damage-taken 0
                                                                 :entity-type  :hero
                                                                 :mana         10
                                                                 :max-mana     10
                                                                 :fatigue      1}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :secrets []
                                                       :hero    {:name         "Gul'dan"
                                                                 :id           "h2"
                                                                 :damage-taken 0
                                                                 :entity-type  :hero
                                                                 :mana         10
                                                                 :max-mana     10
                                                                 :fatigue      1}}}
                 :counter                       1
                 :seed                          42
                 :minion-ids-summoned-this-turn []
                 :hero-power-used-this-turn #{} }))}
  ([]
   (create-empty-state []))
  ([heroes]

   (let [heroes (->> (concat heroes [(create-hero "Jaina Proudmoore")
                                     (create-hero "Gul'dan")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          (map-indexed (fn [index hero]
                                                         {:id      (str "p" (inc index))
                                                          :deck    []
                                                          :hand    []
                                                          :minions []
                                                          :secrets []
                                                          :hero    (if (contains? hero :id)
                                                                     hero
                                                                     (assoc hero :id (str "h" (inc index))))}))
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :seed                          42
      :minion-ids-summoned-this-turn []
      :hero-power-used-this-turn #{} })))


(defn get-player
  "Returns the player with the given id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player "p1")
                    (:id))
                "p1"))}
  [state player-id]
  (get-in state [:players player-id]))


(defn get-player-id-in-turn
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player-id-in-turn))
                "p1"))}
  [state]
  (:player-id-in-turn state))


(defn get-minions
  "Returns the minions on the board for the given player-id or for both players."
  {:test (fn []
           ; Getting minions is also tested in add-minion-to-board.
           (is= (-> (create-empty-state)
                    (get-minions "p1"))
                [])
           (is= (-> (create-empty-state)
                    (get-minions))
                [])
           (is= (as-> (create-empty-state) $
                      (assoc-in $ [:players "p1" :minions] [(create-minion "Sheep")])
                      (get-minions $ "p1")
                      (map :name $))
                ["Sheep"]))}
  ([state player-id]
   (:minions (get-player state player-id)))
  ([state]
   (->> (:players state)
        (vals)
        (map :minions)
        (apply concat))))


(defn get-deck
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-deck "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :deck]))


(defn get-hand
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-hand "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :hand]))

(defn get-secrets
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-secrets "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :secrets]))


(defn- generate-id
  "Generates an id and returns a tuple with the new state and the generated id."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])


(defn- generate-time-id
  "Generates a number and returns a tuple with the new state and the generated number."
  {:test (fn []
           (is= (generate-time-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(declare get-all-characters)

(defn add-minion-to-board
  "Adds a minion with a given position to a player's minions and updates the other minions' positions."
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-minion-to-board $ "p1" (create-minion "Sheep" :id "m") 0)
                      (get-minions $ "p1")
                      (map (fn [m] {:id (:id m) :name (:name m)}) $))
                [{:id "m" :name "Sheep"}])
           ; Adding a minion and update positions
           (let [minions (-> (create-empty-state)
                             (add-minion-to-board "p1" (create-minion "Sheep" :id "m1") 0)
                             (add-minion-to-board "p1" (create-minion "Sheep" :id "m2") 0)
                             (add-minion-to-board "p1" (create-minion "Sheep" :id "m3") 1)
                             (get-minions "p1"))]
             (is= (map :id minions) ["m1" "m2" "m3"])
             (is= (map :position minions) [2 0 1]))
           (let [state (-> (create-empty-state)
                           (add-minion-to-board "p1" (create-minion "Sheep") 0))]
             (is= (-> (get-minions state "p1")
                      (first)
                      (:name))
                  "Sheep")
             (is= (:counter state) 3)))}
  [state player-id minion position]
  {:pre [(map? state) (string? player-id) (map? minion) (number? position)]}
  (let [minions (get-in state [:players player-id :minions])
        current-count (count minions)]
    (if-not (< current-count 7)
      state

      (let [[state id] (if (contains? minion :id)
                         [state (:id minion)]
                         (let [[state value] (generate-id state)]
                           [state (str "m" value)]))
            [state time-id] (generate-time-id state)
            ready-minion (assoc minion :position position
                                       :effects (if (contains? minion :effects)
                                                  (:effects minion)
                                                  (let [effect (get (get-definition (:name minion)) :effect)]
                                                    (if effect [effect] [])))
                                       :owner-id player-id
                                       :id id
                                       :added-to-board-time-id time-id)]
        (let [state (-> state (update-in
                                [:players player-id :minions]
                                (fn [minions]
                                  (conj (->> minions
                                             (mapv (fn [m]
                                                     (if (< (:position m) position)
                                                       m
                                                       (update m :position inc)))))
                                        ready-minion))))]
          (let [state (effects-parser state
                                      (get-all-characters state)
                                      player-id
                                      :summon-minion)]
            (effects-parser state
                            (get-minions state player-id)
                            player-id
                            :process-auras)))))))

(defn add-minions-to-board
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-minions-to-board $ "p1" [(create-minion "Boulderfist Ogre")
                                                    "Sheep"
                                                    (create-minion "Leper Gnome")])
                      (get-minions $ "p1")
                      (map :name $))
                ["Boulderfist Ogre" "Sheep" "Leper Gnome"]))}
  [state player-id minions]
  (->> minions
       (reduce-kv (fn [state index minion]
                    (add-minion-to-board state
                                         player-id
                                         (if (string? minion)
                                           (create-minion minion)
                                           minion)
                                         index))
                  state)))


(defn- add-card-to
  "Adds a card to either the hand or the deck."
  {:test (fn []
           ; Adding cards to deck
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Leper Gnome" :deck)
                      (add-card-to $ "p1" "Sheep" :deck)
                      (get-deck $ "p1")
                      (map :name $))
                ["Leper Gnome" "Sheep"])
           ; Adding cards to hand
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Leper Gnome" :hand)
                      (add-card-to $ "p1" "Sheep" :hand)
                      (get-hand $ "p1")
                      (map :name $))
                ["Leper Gnome" "Sheep"]))}
  [state player-id card-or-name place]
  (let [card (if (string? card-or-name)
               (create-card card-or-name)
               card-or-name)
        [state id] (if (contains? card :id)
                     [state (:id card)]
                     (let [[state value] (generate-id state)]
                       [state (str "c" value)]))
        card-to-be-added (assoc card :owner-id player-id
                                     :id id)]
    (condp = place
      :hand
      (let [hand-count (count (get-hand state player-id))]
        (if-not (< hand-count 10)
          state
          (update-in state [:players player-id place] conj card-to-be-added)))

      :secrets
      (let [current-secrets (get-secrets state player-id)
            card-name (if (string? card-or-name) card-or-name (:name card-or-name))
            secret-already-exists? (some #(= (:name %) card-name) current-secrets)]
        (if secret-already-exists?
          state
          (update-in state [:players player-id place] conj card-to-be-added)))

      (update-in state [:players player-id place] conj card-to-be-added))))


(defn add-card-to-deck
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-card-to-deck $ "p1" "Leper Gnome")
                      (get-deck $ "p1")
                      (map :name $))
                ["Leper Gnome"]))}
  [state player-id card]
  (add-card-to state player-id card :deck))


(defn add-card-to-hand
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-card-to-hand $ "p1" "Leper Gnome")
                      (get-hand $ "p1")
                      (map :name $))
                ["Leper Gnome"]))}
  [state player-id card]
  (add-card-to state player-id card :hand))


(defn add-cards-to-deck
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-cards-to-deck $ "p1" ["Leper Gnome" "Loot Hoarder"])
                      (get-deck $ "p1")
                      (map :name $))
                ["Leper Gnome" "Loot Hoarder"]))}
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-deck state player-id card))
          state
          cards))

(defn add-card-to-secrets
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-card-to-secrets $ "p1" (create-card "Cat Trick"))
                      (get-secrets $ "p1")
                      (map :name $))
                ["Cat Trick"]))}
  [state player-id card]
  (add-card-to state player-id card :secrets))

(defn add-cards-to-secrets
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-secrets state player-id card))
          state
          cards))

(defn add-cards-to-hand
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-cards-to-hand $ "p1" ["Leper Gnome" "Loot Hoarder"])
                      (get-hand $ "p1")
                      (map :name $))
                ["Leper Gnome" "Loot Hoarder"]))}
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-hand state player-id card))
          state
          cards))

(defn set-max-mana
  [state player-id value]
  (-> state
      (assoc-in [:players player-id :hero :max-mana] (min 10 value))))

(defn set-mana
  [state player-id value]
  (assoc-in state [:players player-id :hero :mana] value))


(defn create-game
  "Creates a game with the given deck, hand, minions (placed on the board), and heroes."
  {:test (fn []
           (is= (create-game) (create-empty-state))

           (is= (create-game [{:hero (create-hero "Gul'dan")}])
                (create-game [{:hero "Gul'dan"}]))

           (is= (create-game [{:minions [(create-minion "Leper Gnome")]}])
                (create-game [{:minions ["Leper Gnome"]}]))

           ; This test is showing the state structure - otherwise avoid large assertions
           (is= (create-game [{:minions ["Leper Gnome"]
                               :deck    ["Loot Hoarder"]
                               :hand    ["Sheep"]}
                              {:hero "Gul'dan"}]
                             :player-id-in-turn "p2")
                {:player-id-in-turn             "p2"
                 :players                       {"p1" {:id      "p1"
                                                       :deck    [{:attack             2
                                                                  :description        "Deathrattle: Draw a card."
                                                                  :entity-type        :card
                                                                  :health             1
                                                                  :id                 "c3"
                                                                  :mana-cost          2
                                                                  :name               "Loot Hoarder"
                                                                  :original-attack    2
                                                                  :original-health    1
                                                                  :original-mana-cost 2
                                                                  :owner-id           "p1"
                                                                  :playable           false
                                                                  :type               :minion
                                                                  :valid-target-ids   []}]
                                                       :hand    [{:attack             1
                                                                  :description        ""
                                                                  :entity-type        :card
                                                                  :health             1
                                                                  :id                 "c4"
                                                                  :mana-cost          1
                                                                  :name               "Sheep"
                                                                  :original-attack    1
                                                                  :original-health    1
                                                                  :original-mana-cost 1
                                                                  :owner-id           "p1"
                                                                  :playable           false
                                                                  :type               :minion
                                                                  :valid-target-ids   []}]
                                                       :minions [{:attack                      1
                                                                  :attacks-performed-this-turn 0
                                                                  :added-to-board-time-id      2
                                                                  :can-attack                  nil
                                                                  :damage-taken                0
                                                                  :description                 "Deathrattle: Deal 2 damage to the enemy hero."
                                                                  :effects                     ["Leper Gnome effect"]
                                                                  :entity-type                 :minion
                                                                  :health                      1
                                                                  :id                          "m1"
                                                                  :mana-cost                   1
                                                                  :max-health                  1
                                                                  :name                        "Leper Gnome"
                                                                  :original-attack             1
                                                                  :original-health             1
                                                                  :owner-id                    "p1"
                                                                  :position                    0
                                                                  :race                        nil
                                                                  :set                         :classic}]
                                                       :secrets []
                                                       :hero    {:class              :mage
                                                                 :damage-taken       0
                                                                 :effects            []
                                                                 :entity-type        :hero
                                                                 :fatigue-counter    0
                                                                 :has-used-your-turn false
                                                                 :health             30
                                                                 :hero-power         {:class       :mage
                                                                                      :description "Deal 1 damage."
                                                                                      :mana-cost   2
                                                                                      :name        "Fireblast"
                                                                                      :type        :hero-power}
                                                                 :id                 "h1"
                                                                 :mana               10
                                                                 :max-mana           10
                                                                 :name               "Jaina Proudmoore"
                                                                 :stealth            nil
                                                                 :type               :hero}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :secrets []
                                                       :hero    {:class              :warlock
                                                                 :damage-taken       0
                                                                 :effects            []
                                                                 :entity-type        :hero
                                                                 :fatigue-counter    0
                                                                 :has-used-your-turn false
                                                                 :health             30
                                                                 :hero-power         {:class       :warlock
                                                                                      :description "Draw a card and take 2 damage."
                                                                                      :mana-cost   2
                                                                                      :name        "Life Tap"
                                                                                      :type        :hero-power}
                                                                 :id                 "h2"
                                                                 :mana               10
                                                                 :max-mana           10
                                                                 :name               "Gul'dan"
                                                                 :stealth            nil
                                                                 :type               :hero}}}
                 :counter                       5
                 :effects                       []
                 :minion-ids-summoned-this-turn []}))}
  ([data & kvs]
   (let [players-data (map-indexed (fn [index player-data]
                                     (assoc player-data :player-id (str "p" (inc index))))
                                   data)
         state (as-> (create-empty-state (map (fn [player-data]
                                                (let [overrides (select-keys player-data [:mana :max-mana])]
                                                  (cond
                                                    (nil? (:hero player-data))
                                                    (create-hero "Jaina Proudmoore"
                                                                 :mana (or (:mana player-data) 10)
                                                                 :max-mana (or (:max-mana player-data) 10))

                                                    (string? (:hero player-data))
                                                    (create-hero (:hero player-data)
                                                                 :mana (or (:mana player-data) 10)
                                                                 :max-mana (or (:max-mana player-data) 10))

                                                    :else
                                                    (merge (:hero player-data) overrides))))
                                              data)) $
                     (reduce (fn [state {player-id :player-id
                                         minions   :minions
                                         deck      :deck
                                         board     :board
                                         hand      :hand
                                         secrets   :secrets}]
                               (-> state
                                   (add-minions-to-board player-id minions)
                                   (add-cards-to-deck player-id deck)
                                   (add-cards-to-hand player-id hand)
                                   (add-cards-to-secrets player-id secrets)))
                             $
                             players-data))]
     (if (empty? kvs)
       state
       (apply assoc state kvs))))
  ([]
   (create-game [])))

(defn get-max-mana
  [state player-id]
  (get-in state [:players player-id :hero :max-mana]))

(defn get-mana
  [state player-id]
  (get-in state [:players player-id :hero :mana]))

(defn reset-player-mana
  "Resets a player's mana at the end of their turn. Increases max mana by 1 if it's less than 10, and sets current mana to that new max."
  [state player-id]
  (let [old-max-mana (get-max-mana state player-id)
        new-max-mana (min 10 (inc old-max-mana))]
    (-> state
        (assoc-in [:players player-id :hero :max-mana] new-max-mana)
        (assoc-in [:players player-id :hero :mana] new-max-mana))))


(defn get-minion
  "Returns the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Leper Gnome" :id "m")]}])
                    (get-minion "m")
                    (:name))
                "Leper Gnome"))}
  [state id]
  (->> (get-minions state)
       (filter (fn [m] (= (:id m) id)))
       (first)))


(defn get-players
  {:test (fn []
           (is= (->> (create-game)
                     (get-players)
                     (map :id))
                ["p1" "p2"]))}
  [state]
  (->> (:players state)
       (vals)))

(defn get-hero
  "Returns the hero of the player with the given player-id from the game state."
  {:test (fn []
           (let [game-state {:player-id-in-turn "p1"
                             :players {"p1" {:id "p1"
                                             :hero {:name "Jaina Proudmoore"
                                                    :id "h1"
                                                    :entity-type :hero
                                                    :damage-taken 0
                                                    :fatigue 0}}
                                       "p2" {:id "p2"
                                             :hero {:name "Gul'dan"
                                                    :id "h2"
                                                    :entity-type :hero
                                                    :damage-taken 0
                                                    :fatigue 0}}}}]
             (is (= (get-hero game-state "p1")
                    {:name "Jaina Proudmoore"
                     :id "h1"
                     :entity-type :hero
                     :damage-taken 0
                     :fatigue 0}))
             (is (= (get-hero game-state "p2")
                    {:name "Gul'dan"
                     :id "h2"
                     :entity-type :hero
                     :damage-taken 0
                     :fatigue 0}))
             (is (nil? (get-hero game-state "p3")))))}
  [state player-id]
  (get-in state [:players player-id :hero]))


(defn get-heroes
  {:test (fn []
           (is= (->> (create-game [{:hero "Gul'dan"}])
                     (get-heroes)
                     (map :name))
                ["Gul'dan" "Jaina Proudmoore"]))}
  [state]
  (->> (get-players state)
       (map :hero)))

(defn replace-minion
  "Replaces a minion with the same id by the given new-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Leper Gnome" :id "m")]}])
                    (replace-minion (create-minion "Sheep" :id "m"))
                    (get-minion "m")
                    (:name))
                "Sheep"))}
  [state new-minion]
  (let [owner-id (or (:owner-id new-minion)
                     (:owner-id (get-minion state (:id new-minion))))]
    (update-in state
               [:players owner-id :minions]
               (fn [minions]
                 (map (fn [m]
                        (if (= (:id m) (:id new-minion))
                          new-minion
                          m))
                      minions)))))

(defn update-minion
   "Updates the value of the given key for the minion with the given id."
   [state id key function-or-value]
   (let [minion (get-minion state id)
         new-value (if (fn? function-or-value)
                     (function-or-value (get minion key))
                     function-or-value)]
     (replace-minion state (assoc minion key new-value))))

(defn should-take-fatigue?
  "Returns truthy if the player should take fatigue damage (i.e., their deck is empty), falsey otherwise."
  {:test (fn []
           ;; Test when the deck is empty
           (let [state (create-game)]
             (is (should-take-fatigue? state "p1")))
           ;; Test when the deck has one card
           (let [state (-> (create-game)
                           (add-card-to-deck "p1" "Leper Gnome"))]
             (is-not (should-take-fatigue? state "p1")))
           ;; Test when the deck has multiple cards
           (let [state (-> (create-game)
                           (add-card-to-deck "p1" "Leper Gnome")
                           (add-card-to-deck "p1" "Boulderfist Ogre"))]
             (is-not (should-take-fatigue? state "p1"))))}
  [state player-id]
  (empty? (get-in state [:players player-id :deck])))

(defn handle-fatigue
  "Handles fatigue when a player's deck is empty, and they need to draw a card.
   Increases fatigue damage by 1 each time it occurs."
  [state player-id]
  (let [fatigue-damage (+ (get-in state [:players player-id :hero :fatigue-counter] 0) 1)
        current-damage-taken (get-in state [:players player-id :hero :damage-taken] 0)]
    (-> state
        (assoc-in [:players player-id :hero :fatigue-counter] fatigue-damage)
        (assoc-in [:players player-id :hero :damage-taken] (+ current-damage-taken fatigue-damage)))))

(defn remove-minion
  "Removes a minion with the given id from the state, triggering its Deathrattle if applicable."
  {:test (fn []
           (let [initial-state (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :damage-taken 0)}
                                             {:hero (create-hero "Gul'dan" :id "h2" :damage-taken 0)}])
                 leper-gnome (create-minion "Leper Gnome" :id "lg" :owner-id "p1")
                 loot-hoarder (create-minion "Loot Hoarder" :id "lh" :owner-id "p1")
                 boulderfist-ogre (create-minion "Boulderfist Ogre" :id "bo" :owner-id "p1")
                 state-with-minions (-> initial-state
                                        (add-minion-to-board "p1" leper-gnome 0)
                                        (add-minion-to-board "p1" loot-hoarder 1)
                                        (add-minion-to-board "p1" boulderfist-ogre 2)
                                        (add-card-to-deck "p1" "Sheep")) ; Changed from "Test Card" to "Sheep"
                 state-after-leper-gnome (remove-minion state-with-minions "lg")
                 state-after-loot-hoarder (remove-minion state-after-leper-gnome "lh")
                 state-after-boulderfist (remove-minion state-after-loot-hoarder "bo")]
             ; Test Leper Gnome removal and Deathrattle
             (is= (count (get-minions state-after-leper-gnome "p1")) 2)
             (is-not (get-minion state-after-leper-gnome "lg"))
             (is= (get-in state-after-leper-gnome [:players "p2" :hero :damage-taken]) 2)
             ; Test Loot Hoarder removal and Deathrattle
             (is= (count (get-minions state-after-loot-hoarder "p1")) 1)
             (is-not (get-minion state-after-loot-hoarder "lh"))
             (is= (count (get-hand state-after-loot-hoarder "p1")) 1)
             (is= (count (get-deck state-after-loot-hoarder "p1")) 0)
             ; Test Boulderfist Ogre removal (no Deathrattle)
             (is= (count (get-minions state-after-boulderfist "p1")) 0)
             (is-not (get-minion state-after-boulderfist "bo"))
             ; Test removing non-existent minion
             (is= state-after-boulderfist (remove-minion state-after-boulderfist "non-existent"))))}
  [state id]
  (let [minion (get-minion state id)]
    (if-not minion
      state

      (let [owner-id (:owner-id minion)
            state (effects-parser state [minion] owner-id :deathrattle)
            state (update-in state
                             [:players owner-id :minions]
                             (fn [minions]
                               (let [position (:position minion)]
                                 (->> minions
                                      (remove (fn [m] (= (:id m) id)))
                                      (mapv (fn [m]
                                              (if (> (:position m) position)
                                                (update m :position dec)
                                                m)))))))]
        (effects-parser state
                        (get-minions state owner-id)
                        owner-id
                        :process-auras)))))

(defn remove-minions
  "Removes the minions with the given ids from the state."
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Leper Gnome" :id "n1")
                                               (create-minion "Leper Gnome" :id "n2")]}
                                    {:minions [(create-minion "Leper Gnome" :id "n3")
                                               (create-minion "Leper Gnome" :id "n4")]}]) $
                      (remove-minions $ "n1" "n4")
                      (get-minions $)
                      (map :id $))
                ["n2" "n3"]))}
  [state & ids]
  (reduce remove-minion state ids))

(defn remove-card-from-hand
  "Removes a specified card from the player's hand."
  {:test (fn []
           (let [state (-> (create-game [{:hand [(create-card "Leper Gnome" :id "c1")
                                                 (create-card "Boulderfist Ogre" :id "c2")
                                                 (create-card "Leper Gnome" :id "c3")]}]))
                 card (->> (get-hand state "p1")
                           (second))]
             (is= (as-> state $
                        (remove-card-from-hand $ "p1" card)
                        (get-hand $ "p1")
                        (map :id $))
                  ["c1" "c3"])))}
  [state player-id card]
  (update-in state [:players player-id :hand]
             (fn [hand]
               (->> hand
                    (remove (fn [c] (= (:id card) (:id c))))))))

(defn can-play-minion?
  "Determines if a player can play a specified minion card"
  {:test (fn []
           (is= (-> (create-game [{:mana 5}])
                    (can-play-minion? "p1" (create-card "Boulderfist Ogre")))
                false)
           (is= (-> (create-game [{:mana 6}])
                    (can-play-minion? "p1" (create-card "Boulderfist Ogre")))
                true)
           (is= (-> (create-game [{:mana 7}])
                    (can-play-minion? "p1" (create-card "Boulderfist Ogre")))
                true))}
  [state player-id card]
  {:pre [(map? state) (string? player-id) (map? card)]}
  (and
    ;; Check if it's the player's turn
    (= (:player-id-in-turn state) player-id)

    ;; Check if the player has fewer than 7 minions on the board
    (< (count (get-in state [:players player-id :minions])) 7)

    ;; Check if the player has enough mana to play the card
    (<= (-> card
            (get-definition)
            (:mana-cost))  ;; Retrieve the card's mana cost
        (get-in state [:players player-id :hero :mana]))))  ;; Retrieve the player's current mana

(defn convert-card-to-minion
  "Creates a minion out of a card using the create-minion function."
  {:test (fn []
           (is= (-> (convert-card-to-minion (create-card "Boulderfist Ogre"))
                    (:damage-taken))
                0)
           (is= (-> (convert-card-to-minion (create-card "Boulderfist Ogre"))
                    (:entity-type))
                :minion))}
  [card]
  {:pre [(map? card)]}
  (let [definition (get-definition (:name card))
        base-states (filter identity [(when (:stealth definition) :stealth)
                                      (when (:deathrattle definition) :deathrattle)])]
    (create-minion (:name card)
                   :states base-states
                   :damage-taken 0
                   :attacks-performed-this-turn 0
                   :sleepy true
                   :valid-attack-ids {})))

(defn deduct-player-mana
  "Reduces a player's mana by the specified cost and returns the updated game state."
  {:test (fn []
           ; Test that mana is properly deducted
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :mana 7)}])
                    (deduct-player-mana "p1" 6)
                    (get-in [:players "p1" :hero :mana]))
                1))}
  [state player-id mana-cost]
  {:pre [(map? state) (string? player-id) (int? mana-cost)]}
  (let [player-mana (get-in state [:players player-id :hero :mana])
        updated-mana (- player-mana mana-cost)]
    (update-in state [:players player-id :hero :mana] (constantly updated-mana))))

(defn put-card-on-board
  "Transforms a card into a minion using convert-card-to-minion and places it on the board for the specified player at the given position.
   Handles special effects like reaction and end-of-turn effects."
  {:test (fn []
           (is= (let [card (create-card "Boulderfist Ogre" :id "bo")]
                  (-> (create-game [{:hand [card]}])
                      (put-card-on-board "p1" card 1)
                      (get-in [:players "p1" :minions])
                      (first)
                      (:name)))
                "Boulderfist Ogre"))}
  [state player-id card position]
  {:pre [(map? state) (string? player-id) (map? card) (int? position)]}
  (let [minion (convert-card-to-minion card)]
    (-> state
        (add-minion-to-board player-id minion position))))

(defn update-hero
  [state player-id key function-or-value]
  (when-not (or (= player-id "p1") (= player-id "p2"))
    (throw (ex-info "Invalid player-id in update-hero" {:player-id player-id})))
  (update-in state [:players player-id :hero]
             (fn [hero]
               (if (fn? function-or-value)
                 (update hero key function-or-value)
                 (assoc hero key function-or-value)))))

(defn get-player-id-by-hero-id
  "Returns the player ID that owns the hero with the given hero ID."
  [state hero-id]
  (some (fn [[player-id player-data]]
          (when (= (:id (:hero player-data)) hero-id)
            player-id))
        (:players state)))

(defn get-character
  "Returns the character with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-character "h1")
                    (:name))
                "Jaina Proudmoore")
           (is= (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                    (get-character "m")
                    (:name))
                "Sheep"))}
  [state id]
  (or (some (fn [m] (when (= (:id m) id) m))
            (get-minions state))
      (some (fn [h] (when (= (:id h) id) h))
            (get-heroes state))))

(defn get-health
  "Returns the health of the character."
  ([character]
   {:pre [(map? character) (contains? character :damage-taken)]}
   (let [definition (get-definition (:name character))
         base-health (or (:health definition) 1)
         health-bonus (if (= (:entity-type character) :minion)
                        (get-in character [:overrides :health-bonus] 0)
                        0)
         max-health (+ base-health health-bonus)]
     (- max-health (:damage-taken character))))
  ([state id]
   (get-health (get-character state id))))

(defn get-all-characters
  "Returns all characters (hero + minions) in the game state.
   This includes all heroes and minions from both players."
  [state]
  (let [heroes (get-heroes state)
        minions (get-minions state)]
    (concat heroes minions)))

(defn handle-minion-attack-on-minion
  "Handles minion vs. minion combat"
  [{:keys [state attacker-id target-id]}]
  (let [attacker (get-minion state attacker-id)
        target (get-minion state target-id)
        attacker-player-id (:owner-id attacker)
        attacker-def (get-definition (:name attacker))
        target-def (get-definition (:name target))
        attacker-attack (or (:attack attacker-def) 0)
        target-attack (or (:attack target-def) 0)

        state (-> state
                  (update-minion attacker-id :damage-taken #(+ % target-attack))
                  (update-minion target-id :damage-taken #(+ % attacker-attack))
                  (update-minion attacker-id :attacks-performed-this-turn inc)
                  (update-minion attacker-id :can-attack nil))
        state (effects-parser state
                              [(get-minion state attacker-id)
                               (get-minion state target-id)]
                              attacker-player-id
                              :damage-minion)]
    (let [updated-attacker (get-minion state attacker-id)
          updated-target (get-minion state target-id)]
      (cond-> state
              (and updated-attacker (<= (get-health updated-attacker) 0))
              (remove-minion attacker-id)

              (and updated-target (<= (get-health updated-target) 0))
              (remove-minion target-id)))))

(defn handle-minion-attack-on-hero
  "Handles minion vs. hero combat"
  [{:keys [state attacker-id target-id]}]
  (let [attacker (get-minion state attacker-id)
        attacker-player-id (:owner-id attacker)
        target-player-id (get-player-id-by-hero-id state target-id)
        attacker-def (get-definition (:name attacker))
        attack-value (or (:attack attacker-def) 0)
        active-secrets (get-in state [:players target-player-id :secrets] [])
        state (effects-parser state
                              active-secrets
                              target-player-id
                              :hero-attacked
                              {:attacker-id attacker-id
                               :attacker-player-id attacker-player-id})
        state (-> state
                  (update-hero target-player-id :damage-taken #(+ % attack-value))
                  (update-minion attacker-id :states #(filter (fn [state] (not= state :stealth)) %))
                  (update-minion attacker-id :can-attack nil)
                  (update-minion attacker-id :attacks-performed-this-turn inc))]
    (effects-parser state
                    (cons attacker (get-all-characters state))
                    attacker-player-id
                    :attack
                    {:target-id target-id})))

(defn clear-sleepy-status-from-minions
  "Removes sleepy from minion-ids-summoned-this-turn list."
  [state]
  (let [minion-ids (:minion-ids-summoned-this-turn state)]
    (reduce
      (fn [state minion-id]
        (if (get-minion state minion-id)
          (update-minion state minion-id :sleepy false)
          state))
      (assoc state :minion-ids-summoned-this-turn [])
      minion-ids)))


(defn reset-turn-static-properties
  "Only resets truly static/semi-static properties at turn boundaries."
  [state player-id]
  (let [minions (get-minions state player-id)]
    (reduce
      (fn [state minion]
        (let [id (:id minion)]
          (-> state
              (update-minion id :attacks-performed-this-turn 0)
              (update-minion id :sleepy false))))
      state
      minions)))

(defn remove-can-attack
  "Sets can-attack for the minions of the given player-id to nil"
  [state player-id]
  (let [minions (get-minions state player-id)]
    (reduce
      (fn [state minion]
        (update-minion state (:id minion) :can-attack nil))
      state
      minions)))

(defn reset-attacks-performed-this-turn
  "Resets attacks-performed-this-turn for the minions of the given player-id"
  [state player-id]
  (let [minions (get-minions state player-id)]
    (reduce
      (fn [state minion]
        (update-minion state (:id minion) :attacks-performed-this-turn 0))
      state
      minions)))

(defn get-hero-mana
  "Returns the hero's mana"
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-hero-mana "p1"))
                10))}
  [state player-id]
  (get-in state [:players player-id :hero :mana]))

(defn update-hero-mana
  "Updates the mana of the hero for the given player to either a specified value or applies a function to the current mana."
  {:test (fn []
           ;; Test setting a fixed value
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (update-hero-mana "p1" 4)
                    (get-hero-mana "p1"))
                4)
           ;; Test using a function to modify the current mana
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :mana 10)}])
                    (update-hero-mana "p1" #(- % 2))
                    (get-hero-mana "p1"))
                8))}
  [state player-id function-or-value]
  (let [current-mana (get-in state [:players player-id :hero :mana])
        new-mana (if (fn? function-or-value) (function-or-value current-mana) function-or-value)]
    (-> state
        (assoc-in [:players player-id :hero :mana] new-mana))))

(defn hero-can-use-power?
  "Determines if a hero can use their power based on game state."
  [state player-id hero]
  (let [hero-mana (get-hero-mana state player-id)
        hero-def (get-definition (:name hero))
        hero-power-name (:hero-power hero-def)
        hero-power-def (when hero-power-name
                         (get-definition hero-power-name))
        mana-cost (:mana-cost hero-power-def 2)
        hero-power-used? (:has-used-your-turn hero false)]
    (and (some? hero-power-def)
         (<= mana-cost hero-mana)
         (= (:player-id-in-turn state) player-id)
         (not hero-power-used?))))

(defn get-valid-target-ids
  "Returns a collection of valid target IDs based on the card or hero's target definitions."
  [state player-id hero-or-card]
  (let [definition (get-definition (:name hero-or-card))
        hero-ids [(get-in (get-hero state "p1") [:id])
                  (get-in (get-hero state "p2") [:id])]
        minion-ids (map :id (get-minions state))
        enemy-player-id (if (= player-id "p1") "p2" "p1")
        enemy-minion-ids (map :id
                              (filter #(not (some #{:stealth} (:states %)))
                                      (get-minions state enemy-player-id)))
        friendly-minion-ids (map :id (get-minions state player-id))
        stealth-minion-ids (map :id
                                (filter #(some #{:stealth} (:states %))
                                        (get-minions state)))]
    (cond-> []
            (some #{:all-heroes} (:target definition)) (into hero-ids)
            (some #{:all-minions} (:target definition)) (into (concat friendly-minion-ids enemy-minion-ids))
            (some #{:friendly-minions} (:target definition)) (into friendly-minion-ids)
            (some #{:stealth-minions} (:target definition)) (into stealth-minion-ids))))


(defn get-attackable-ids
  "Returns the valid attackable ids for the given player-id, excluding minions with :stealth not nil. Test in cardtests.cljc"
  [state player-id]
  (let [minion-ids (reduce
                     (fn [acc minion]
                       (if (not (some #(= % :stealth) (:states minion)))
                         (conj acc (:id minion))
                         acc))
                     []
                     (get-minions state (if (= player-id "p1") "p2" "p1")))]
    (concat minion-ids
            [(:id (get-hero state (if (= player-id "p1") "p2" "p1")))])))

(defn is-card-playable?
  "Returns true if the given card is playable for the specified player, else false."
  {:test (fn []
           (let [state (-> (create-game [{:hand [(create-minion "Sheep" :id "m1")]}
                                         {:hand [(create-minion "Sheep" :id "m2")]}]))
                 hand (get-hand state "p1")
                 sheep (first hand)]
             (is= (is-card-playable? state "p2" sheep) false) ; can't play a minion that doesn't belong to us
             )
           )}
  [state player-id card]
  (let [hero-mana (get-hero-mana state player-id)
        minion-count (count (get-minions state player-id))
        current-secrets (get-secrets state player-id)
        card-mana-cost (or (:mana-cost card)
                           (get-mana-cost (:name card)))]
    (and (<= card-mana-cost hero-mana)
         (< minion-count 7)
         (= (:owner-id card) player-id)
         (= (:player-id-in-turn state) player-id)
         (or (empty? current-secrets)
             (not (some #(= (:name %) (:name card)) current-secrets))))))