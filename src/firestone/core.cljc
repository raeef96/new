(ns firestone.core
  "A namespace for the business logic of the game."
  (:require [ysera.test :refer [is is-not is= error?]]  ; Add error? here
            [ysera.collections :refer [seq-contains?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-hero
                                         create-minion
                                         create-empty-state
                                         get-heroes
                                         get-minion
                                         get-minions
                                         get-deck
                                         get-hand
                                         add-minion-to-board
                                         set-mana
                                         create-card
                                         get-character
                                         get-all-characters
                                         deduct-player-mana
                                         get-max-mana
                                         handle-minion-attack-on-minion
                                         handle-minion-attack-on-hero
                                         can-play-minion?
                                         remove-card-from-hand
                                         update-minion
                                         get-player-id-in-turn
                                         effects-parser
                                         update-hero]]))


(defn get-entity-type
  "Returns the entity type (:hero or :minion) of an entity given its ID."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-entity-type "h1"))
                :hero))}
  [state id]
  (let [entity (get-character state id)]
    (cond
      (= (:entity-type entity) :hero) :hero
      (= (:entity-type entity) :minion) :minion
      :else (error "Unknown entity type"))))

(defn get-attack
  "Returns the attack of the minion with the given id, including any aura effects from adjacent minions and temporary buffs."
  {:test (fn []
           ; Test basic attack value
           (is= (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}])
                    (get-attack "m1"))
                1)

           ; Test with Dire Wolf Alpha adjacent (position difference of 1)
           (let [state (-> (create-empty-state)
                           (add-minion-to-board "p1" (create-minion "Sheep" :id "m1") 0)
                           (add-minion-to-board "p1" (create-minion "Dire Wolf Alpha" :id "m2") 1))]
             (println "Sheep position:" (:position (get-minion state "m1")))
             (println "Dire Wolf position:" (:position (get-minion state "m2")))
             (is= (get-attack state "m1") 2))

           ; Test with Dire Wolf Alpha not adjacent
           (let [state (-> (create-empty-state)
                           (add-minion-to-board "p1" (create-minion "Sheep" :id "m1") 0)
                           (add-minion-to-board "p1" (create-minion "Dire Wolf Alpha" :id "m2") 2))]
             (println "Sheep position:" (:position (get-minion state "m1")))
             (println "Dire Wolf position:" (:position (get-minion state "m2")))
             (is= (get-attack state "m1") 1)))}
  [state id]
  (let [minion       (get-minion state id)
        definition   (get-definition (:name minion))
        base-attack  (or (:attack definition) 0)
        owner-id     (:owner-id minion)
        position     (:position minion)
        player-minions (get-minions state owner-id)

        ;; ADDED: Get temporary attack bonus from overrides
        attack-bonus (get-in minion [:overrides :attack-bonus] 0)

        ;; Calculate aura bonus from adjacent minions (specifically Dire Wolf Alpha)
        aura-bonus   (reduce (fn [bonus adjacent-minion]
                               (let [adjacent-position (:position adjacent-minion)
                                     adjacent-name     (:name adjacent-minion)]
                                 ;; Check if the minion is adjacent (position difference is 1)
                                 (if (and (= "Dire Wolf Alpha" adjacent-name)
                                          (= 1 (Math/abs (int (- position adjacent-position)))))
                                   ;; Add the aura value from the definition
                                   (+ bonus (or (:aura (get-definition adjacent-name)) 0))
                                   ;; No aura effect from this minion
                                   bonus)))
                             0
                             player-minions)]

    (+ base-attack aura-bonus attack-bonus)))


(defn sleepy?
  "Checks if the minion with given id is sleepy."
  {:test (fn []
           ; Minion in summoned-this-turn list should be sleepy
           (is (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}]
                                :minion-ids-summoned-this-turn ["m1"])
                   (sleepy? "m1")))

           ; Minion with sleepy property explicitly set
           (is (-> (create-game [{:minions [(create-minion "Sheep" :id "m1" :sleepy true)]}])
                   (sleepy? "m1")))

           ; Minion that is not sleepy
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1" :sleepy false)]}])
                       (sleepy? "m1"))))}
  [state id]
  (let [minion (get-minion state id)]
    (or (seq-contains? (:minion-ids-summoned-this-turn state) id)
        (:sleepy minion))))


(defn valid-attack?
  "Checks if the attack is valid"
  {:test (fn []
           ; Should be able to attack an enemy minion
           (is (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}
                                 {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}])
                   (valid-attack? "p1" "m1" "m2")))
           ; Should be able to attack an enemy hero
           (is (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                   (valid-attack? "p1" "m" "h2")))
           ; Should not be able to attack your own minions
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")
                                                (create-minion "Boulderfist Ogre" :id "m2")]}])
                       (valid-attack? "p1" "m1" "m2")))
           ; Should not be able to attack if it is not your turn
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}
                                     {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}]
                                    :player-id-in-turn "p2")
                       (valid-attack? "p1" "m1" "m2")))
           ; Should not be able to attack if you are sleepy
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}
                                     {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}]
                                    :minion-ids-summoned-this-turn ["m1"])
                       (valid-attack? "p1" "m" "bo")))
           ; Should not be able to attack if you already attacked this turn
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1" :attacks-performed-this-turn 1)]}
                                     {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}])
                       (valid-attack? "p1" "m1" "m2"))))}
  [state player-id attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-character state target-id)]
    (and attacker
         target
         (= (:player-id-in-turn state) player-id)
         (< (:attacks-performed-this-turn attacker) 1)
         (not (sleepy? state attacker-id)) ;; Use our improved sleepy? function
         (not= (:owner-id target) (:player-id-in-turn state))
         (not= (:owner-id attacker) (:owner-id target)))))

(defn draw-card
  {:test (fn []
           ; Test drawing a card from a non-empty deck
           (let [state (-> (create-game [{:deck ["Boulderfist Ogre"]}])
                           (draw-card "p1"))]
             (is (empty? (get-deck state "p1")))
             (is= (->> (get-hand state "p1")
                       (map :name))
                  ["Boulderfist Ogre"]))

           ; Test drawing from an empty deck (should just return state unchanged)
           (let [initial-state (create-game [{:deck []}])
                 result-state (draw-card initial-state "p1")]
             (is= initial-state result-state))

           ; Test drawing when hand is full (card should be burned)
           (let [full-hand (repeatedly 10 #(create-card "Sheep"))
                 state (-> (create-game [{:deck ["Boulderfist Ogre"]
                                          :hand full-hand}])
                           (draw-card "p1"))]
             (is (empty? (get-deck state "p1")))
             (is= (count (get-hand state "p1")) 10)))}
  [state player-id]
  (let [deck (get-in state [:players player-id :deck])
        hand (get-in state [:players player-id :hand])]
    (if (not (empty? deck))
      (let [drawn-card (first deck)
            new-deck (rest deck)
            new-hand (if (>= (count hand) 10)
                       hand  ; Hand is full, card is burned
                       (conj (vec hand) drawn-card))]  ; Add card to the hand
        (-> state
            (assoc-in [:players player-id :deck] new-deck)
            (assoc-in [:players player-id :hand] new-hand)))
      state)))

(defn attack
  "Allows a minion to attack another minion or a hero after validating the attack."
  [state player-id attacker-id target-id]
  {:pre [(map? state)                     ; Ensure state is a map
         (string? player-id)              ; Ensure player-id is a string
         (string? attacker-id)            ; Ensure attacker-id is a string
         (string? target-id)]}            ; Ensure target-id is a string
  (if (valid-attack? state player-id attacker-id target-id)
    (let [type (get-entity-type state target-id)]
      (cond
        (= type :minion) (handle-minion-attack-on-minion {:state state
                                                          :attacker-id attacker-id
                                                          :target-id target-id})
        (= type :hero)   (handle-minion-attack-on-hero {:state state
                                                        :attacker-id attacker-id
                                                        :target-id target-id})
        :else            (error "card doesn't exist")))
    (error "Invalid attack")))

;; Add these helper functions before the play-card function in firestone.core

(defn validate-player-turn
  "Validates that it's the player's turn. Throws error if not."
  [state player-id]
  (when (not= (get-player-id-in-turn state) player-id)
    (error "It's not the player's turn.")))

(defn find-card-in-hand
  "Finds a card by ID in the player's hand. Throws error if not found."
  [state player-id card-id]
  (let [hand (get-hand state player-id)
        card (some #(when (= (:id %) card-id) %) hand)]
    (when (nil? card)
      (error "The player does not have the specified card in their hand."))
    card))

(defn normalize-position
  "Normalizes position value to a number, defaulting to 0 if invalid."
  [position]
  (cond
    (number? position) position
    (string? position) (try (Integer/parseInt position)
                            (catch Exception e 0))
    :else 0))

(defn play-minion-card
  "Handles playing a minion card."
  [state player-id card position]
  (when-not (can-play-minion? state player-id card)
    (error "Cannot play the minion card."))

  (let [card-def (get-definition (:name card))
        mana-cost (or (:mana-cost card-def) 0)

        ;; Use shadowing instead of creating new variable names
        state (update-hero state player-id :mana #(- % mana-cost))
        state (remove-card-from-hand state player-id card)

        new-minion (create-minion (:name card)
                                  :sleepy true
                                  :effects (when (:effect card-def) [(:effect card-def)]))
        state (add-minion-to-board state player-id new-minion position)

        placed-minion (last (get-minions state player-id))

        state (update state :minion-ids-summoned-this-turn
                      #(conj (or % []) (:id placed-minion)))]

    ;; Return state and placed-minion for battlecry processing
    {:state state :minion placed-minion}))

(defn play-spell-card
  "Handles playing a spell card."
  [state player-id card target-id]
  (let [card-def (get-definition (:name card))
        mana-cost (or (:mana-cost card-def) 0)
        spell-effect-name (:spell-effect card-def)
        current-mana (get-in state [:players player-id :hero :mana] 0)]

    (when-not (and spell-effect-name (>= current-mana mana-cost))
      (error "Cannot play the spell card."))

    (let [;; Use shadowing
          state (update-hero state player-id :mana #(- % mana-cost))
          state (remove-card-from-hand state player-id card)
          ;; Match original exactly - include effects array with spell-effect-name
          spell-entity {:id (:id card)
                        :name (:name card)
                        :effects [spell-effect-name]}]

      (effects-parser state
                      [spell-entity]
                      player-id
                      :spell-effect
                      {:target-id target-id}))))

(defn play-card
  "Handles playing a card by its type (minion or spell)."
  [state player-id card-id target-id position]
  ;; Use when for side-effect validation
  (validate-player-turn state player-id)

  (let [card (find-card-in-hand state player-id card-id)
        card-def (get-definition (:name card))
        card-type (:type card-def)

        ;; Check if position is actually a target ID (starts with "m")
        position-is-target? (and (string? position)
                                 (clojure.string/starts-with? position "m"))

        ;; Handle case where position might actually be a target ID
        effective-target-id (if position-is-target? position target-id)

        ;; Normalize position value for minion placement
        position-num (if position-is-target? 0 (normalize-position position))

        ;; Resolve target ID using the exact same logic as original
        actual-target-id (cond
                           (nil? effective-target-id) nil

                           (and (string? effective-target-id)
                                (or (clojure.string/starts-with? effective-target-id "m")
                                    (clojure.string/starts-with? effective-target-id "h")))
                           effective-target-id

                           (or (number? effective-target-id)
                               (re-matches #"\d+" (str effective-target-id)))
                           (let [pos (if (number? effective-target-id)
                                       effective-target-id
                                       (Integer/parseInt (str effective-target-id)))
                                 enemy-id (if (= player-id "p1") "p2" "p1")
                                 all-minions (concat (get-minions state player-id)
                                                     (get-minions state enemy-id))
                                 minion (first (filter #(= (:position %) pos) all-minions))]
                             (:id minion))

                           :else effective-target-id)]

    (condp = card-type
      :minion
      (let [{:keys [state minion]} (play-minion-card state player-id card position-num)
            ;; Process battlecry
            state (effects-parser state
                                  [minion]
                                  player-id
                                  :battlecry
                                  {:target-id actual-target-id})
            ;; Process play-card effects
            state (effects-parser state
                                  (get-all-characters state)
                                  player-id
                                  :play-card
                                  {:card-id card-id})]
        state)

      :spell
      (play-spell-card state player-id card actual-target-id)

      ;; Default case
      (error "Unknown card type."))))

(defn get-valid-attacks
  "Updates valid attack targets for each minion of the player in turn."
  {:test (fn []
           ; Test with player minions that can attack
           (let [state (create-game [{:minions [(create-minion "Sheep" :id "m1" :sleepy false
                                                               :attacks-performed-this-turn 0)]}
                                     {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}])]
             (is (contains? (-> state
                                (get-valid-attacks)
                                (get-minion "m1")
                                (:valid-attack-ids)
                                (set))
                            "m2")))
           ; Test that enemy hero is a valid target
           (let [state (create-game [{:minions [(create-minion "Sheep" :id "m1" :sleepy false
                                                               :attacks-performed-this-turn 0)]}])]
             (is (contains? (-> state
                                (get-valid-attacks)
                                (get-minion "m1")
                                (:valid-attack-ids)
                                (set))
                            "h2"))))}
  [state]
  (let [player-id-in-turn (get state :player-id-in-turn)
        player-change-fn {"p1" "p2"
                          "p2" "p1"}
        opponent-id (player-change-fn player-id-in-turn)
        opponent-minions (get-minions state opponent-id)
        opponent-minion-ids (map :id (filter (fn [m] (not (:stealth m))) opponent-minions))
        hero-id (get-in state [:players opponent-id :hero :id])]
    (reduce
      (fn [new-state minion]
        (let [minion-id (:id minion)
              valid-ids (conj opponent-minion-ids hero-id)]
          (update-minion new-state minion-id :valid-attack-ids valid-ids)))
      state
      (get-minions state player-id-in-turn))))

(defn use-hero-power
  "Allows a player to use their hero power, validating they can use it and applying its effects."
  [state player-id target-id]
  {:pre [(map? state)
         (string? player-id)]}

  (when (not= (get-player-id-in-turn state) player-id)
    (error "It's not the player's turn."))

  (let [hero (get-in state [:players player-id :hero])
        hero-def (get-definition (:name hero))
        hero-power-name (:hero-power hero-def)
        hero-power-def (when hero-power-name (get-definition hero-power-name))]

    (when (nil? hero-power-def)
      (error "Hero power not found."))

    (let [mana-cost (:mana-cost hero-power-def 2)
          current-mana (:mana hero 0)
          hero-power-used? (:has-used-your-turn hero false)]

      (when hero-power-used?
        (error "Hero power already used this turn."))

      (when (< current-mana mana-cost)
        (error "Not enough mana to use hero power."))

      (let [state (update-hero state player-id :mana #(- % mana-cost))
            state (update-hero state player-id :has-used-your-turn true)
            hero-power-fn (:power hero-power-def)]

        (when (nil? hero-power-fn)
          (error "Hero power has no effect defined."))

        (case hero-power-name
          "Life Tap"
          (hero-power-fn state player-id)

          "Fireblast"
          (do
            (when (nil? target-id)
              (error "Fireblast requires a target."))
            (hero-power-fn state target-id))

          ;; default case - using when with or for conditional logic
          (or (when target-id
                (hero-power-fn state player-id target-id))
              (hero-power-fn state player-id)))))))