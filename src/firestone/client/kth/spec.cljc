(ns firestone.client.kth.spec
  (:require #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])))

(s/def ::non-negative-int (s/and int? (comp not neg?)))
(s/def ::positive-int (s/and int? pos?))

;; Attributes
(s/def ::name string?)
(s/def ::id string?)
(s/def ::owner-id string?)
(s/def ::set #{:basic
               :classic
               :custom
               :hall-of-fame
               :curse-of-naxxramas
               :goblins-vs-gnomes
               :blackrock-mountain
               :the-grand-tournament
               :the-league-of-explorers
               :whispers-of-the-old-gods
               :one-night-in-karazhan
               :mean-streets-of-gadgetzan
               :journey-to-un'goro
               :knights-of-the-frozen-throne
               :kobolds-and-catacombs
               :the-witchwood
               :the-boomsday-project})

(s/def ::race #{:elemental
                :beast
                :demon
                :murloc
                :mech
                :dragon
                :pirate
                :all
                :totem})

(s/def ::entity-type #{:card
                       :hero
                       :hero-power
                       :minion
                       :permanent
                       :player
                       :secret
                       :weapon})

(s/def :firestone.client.kth.card.spec/type #{:hero
                                              :minion
                                              :spell
                                              :weapon})

(s/def ::rarity #{:none :common :rare :epic :legendary})
(s/def ::class #{:custom :druid :hunter :mage :paladin :priest :rogue :shaman :warlock :warrior})

;; Minions and permanents
(s/def ::position (s/and int? (fn [x] (<= 0 x 6))))
(s/def ::attack ::non-negative-int)
(s/def ::original-attack ::non-negative-int)
(s/def ::health int?)
(s/def ::max-health ::non-negative-int)
(s/def ::original-health pos?)
(s/def ::sleepy boolean?)
(s/def ::can-attack boolean?)
(s/def ::description string?)
(s/def ::valid-attack-ids (s/coll-of ::id))
(s/def ::states (s/coll-of #{:aura
                             :deathrattle
                             :divine-shield
                             :effect
                             :elusive
                             :enrage
                             :frozen
                             :immune
                             :inspire
                             :lifesteal
                             :mega-windfury
                             :poisonous
                             :silenced
                             :spell-damage
                             :stealth
                             :taunt
                             :windfury}))


;; Cards
(s/def ::original-mana-cost ::non-negative-int)
(s/def ::playable boolean?)
(s/def ::special-effect-active boolean?)
(s/def ::valid-target-ids (s/coll-of ::id))

;; Heroes
(s/def ::armor ::non-negative-int)
(s/def ::mana-cost ::non-negative-int)

;; Weapons
(s/def ::durability ::positive-int)
(s/def ::max-durability ::positive-int)
(s/def ::original-durability ::positive-int)

;; Hero powers
(s/def ::can-use boolean?)
(s/def ::has-used-your-turn boolean?)

;; Entities
(s/def ::hero-power (s/and (s/keys :req-un [::can-use
                                            ::owner-id
                                            ::entity-type
                                            ::has-used-your-turn
                                            ::name
                                            ::description]
                                   :opt-un [::mana-cost
                                            ::original-mana-cost
                                            ::valid-target-ids])
                           (fn [hero-power]
                             (= (:entity-type hero-power) :hero-power))))

(s/def ::weapon (s/and (s/keys :req-un [::name
                                        ::owner-id
                                        ::durability
                                        ::entity-type
                                        ::original-durability
                                        ::max-durability
                                        ::attack
                                        ::original-attack
                                        ::states
                                        ::rarity
                                        ::set
                                        ::class])
                       (fn [weapon]
                         (= (:entity-type weapon) :weapon))))

(s/def ::hero (s/keys :req-un [::armor
                               ::owner-id
                               ::entity-type
                               ::attack
                               ::can-attack
                               ::health
                               ::id
                               ::mana
                               ::max-health
                               ::max-mana
                               ::name
                               ::states
                               ::valid-attack-ids]
                      :opt-un [::weapon
                               ::hero-power
                               ::class]))

(s/def ::minion-card (s/and (s/keys :req-un [::entity-type
                                             ::name
                                             ::mana-cost
                                             ::original-mana-cost
                                             ::attack
                                             ::original-attack
                                             ::health
                                             ::original-health
                                             :firestone.client.kth.card.spec/type]
                                    :opt-un [::class
                                             ::owner-id
                                             ::id
                                             ::playable
                                             ::rarity
                                             ::description
                                             ::valid-target-ids
                                             ::set
                                             ::race
                                             ::special-effect-active])
                            (fn [x]
                              (and (= (:entity-type x) :card)
                                   (= (:type x) :minion)))))

(s/def ::spell-card (s/and (s/keys :req-un [::entity-type
                                            ::name
                                            ::mana-cost
                                            ::original-mana-cost
                                            :firestone.client.kth.card.spec/type]
                                   :opt-un [::class
                                            ::owner-id
                                            ::id
                                            ::playable
                                            ::rarity
                                            ::set
                                            ::description
                                            ::valid-target-ids
                                            ::special-effect-active])
                           (fn [x]
                             (and (= (:entity-type x) :card)
                                  (= (:type x) :spell)))))

(s/def ::weapon-card (s/and (s/keys :req-un [::entity-type
                                             ::name
                                             ::mana-cost
                                             ::original-mana-cost
                                             ::attack
                                             ::original-attack
                                             ::durability
                                             ::original-durability
                                             :firestone.client.kth.card.spec/type]
                                    :opt-un [::class
                                             ::owner-id
                                             ::id
                                             ::playable
                                             ::rarity
                                             ::set
                                             ::description
                                             ::valid-target-ids
                                             ::special-effect-active])
                            (fn [x]
                              (and (= (:entity-type x) :card)
                                   (= (:type x) :weapon)))))

(s/def ::hero-card (s/and (s/keys :req-un [::entity-type
                                           ::name
                                           ::mana-cost
                                           ::original-mana-cost
                                           :firestone.client.kth.card.spec/type]
                                  :opt-un [::class
                                           ::owner-id
                                           ::id
                                           ::armor
                                           ::playable
                                           ::set
                                           ::description
                                           ::valid-target-ids
                                           ::special-effect-active])
                          (fn [x]
                            (and (= (:entity-type x) :card)
                                 (= (:type x) :hero)))))

(s/def ::card (s/or :minion-card ::minion-card
                    :spell-card ::spell-card
                    :weapon-card ::weapon-card
                    :hero-card ::hero-card))

;; OLD
;(s/def ::card-in-hand (s/and (s/keys :req-un [::entity-type
;                                              ::id
;                                              ::name
;                                              ::mana-cost
;                                              ::original-mana-cost
;                                              ::owner-id
;                                              ::playable
;                                              ::valid-target-ids
;                                              :firestone.client.kth.card.spec/type]
;                                     :opt-un [::class
;                                              ::attack
;                                              ::original-attack
;                                              ::health
;                                              ::original-health
;                                              ::armor
;                                              ::rarity
;                                              ::description
;                                              ::special-effect-active])
;                             (fn [x]
;                               (= (:entity-type x) :card))))

(s/def ::minion-card-in-hand (s/merge ::minion-card
                                      (s/keys :req-un [::id
                                                       ::owner-id
                                                       ::playable
                                                       ::valid-target-ids])))

(s/def ::spell-card-in-hand (s/merge ::spell-card
                                     (s/keys :req-un [::id
                                                      ::owner-id
                                                      ::playable
                                                      ::valid-target-ids])))

(s/def ::weapon-card-in-hand (s/merge ::weapon-card
                                      (s/keys :req-un [::id
                                                       ::owner-id
                                                       ::playable
                                                       ::valid-target-ids])))

(s/def ::hero-card-in-hand (s/merge ::hero-card
                                    (s/keys :req-un [::id
                                                     ::owner-id
                                                     ::playable
                                                     ::valid-target-ids])))

(s/def ::card-in-hand (s/or :minion-card-in-hand ::minion-card-in-hand
                            :spell-card-in-hand ::spell-card-in-hand
                            :weapon-card-in-hand ::weapon-card-in-hand
                            :hero-card-in-hand ::hero-card-in-hand))

(s/def ::secret (s/and (s/keys :req-un [::name
                                        ::owner-id
                                        ::class
                                        ::id
                                        ::entity-type
                                        ::rarity
                                        ::original-mana-cost
                                        ::description])
                       (fn [secret]
                         (= (:entity-type secret) :secret))))

(s/def ::permanent (s/and (s/keys :req-un [::entity-type
                                           ::id
                                           ::name
                                           ::owner-id
                                           ::position
                                           ::set]
                                  :opt-un [::class
                                           ::rarity
                                           ::description])
                          (fn [x]
                            (= (:entity-type x) :permanent))))

(s/def ::minion (s/and (s/keys :req-un [::attack
                                        ::can-attack
                                        ::entity-type
                                        ::health
                                        ::id
                                        ::name
                                        ::mana-cost
                                        ::max-health
                                        ::original-attack
                                        ::original-health
                                        ::owner-id
                                        ::position
                                        ::set
                                        ::sleepy
                                        ::states
                                        ::valid-attack-ids]
                               :opt-un [::class
                                        ::description
                                        ::rarity])
                       (fn [minion]
                         (= (:entity-type minion) :minion))))

(s/def ::board-entities (s/and (s/coll-of (s/or :minion ::minion
                                                :permanent ::permanent))
                               (fn [board-entities]
                                 (let [board-entities (map second board-entities)
                                       positions (->> board-entities
                                                      (map :position))]
                                   (and (= (count positions) (count (set positions)))
                                        (= board-entities (sort-by :position board-entities))
                                        (= (range (count board-entities))
                                           (sort (map :position board-entities))))))))

(s/def ::active-secrets (s/coll-of ::secret))
(s/def ::deck-size ::non-negative-int)
(s/def ::hand (s/coll-of ::card-in-hand))

(s/def ::player (s/keys :req-un [::board-entities
                                 ::active-secrets
                                 ::deck-size
                                 ::hand
                                 ::hero
                                 ::id]))

(s/def ::player-in-turn ::id)
(s/def ::players (s/coll-of ::player))
(s/def ::minion-id ::id)
(s/def ::card-id ::id)

(s/def ::action-index ::non-negative-int)

(s/def ::game-state (s/keys :req-un [::action-index         ; should increase by one for each action the user has done, like ending the turn, attacking, playing a card, ...
                                     ::id                   ; the id of the game
                                     ::player-in-turn
                                     ::players]))

(s/def ::game-states (s/and vector?
                            (s/coll-of ::game-state)))


(s/def ::supports-redo boolean?)
(s/def ::supports-undo boolean?)
(s/def ::audio #{:auto :events})
(s/def ::volume (s/and int? (fn [x] (<= 0 x 100))))
(s/def ::effect-volume ::volume)
(s/def ::background-volume ::volume)

(s/def ::engine-settings (s/keys :req-un [::supports-redo
                                          ::supports-undo
                                          ::audio]
                                 :opt-un [::effect-volume
                                          ::background-volume]))