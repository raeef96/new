(ns firestone.client.kth.full-spec
  (:require #?(:clj  [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])))

(s/def ::non-negative-int (s/and int? (comp not neg?)))
(s/def ::positive-int (s/and int? pos?))

;; Attributes
(s/def ::name string?)
(s/def ::display-name string?)
(s/def ::external-id string?)
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
                       :quest
                       :secret
                       :weapon})

(s/def :firestone.client.kth.card.spec/type #{:hero
                                              :minion
                                              :spell
                                              :weapon})

(s/def ::rarity #{:none :common :rare :epic :legendary})
(s/def ::class #{:custom :druid :hunter :mage :paladin :priest :rogue :shaman :warlock :warrior})
(s/def ::class-group #{:grimy-goons :jade-lotus :kabal})

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
                                             ::class-group
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

(s/def ::reward-card ::card)

(s/def ::counter ::non-negative-int)
(s/def ::goal ::non-negative-int)

(s/def ::progress (s/and (s/keys :req-un [::counter
                                          ::goal])))

(s/def ::quest (s/and (s/keys :req-un [::id
                                       ::owner-id
                                       ::entity-type
                                       ::progress
                                       ::card
                                       ::reward-card])
                      (fn [x]
                        (= (:entity-type x) :quest))))

(s/def ::permanent (s/and (s/keys :req-un [::entity-type
                                           ::id
                                           ::name
                                           ::owner-id
                                           ::position
                                           ::set]
                                  :opt-un [::class
                                           ::rarity
                                           ::description
                                           ::display-name
                                           ::external-id])
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
                                        ::class-group
                                        ::description
                                        ::display-name
                                        ::external-id
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
(s/def ::active-quest ::quest)
(s/def ::deck-size ::non-negative-int)
(s/def ::hand (s/coll-of ::card-in-hand))

(s/def ::player (s/keys :req-un [::board-entities
                                 ::active-secrets
                                 ::deck-size
                                 ::hand
                                 ::hero
                                 ::id]
                        :opt-un [::active-quest]))

(s/def ::player-in-turn ::id)
(s/def ::players (s/coll-of ::player))
(s/def :firestone.client.kth.game-blocker.spec/type #{:adapt :choose-one :discover :general-choice})
(s/def ::minion-id ::id)
(s/def ::card-id ::id)

(s/def :firestone.client.kth.discover.spec/options (s/coll-of (s/keys :req-un [::name
                                                                               ::mana-cost
                                                                               ::original-mana-cost
                                                                               :firestone.client.kth.card.spec/type]
                                                                      :opt-un [::description
                                                                               ::display-name
                                                                               ::external-id])))

(s/def ::discover (s/and (s/keys :req-un [:firestone.client.kth.discover.spec/options
                                          :firestone.client.kth.game-blocker.spec/type])
                         (fn [x] (= (:type x) :discover))))

(s/def :firestone.client.kth.general-choice.spec/options (s/coll-of (s/keys :req-un [::name
                                                                                     ::description
                                                                                     ::mana-cost
                                                                                     ::original-mana-cost
                                                                                     :firestone.client.kth.card.spec/type]
                                                                            :opt-un [::display-name
                                                                                     ::external-id])))

(s/def ::general-choice (s/and (s/keys :req-un [:firestone.client.kth.general-choice.spec/options
                                                :firestone.client.kth.game-blocker.spec/type])
                               (fn [x] (= (:type x) :general-choice))))


(s/def ::choosable boolean?)

(s/def :firestone.client.kth.choose-one.spec/options (s/coll-of (s/keys :req-un [::name
                                                                                 ::description
                                                                                 ::type
                                                                                 ::rarity
                                                                                 ::set
                                                                                 ::valid-target-ids
                                                                                 ::choosable]
                                                                        :opt-un [::display-name
                                                                                 ::external-id])))

(s/def :firestone.client.kth.adapt.spec/options (s/coll-of (s/keys :req-un [::name
                                                                            ::description])))

(s/def ::choose-one (s/and (s/keys :req-un [:firestone.client.kth.choose-one.spec/options
                                            :firestone.client.kth.game-blocker.spec/type])
                           (fn [x] (= (:type x) :choose-one))))

(s/def ::adapt (s/and (s/keys :req-un [:firestone.client.kth.adapt.spec/options
                                       :firestone.client.kth.game-blocker.spec/type])
                      (fn [x] (= (:type x) :adapt))))


(s/def ::game-blocker (s/or :adapt ::adapt
                            :choose-one ::choose-one
                            :discover ::discover
                            :general-choice ::general-choice))

(s/def ::entity (s/or :card ::card
                      :hero ::hero
                      :hero-power ::hero-power
                      :minion ::minion
                      :permanent ::permanent
                      :quest ::quest
                      :secret ::secret
                      :weapon ::weapon
                      :player ::player))

(s/def ::character (s/or :minion ::minion
                         :permanent ::permanent
                         :hero ::hero))
(s/def :firestone.client.kth.event.spec/name keyword?)
(s/def :firestone.client.kth.event/hero-power-name string?)
(s/def :firestone.client.kth.event/attacker ::character)

(s/def ::after-attack-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                    :firestone.client.kth.event/attacker])
                                   (fn [x] (= (:name x) :after-attack))))

(s/def ::after-hero-power-used-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                             :firestone.client.kth.event/hero-power-name])
                                            (fn [x] (= (:name x) :after-hero-power-used))))

(s/def ::after-hero-card-played-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                              ::card])
                                             (fn [x] (= (:name x) :after-hero-card-played))))

(s/def ::after-minion-card-played-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                                ::card])
                                               (fn [x] (= (:name x) :after-minion-card-played))))

(s/def ::after-spell-card-played-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                               ::card])
                                              (fn [x] (= (:name x) :after-spell-card-played))))

(s/def ::choose-one-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                  :firestone.client.kth.choose-one.spec/options])
                                 (fn [x] (= (:name x) :choose-one))))

(s/def ::start-turn-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name])
                                 (fn [x] (= (:name x) :start-turn))))

(s/def ::start-of-game-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name])
                                    (fn [x] (= (:name x) :start-of-game))))

(s/def ::after-weapon-card-played-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                                ::card])
                                               (fn [x] (= (:name x) :after-weapon-card-played))))

(s/def ::add-card-to-hand-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                        ::card])
                                       (fn [x] (= (:name x) :add-card-to-hand))))

(s/def ::card-drawn-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                  ::card])
                                 (fn [x] (= (:name x) :card-drawn))))

(s/def ::amount int?)

(s/def :firestone.client.kth.event.spec/character-amount (s/keys :req-un [::character
                                                                          ::amount]))

(s/def :firestone.client.kth.event.spec/character-amount-seq (s/coll-of :firestone.client.kth.event.spec/character-amount))

(s/def ::damage-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                              :firestone.client.kth.event.spec/character-amount-seq])
                             (fn [x] (= (:name x) :damage))))

(s/def ::heal-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                            :firestone.client.kth.event.spec/character-amount-seq])
                           (fn [x] (= (:name x) :heal))))

(s/def ::minion-summoned-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                       ::minion])
                                      (fn [x] (= (:name x) :minion-summoned))))

(s/def ::minion-transformed-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                          ::minion])
                                         (fn [x] (= (:name x) :minion-transformed))))

(s/def ::secret-added-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                    ::secret])
                                   (fn [x] (= (:name x) :secret-added))))

(s/def ::destroyed-entities-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                                          ::entities])
                                         (fn [x] (= (:name x) :destroyed-entities))))

(s/def ::trigger-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                               ::entity])
                              (fn [x] (= (:name x) :trigger))))

(s/def ::quest-event (s/and (s/keys :req-un [:firestone.client.kth.event.spec/name
                                             ::quest])
                            (fn [x] (= (:name x) :quest))))

(s/def ::event (s/or :add-card-to-hand ::add-card-to-hand-event
                     :after-attack ::after-attack-event
                     :after-hero-card-played ::after-hero-card-played-event
                     :after-hero-power-used ::after-hero-power-used-event
                     :after-minion-card-played ::after-minion-card-played-event
                     :after-spell-card-played ::after-spell-card-played-event
                     :start-of-game ::start-of-game-event
                     :start-turn ::start-turn-event
                     :after-weapon-card-played ::after-weapon-card-played-event
                     :card-drawn ::card-drawn-event
                     :choose-one ::choose-one-event
                     :damage ::damage-event
                     :destroyed-entities-event ::destroyed-entities-event
                     :heal ::heal-event
                     :minion-summoned ::minion-summoned-event
                     :minion-transformed ::minion-transformed-event
                     :secret-added ::secret-added-event
                     :trigger ::trigger-event
                     :quest ::quest-event))

(s/def ::action-index ::non-negative-int)

(s/def ::game-state (s/keys :req-un [::action-index         ; should increase by one for each action the user has done, like ending the turn, attacking, playing a card, ...
                                     ::id                   ; the id of the game
                                     ::player-in-turn
                                     ::players]
                            :opt-un [::event
                                     ::game-blocker]))

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