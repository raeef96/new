(ns firestone.definition.card
  (:require [firestone.definitions :refer [add-definitions!
                                           get-definition
                                           get-definitions]]
            [firestone.core :refer [draw-card]]
            [firestone.construct :refer [create-minion
                                         get-minions
                                         add-minion-to-board
                                         add-card-to-hand
                                         remove-minion
                                         get-hand
                                         get-hero
                                         get-minion
                                         update-minion
                                         effects-parser
                                         create-card
                                         add-card-to-secrets
                                         get-all-characters
                                         update-hero]]))

;; Card definitions - properties that describe each card
(def card-definitions
  {
   "Boulderfist Ogre"
   {:name      "Boulderfist Ogre"
    :attack    6
    :health    7
    :mana-cost 6
    :type      :minion
    :set       :basic}

   "Leper Gnome"
   {:name        "Leper Gnome"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :set         :classic
    :rarity      :common
    :effect      "Leper Gnome effect"
    :description "Deathrattle: Deal 2 damage to the enemy hero."}

   "Loot Hoarder"
   {:name        "Loot Hoarder"
    :attack      2
    :health      1
    :mana-cost   2
    :type        :minion
    :set         :classic
    :rarity      :common
    :effect      "Loot Hoarder effect"
    :description "Deathrattle: Draw a card."}

   "Sheep"
   {:name      "Sheep"
    :attack    1
    :health    1
    :mana-cost 1
    :race      :beast
    :type      :minion
    :set       :basic}

   "Moroes"
   {:name        "Moroes"
    :attack      1
    :health      1
    :mana-cost   3
    :stealth     1
    :effect      "Moroes effect"
    :description "Stealth. At the end of your turn, summon a 1/1 Steward."
    :rarity      :legendary
    :set         :one-night-in-karazhan
    :type        :minion}

   "Consecration"
   {:name         "Consecration"
    :mana-cost    4
    :description  "Deal 2 damage to all enemies."
    :spell-effect "Consecration spell effect"
    :class        :paladin
    :set          :basic
    :type         :spell}

   "Kill Command"
   {:name         "Kill Command"
    :mana-cost    3
    :class        :hunter
    :set          :basic
    :type         :spell
    :spell-effect "Kill Command spell effect"
    :description  "Deal 3 damage. If you control a Beast deal 5 damage instead."}

   "Silver Hand Knight"
   {:name        "Silver Hand Knight"
    :attack      4
    :health      4
    :mana-cost   5
    :effect      "Silver Hand Knight effect"
    :description "Battlecry: Summon a 2/2 Squire."
    :rarity      :common
    :set         :classic
    :type        :minion}

   "Abusive Sergeant"
   {:name        "Abusive Sergeant"
    :attack      1
    :health      1
    :mana-cost   1
    :effect      "Abusive Sergeant effect"
    :rarity      :common
    :set         :classic
    :type        :minion
    :description "Battlecry: Give a minion +2 Attack this turn."}

   "Stampeding Kodo"
   {:name        "Stampeding Kodo"
    :attack      3
    :mana-cost   5
    :health      5
    :effect      "Stampeding Kodo effect"
    :description "Battlecry: Destroy a random enemy minion with 2 or less Attack."
    :type        :minion
    :race        :beast
    :set         :classic
    :rarity      :rare}

   "Mad Bomber"
   {:name        "Mad Bomber"
    :attack      3
    :health      2
    :mana-cost   2
    :effect      "Mad Bomber effect"
    :description "Battlecry: Deal 3 damage randomly split between all other characters."
    :rarity      :common
    :set         :classic
    :type        :minion}

   "Twilight Drake"
   {:name        "Twilight Drake"
    :attack      4
    :health      1
    :mana-cost   4
    :effect      "Twilight Drake effect"
    :description "Battlecry: Gain +1 Health for each card in your hand."
    :race        :dragon
    :type        :minion
    :set         :classic
    :rarity      :rare}

   "Dr. Boom"
   {:name        "Dr. Boom"
    :attack      7
    :health      7
    :mana-cost   7
    :effect      "Dr. Boom effect"
    :description "Battlecry: Summon two 1/1 Boom Bots. WARNING: Bots may explode."
    :rarity      :legendary
    :set         :goblins-vs-gnomes
    :type        :minion}

   "Blood Imp"
   {:name        "Blood Imp"
    :attack      0
    :health      1
    :mana-cost   1
    :stealth     1
    :effect      "Blood Imp effect"
    :description "Stealth. At the end of your turn give another random friendly minion +1 Health."
    :race        :demon
    :type        :minion
    :class       :warlock
    :set         :classic
    :rarity      :common}

   "Equality"
   {:name         "Equality"
    :mana-cost    2
    :description  "Change the Health of ALL minions to 1."
    :spell-effect "Equality spell effect"
    :class        :paladin
    :set          :classic
    :type         :spell}

   "Feign Death"
   {:name         "Feign Death"
    :mana-cost    2
    :description  "Trigger all Deathrattles on your minions."
    :spell-effect "Feign Death spell effect"
    :class        :hunter
    :rarity       :epic
    :set          :goblins-vs-gnomes
    :type         :spell}

   "Squire"
   {:name      "Squire"
    :attack    2
    :health    2
    :mana-cost 1
    :set       :classic
    :rarity    :common
    :type      :minion}

   "Boom Bot"
   {:name        "Boom Bot"
    :attack      1
    :health      1
    :mana-cost   1
    :effect      "Boom Bot effect"
    :type        :minion
    :race        :mech
    :set         :goblins-vs-gnomes
    :description "Deathrattle: Deal 1-4 damage to a random enemy."}

   "Steward"
   {:name      "Steward"
    :attack    1
    :health    1
    :mana-cost 1
    :type      :minion
    :set       :one-night-in-karazhan}

   "Knife Juggler"
   {:name        "Knife Juggler"
    :attack      3
    :health      2
    :mana-cost   2
    :effect      "Knife Juggler effect"
    :rarity      :rare
    :set         :classic
    :type        :minion
    :description "After you summon a minion, deal 1 damage to a random enemy."}

   "Questing Adventurer"
   {:name        "Questing Adventurer"
    :attack      2
    :health      2
    :mana-cost   3
    :effect      "Questing Adventurer effect"
    :rarity      :rare
    :set         :classic
    :type        :minion
    :description "Whenever you play a card, gain +1/+1."}

   "Dire Wolf Alpha"
   {:name        "Dire Wolf Alpha"
    :attack      2
    :health      2
    :mana-cost   2
    :effect      "Dire Wolf Alpha effect"
    :set         :classic
    :race        :beast
    :type        :minion
    :rarity      :common
    :aura        1
    :description "Adjacent minions have +1 Attack."}

   "Explosive Trap"
   {:name         "Explosive Trap"
    :mana-cost    2
    :class        :hunter
    :rarity       :common
    :set          :classic
    :type         :spell
    :spell-effect "Explosive Trap spell effect"
    :description  "Secret: When your hero is attacked deal 2 damage to all enemies."}

   "Cat Trick"
   {:name         "Cat Trick"
    :mana-cost    2
    :class        :hunter
    :rarity       :rare
    :set          :one-night-in-karazhan
    :type         :spell
    :spell-effect "Cat Trick spell effect"
    :description  "Secret: After your opponent casts a spell, summon a 4/2 Panther with Stealth."}

   "Cat in a Hat"
   {:name        "Cat in a Hat"
    :attack      4
    :health      2
    :mana-cost   3
    :stealth     1
    :class       :hunter
    :type        :minion
    :set         :one-night-in-karazhan
    :description "Stealth"}

  "Alexstrasza"
  {:name        "Alexstrasza"
   :attack      8
   :mana-cost   9
   :health      8
   :race        :dragon
   :effect      "Alexstrasza effect"
   :type        :minion
   :set         :classic
   :rarity      :legendary
   :description "Battlecry: Set a hero's remaining Health to 15."}

  "Master of Disguise"
  {:name        "Master of Disguise"
   :attack      4
   :health      4
   :mana-cost   4
   :effect      "Master of Disguise effect"
   :type        :minion
   :class       :rogue
   :set         :classic
   :rarity      :rare
   :description "Battlecry: Give a friendly minion Stealth until your next turn."}

  "Infest"
  {:name         "Infest"
   :mana-cost    3
   :class        :hunter
   :rarity       :rare
   :set          :whispers-of-the-old-gods
   :type         :spell
   :spell-effect "Infest spell effect"
   :description  "Give your minions \"Deathrattle: Add a random Beast to your hand.\""}

  "Ragnaros the Firelord"
  {:name        "Ragnaros the Firelord"
   :mana-cost   8
   :health      8
   :attack      8
   :effect      "Ragnaros the Firelord effect"
   :race        :elemental
   :type        :minion
   :set         :hall-of-fame
   :rarity      :legendary
   :description "Can't attack. At the end of your turn, deal 8 damage to a random enemy."}

  "Shadow Sensei"
  {:name        "Shadow Sensei"
   :attack      4
   :health      4
   :mana-cost   4
   :effect      "Shadow Sensei effect"
   :type        :minion
   :class       :rogue
   :set         :mean-streets-of-gadgetzan
   :rarity      :rare
   :description "Battlecry: Give a Stealthed minion +2/+2."}

  "Frothing Berserker"
  {:name        "Frothing Berserker"
   :mana-cost   3
   :health      4
   :attack      2
   :effect      "Frothing Berserker effect"
   :type        :minion
   :class       :warrior
   :set         :classic
   :rarity      :rare
   :description "Whenever a minion takes damage, gain +1 Attack."}

  "Baron Geddon"
  {:name        "Baron Geddon"
   :attack      7
   :health      5
   :mana-cost   7
   :effect      "Baron Geddon effect"
   :race        :elemental
   :type        :minion
   :set         :classic
   :rarity      :legendary
   :description "At the end of your turn, deal 2 damage to ALL other characters."}

  "Misdirection"
  {:name         "Misdirection"
   :mana-cost    2
   :class        :hunter
   :rarity       :rare
   :set          :classic
   :type         :spell
   :spell-effect "Misdirection spell effect"
   :description  "Secret: When a character attacks your hero instead he attacks another random character."}

  "Raid Leader"
  {:name        "Raid Leader"
   :attack      2
   :health      2
   :mana-cost   3
   :effect      "Raid Leader effect"
   :set         :basic
   :type        :minion
   :description "Your other minions have +1 Attack."}

  "Shudderwock"
  {:name        "Shudderwock"
   :attack      6
   :health      6
   :mana-cost   9
   :effect      "Shudderwock effect"
   :type        :minion
   :class       :shaman
   :set         :the-witchwood
   :rarity      :legendary
   :description "Battlecry: Repeat all other Battlecries from cards you played this game (targets chosen randomly)."}

  "Unearthed Raptor"
  {:name        "Unearthed Raptor"
   :attack      3
   :health      4
   :mana-cost   3
   :effect      "Unearthed Raptor effect"
   :type        :minion
   :class       :rogue
   :set         :the-league-of-explorers
   :rarity      :rare
   :description "Battlecry: Choose a friendly minion. Gain a copy of its Deathrattle effect."}
})




;; Effect definitions - functions that implement effects for cards
(def effect-definitions
  {
   ;; Deathrattle effects
   "Leper Gnome effect"
   {:name        "Leper Gnome effect"
    :description "Deathrattle: Deal 2 damage to the enemy hero."
    :deathrattle (fn [state id player-id enemy-id target-id]
                   (update-hero state enemy-id :damage-taken #(+ % 2)))}

   "Loot Hoarder effect"
   {:name        "Loot Hoarder effect"
    :description "Deathrattle: Draw a card."
    :deathrattle (fn [state id player-id enemy-id target-id]
                   (draw-card state player-id))}

   "Boom Bot effect"
   {:name        "Boom Bot effect"
    :description "Deathrattle: Deal 1-4 damage to a random enemy."
    :deathrattle (fn [state id player-id enemy-id target-id]
                   (let [damage (inc (rand-int 4))
                         enemy-minions (get-minions state enemy-id)
                         enemy-hero (get-hero state enemy-id)
                         targets (cons enemy-hero enemy-minions)
                         target (rand-nth targets)]
                     (if (= (:entity-type target) :hero)
                       (update-hero state enemy-id :damage-taken #(+ % damage))
                       (update-minion state (:id target) :damage-taken #(+ % damage)))))}

   ;; End of turn effects
   "Moroes effect"
   {:name        "Moroes effect"
    :description "At the end of your turn, summon a 1/1 Steward."
    :end-of-turn (fn [state id player-id enemy-id target-id]
                   (let [source-minion (get-minion state id)
                         source-position (:position source-minion)

                         ;; Look for Dire Wolf Alpha minions on the board
                         player-minions (get-minions state player-id)
                         dire_wolves (filter #(= (:name %) "Dire Wolf Alpha") player-minions)

                         ;; Calculate the best position for maximum aura benefit
                         best-position (if (seq dire_wolves)
                                         ;; If there's a Dire Wolf Alpha, place adjacent to it
                                         (let [wolf (first dire_wolves)
                                               wolf-position (:position wolf)]
                                           ;; Choose a position adjacent to the wolf
                                           (if (> wolf-position source-position)
                                             ;; If wolf is to the right, place the Steward between Moroes and the wolf
                                             (max 0 (min (inc source-position) wolf-position))
                                             ;; If wolf is to the left, place the Steward between Moroes and the wolf
                                             (max 0 (min source-position (inc wolf-position)))))
                                         ;; Default position right next to Moroes
                                         (inc source-position))]

                     ;; Add the minion at the calculated position
                     (-> state
                         (add-minion-to-board player-id (create-minion "Steward") best-position)
                         ;; After adding the minion, make sure to recalculate all aura effects
                         (effects-parser (get-minions state player-id) player-id :process-auras))))}
   "Blood Imp effect"
   {:name        "Blood Imp effect"
    :description "At the end of your turn give another random friendly minion +1 Health."
    :end-of-turn (fn [state id player-id enemy-id target-id]
                   (let [friendly-minions (filter #(not= (:id %) id) (get-minions state player-id))]
                     (if (seq friendly-minions)
                       (let [random-minion (rand-nth friendly-minions)]
                         (update-minion state (:id random-minion) :health inc))
                       state)))}

   ;; Battlecry effects
   "Silver Hand Knight effect"
   {:name        "Silver Hand Knight effect"
    :description "Battlecry: Summon a 2/2 Squire."
    :battlecry   (fn [state id player-id enemy-id target-id]
                   (add-minion-to-board state player-id (create-minion "Squire") 0))}

   "Abusive Sergeant effect"
   {:name        "Abusive Sergeant effect"
    :description "Battlecry: Give a minion +2 Attack this turn."
    :battlecry   (fn [state source-id player-id enemy-id target-id]
                   (println "Abusive Sergeant battlecry called with:")
                   (println "  source-id:" source-id)
                   (println "  player-id:" player-id)
                   (println "  enemy-id:" enemy-id)
                   (println "  target-id:" target-id)

                   (cond
                     ;; If target is nil, return unchanged state
                     (nil? target-id)
                     (do
                       (println "No target specified for Abusive Sergeant")
                       state)

                     ;; If target is a number (position), find minion at that position
                     (number? target-id)
                     (let [position target-id
                           enemy-minions (get-minions state enemy-id)
                           friendly-minions (get-minions state player-id)
                           all-minions (concat friendly-minions enemy-minions)
                           target-minion (first (filter #(= (:position %) position) all-minions))]
                       (if target-minion
                         (do
                           (println "Found target by position:" (:name target-minion))
                           (-> state
                               (update-minion (:id target-minion) :attack #(+ (or % 1) 2))
                               (update-minion (:id target-minion) :effects
                                              (fn [effects] (conj (or effects []) "Abusive Sergeant reduction")))))
                         (do
                           (println "No minion found at position" position)
                           state)))

                     ;; Otherwise try to find minion by ID
                     :else
                     (let [target-minion (get-minion state target-id)]
                       (if target-minion
                         (do
                           (println "Found target by ID:" (:name target-minion))
                           (-> state
                               (update-minion target-id :attack #(+ (or % 1) 2))
                               (update-minion target-id :effects
                                              (fn [effects] (conj (or effects []) "Abusive Sergeant reduction")))))
                         (do
                           (println "No minion found with ID" target-id)
                           state)))))}

   "Abusive Sergeant reduction"
   {:name      "Abusive Sergeant reduction"
    :this-turn (fn [state id player-id enemy-id target-id]
                 (-> state
                     (update-minion id :attack #(- % 2))
                     (update-minion id :effects #(filter (fn [e] (not= e "Abusive Sergeant reduction")) %))))}

   "Stampeding Kodo effect"
   {:name        "Stampeding Kodo effect"
    :description "Battlecry: Destroy a random enemy minion with 2 or less Attack."
    :battlecry   (fn [state id player-id enemy-id target-id]
                   (let [eligible-minions (filter #(<= (:attack %) 2) (get-minions state enemy-id))]
                     (if (seq eligible-minions)
                       (let [target (rand-nth eligible-minions)]
                         (remove-minion state (:id target)))
                       state)))}

   "Mad Bomber effect"
   {:name        "Mad Bomber effect"
    :description "Battlecry: Deal 3 damage randomly split between all other characters."
    :battlecry   (fn [state id player-id enemy-id target-id]
                   (let [all-characters (filter #(not= (:id %) id) (get-all-characters state))]
                     (loop [s state
                            remaining 3]
                       (if (zero? remaining)
                         s
                         (let [target (rand-nth all-characters)]
                           (recur
                             (if (= (:entity-type target) :hero)
                               (update-hero s (:id target) :damage-taken inc)
                               (update-minion s (:id target) :damage-taken inc))
                             (dec remaining)))))))}

   "Twilight Drake effect"
   {:name        "Twilight Drake effect"
    :description "Battlecry: Gain +1 Health for each card in your hand."
    :battlecry   (fn [state id player-id enemy-id target-id]
                   (let [hand-size (count (get-hand state player-id))]
                     (update-minion state id :health #(+ % hand-size))))}

   "Dr. Boom effect"
   {:name        "Dr. Boom effect"
    :description "Battlecry: Summon two 1/1 Boom Bots."
    :battlecry   (fn [state id player-id enemy-id target-id]
                   (-> state
                       (add-minion-to-board player-id (create-minion "Boom Bot") 0)
                       (add-minion-to-board player-id (create-minion "Boom Bot") 0)))}

   ;; Minion summoned effects
   "Knife Juggler effect"
   {:name          "Knife Juggler effect"
    :description   "After you summon a minion, deal 1 damage to a random enemy."
    :summon-minion (fn [state id player-id enemy-id target-id]
                     (let [all-enemy-characters (cons (get-hero state enemy-id)
                                                      (get-minions state enemy-id))
                           target (rand-nth all-enemy-characters)]
                       (if (= (:entity-type target) :hero)
                         (update-hero state enemy-id :damage-taken inc)
                         (update-minion state (:id target) :damage-taken inc))))}

   "Questing Adventurer effect"
   {:name      "Questing Adventurer effect"
    :description "Whenever you play a card, gain +1/+1."
    :play-card (fn [state id player-id enemy-id target-id]
                 (-> state
                     (update-minion id :attack inc)
                     (update-minion id :health inc)))}

   "Dire Wolf Alpha effect"
   {:name        "Dire Wolf Alpha effect"
    :description "Adjacent minions have +1 Attack."
    :process-auras (fn [state id player-id enemy-id target-id]
                     ;; This function is called to recalculate aura effects
                     ;; after minion positions change
                     (println "Processing Dire Wolf Alpha aura effect")
                     ;; Return the state unchanged - the actual attack bonus
                     ;; is calculated dynamically in get-attack
                     state)}
   ;; Spell effects
   "Consecration spell effect"
   {:name         "Consecration spell effect"
    :description  "Deal 2 damage to all enemies."
    :spell-effect (fn [state id player-id enemy-id target-id]
                    (println "Consecration effect executing:")
                    (println "  Player ID:" player-id)
                    (println "  Enemy ID:" enemy-id)

                    (let [enemy-minions (get-minions state enemy-id)]
                      (println "  Enemy minions:" (map #(select-keys % [:id :name :health :damage-taken]) enemy-minions))

                      ;; Update hero damage
                      (println "  Updating enemy hero damage")
                      (let [state-after-hero (update-hero state enemy-id :damage-taken #(+ % 2))

                            ;; Update minion damage
                            state-after-minions (reduce
                                                  (fn [state minion]
                                                    (println "    Updating minion damage for" (:name minion) "with ID" (:id minion))
                                                    (update-minion state (:id minion) :damage-taken #(+ % 2)))
                                                  state-after-hero
                                                  enemy-minions)

                            ;; Check for and remove dead minions
                            final-state (reduce
                                          (fn [state minion-id]
                                            (let [minion (get-minion state minion-id)]
                                              (if (and minion (>= (:damage-taken minion) (:health minion)))
                                                (do
                                                  (println "Removing dead minion:" (:name minion))
                                                  (remove-minion state minion-id))
                                                state)))
                                          state-after-minions
                                          (map :id enemy-minions))]

                        ;; Final state check
                        (println "  After cleanup, enemy minions:"
                                 (map #(select-keys % [:id :name :health :damage-taken])
                                      (get-minions final-state enemy-id)))
                        final-state)))}

   "Kill Command spell effect"
   {:name         "Kill Command spell effect"
    :description  "Deal 3 damage. If you control a Beast deal 5 damage instead."
    :spell-effect (fn [state id player-id enemy-id target-id]
                    (println "Kill Command effect executing:")
                    (println "  Source ID:" id)
                    (println "  Player ID:" player-id)
                    (println "  Enemy ID:" enemy-id)
                    (println "  Target ID:" target-id)

                    (if (nil? target-id)
                      (do
                        (println "Error: No target specified for Kill Command")
                        state)
                      (let [has-beast (some #(= (:race %) :beast) (get-minions state player-id))
                            damage (if has-beast 5 3)
                            target (get-minion state target-id)]

                        (println "  Has beast minion:" has-beast)
                        (println "  Damage to deal:" damage)
                        (println "  Target found:" (if target "Yes" "No"))
                        (if target
                          (println "  Target details:" (select-keys target [:id :name :health :damage-taken]))
                          (println "  Warning: Target minion not found with ID" target-id))

                        (cond
                          ;; No target found
                          (nil? target)
                          (do
                            (println "  Kill Command failed: target not found")
                            state)

                          ;; Target is a minion
                          (= (:entity-type target) :minion)
                          (let [updated-state (update-minion state target-id :damage-taken #(+ % damage))
                                updated-target (get-minion updated-state target-id)
                                final-state (if (and updated-target (>= (:damage-taken updated-target) (:health updated-target)))
                                              (do
                                                (println "  Removing dead minion:" (:name updated-target))
                                                (remove-minion updated-state target-id))
                                              updated-state)]
                            (println "  Kill Command successful on minion")
                            (println "  Updated target:" (select-keys (get-minion updated-state target-id)
                                                                      [:id :name :health :damage-taken]))
                            final-state)

                          ;; Target is a hero
                          (= (:entity-type target) :hero)
                          (do
                            (println "  Kill Command dealing damage to hero")
                            (update-hero state (:owner-id target) :damage-taken #(+ % damage)))

                          :else
                          (do
                            (println "  Kill Command failed: unknown entity type")
                            state)))))}

   "Equality spell effect"
   {:name         "Equality spell effect"
    :description  "Change the Health of ALL minions to 1."
    :spell-effect (fn [state id player-id enemy-id target-id]
                    (let [all-minions (get-minions state)]
                      (reduce (fn [s minion]
                                (let [max-health (:health minion)
                                      current-health (- max-health (:damage-taken minion))
                                      new-damage (max 0 (- max-health 1))]
                                  (update-minion s (:id minion) :damage-taken new-damage)))
                              state
                              all-minions)))}

   "Feign Death spell effect"
   {:name         "Feign Death spell effect"
    :description  "Trigger all Deathrattles on your minions."
    :spell-effect (fn [state id player-id enemy-id target-id]
                    (let [player-minions (get-minions state player-id)]
                      (reduce (fn [s minion]
                                (effects-parser s [minion] player-id :deathrattle))
                              state
                              player-minions)))}

   "Explosive Trap spell effect"
   {:name          "Explosive Trap spell effect"
    :description   "Secret: When your hero is attacked deal 2 damage to all enemies."
    :spell-effect  (fn [state id player-id enemy-id target-id]
                     (println "Adding Explosive Trap secret to player" player-id)
                     (add-card-to-secrets state player-id (create-card "Explosive Trap")))
    :hero-attacked (fn [state id player-id enemy-id args]
                     (println "EXECUTING EXPLOSIVE TRAP SECRET!")
                     (println "  Secret ID:" id)
                     (println "  Player ID:" player-id)
                     (println "  Enemy ID:" enemy-id)

                     ;; Get attacker ID from args
                     (let [attacker-id (:attacker-id args)
                           enemy-minions (get-minions state enemy-id)]

                       (println "  Found" (count enemy-minions) "enemy minions")
                       (println "  Removing secret and applying damage")

                       (-> state
                           ;; Remove this secret
                           (update-in [:players player-id :secrets]
                                      (fn [secrets]
                                        (filter #(not= (:id %) id) secrets)))
                           ;; Damage the enemy hero
                           (update-hero enemy-id :damage-taken #(+ % 2))
                           ;; Damage all enemy minions
                           (as-> s
                                 (reduce (fn [state minion]
                                           (update-minion state (:id minion) :damage-taken #(+ % 2)))
                                         s
                                         enemy-minions)))))}

   "Cat Trick spell effect"
   {:name                "Cat Trick spell effect"
    :description         "Secret: After your opponent casts a spell, summon a 4/2 Panther with Stealth."
    :spell-effect        (fn [state id player-id enemy-id target-id]
                           (add-card-to-secrets state player-id (create-card "Cat Trick")))
    :opponent-play-spell (fn [state id player-id enemy-id target-id]
                           (-> state
                               (update-in [:players player-id :secrets]
                                          (fn [secrets]
                                            (filter #(not= (:name %) "Cat Trick") secrets)))
                               (add-minion-to-board player-id (create-minion "Cat in a Hat") 0)))}

  "Alexstrasza effect"
  {:name        "Alexstrasza effect"
   :description "Battlecry: Set a hero's remaining Health to 15."
   :battlecry   (fn [state id player-id enemy-id target-id]
                  (if (nil? target-id)
                    state
                    (let [target (get-hero state target-id)]
                      (if target
                        (update-hero state target-id :damage-taken
                                     (fn [_] (- (:health target) 15)))
                        state))))}

  "Master of Disguise effect"
  {:name        "Master of Disguise effect"
   :description "Battlecry: Give a friendly minion Stealth until your next turn."
   :battlecry   (fn [state id player-id enemy-id target-id]
                  (if (nil? target-id)
                    state
                    (let [target (get-minion state target-id)]
                      (if (and target (= (:owner-id target) player-id))
                        (-> state
                            (update-minion target-id :states
                                           (fn [states]
                                             (if (some #(= % :stealth) states)
                                               states
                                               (conj (or states []) :stealth))))
                            (update-minion target-id :effects
                                           (fn [effects] (conj (or effects []) "Master of Disguise reduction"))))
                        state))))}

  "Master of Disguise reduction"
  {:name      "Master of Disguise reduction"
   :next-turn (fn [state id player-id enemy-id target-id]
                (-> state
                    (update-minion id :states
                                   (fn [states] (filter #(not= % :stealth) states)))
                    (update-minion id :effects
                                   (fn [effects] (filter #(not= % "Master of Disguise reduction") effects)))))}

  "Infest spell effect"
  {:name         "Infest spell effect"
   :description  "Give your minions \"Deathrattle: Add a random Beast to your hand.\""
   :spell-effect (fn [state id player-id enemy-id target-id]
                   (let [player-minions (get-minions state player-id)]
                     (reduce (fn [s minion]
                               (update-minion s (:id minion) :effects
                                              (fn [effects] (conj (or effects []) "Infest effect"))))
                             state
                             player-minions)))}

  "Infest effect"
  {:name        "Infest effect"
   :description "Deathrattle: Add a random Beast to your hand."
   :deathrattle (fn [state id player-id enemy-id target-id]
                  (let [beast-cards (filter #(= (:race %) :beast)
                                            (map #(get-definition %)
                                                 (keys (get-definitions))))]
                    (if (seq beast-cards)
                      (let [random-beast (rand-nth beast-cards)]
                        (add-card-to-hand state player-id (:name random-beast)))
                      state)))}

  "Ragnaros the Firelord effect"
  {:name        "Ragnaros the Firelord effect"
   :description "Can't attack. At the end of your turn, deal 8 damage to a random enemy."
   :end-of-turn (fn [state id player-id enemy-id target-id]
                  (let [enemy-characters (cons (get-hero state enemy-id)
                                               (get-minions state enemy-id))]
                    (if (seq enemy-characters)
                      (let [target (rand-nth enemy-characters)]
                        (if (= (:entity-type target) :hero)
                          (update-hero state enemy-id :damage-taken #(+ % 8))
                          (update-minion state (:id target) :damage-taken #(+ % 8))))
                      state)))}

  "Shadow Sensei effect"
  {:name        "Shadow Sensei effect"
   :description "Battlecry: Give a Stealthed minion +2/+2."
   :battlecry   (fn [state id player-id enemy-id target-id]
                  (if (nil? target-id)
                    state
                    (let [target (get-minion state target-id)]
                      (if (and target
                               (some #(= % :stealth) (:states target)))
                        (-> state
                            (update-minion target-id :attack #(+ % 2))
                            (update-minion target-id :health #(+ % 2)))
                        state))))}

  "Frothing Berserker effect"
  {:name          "Frothing Berserker effect"
   :description   "Whenever a minion takes damage, gain +1 Attack."
   :damage-minion (fn [state id player-id enemy-id target-id]
                    (update-minion state id :attack inc))}

  "Baron Geddon effect"
  {:name        "Baron Geddon effect"
   :description "At the end of your turn, deal 2 damage to ALL other characters."
   :end-of-turn (fn [state id player-id enemy-id target-id]
                  (let [all-characters (filter #(not= (:id %) id) (get-all-characters state))]
                    (reduce (fn [s character]
                              (if (= (:entity-type character) :hero)
                                (update-hero s (:id character) :damage-taken #(+ % 2))
                                (update-minion s (:id character) :damage-taken #(+ % 2))))
                            state
                            all-characters)))}

  "Misdirection spell effect"
  {:name          "Misdirection spell effect"
   :description   "Secret: When a character attacks your hero instead he attacks another random character."
   :spell-effect  (fn [state id player-id enemy-id target-id]
                    (add-card-to-secrets state player-id (create-card "Misdirection")))
   :hero-attacked (fn [state id player-id enemy-id args]
                    (let [attacker-id (:attacker-id args)
                          attacker (get-minion state attacker-id)
                          all-characters (filter #(not= (:id %) attacker-id)
                                                 (filter #(not= (:id %) id)
                                                         (get-all-characters state)))
                          new-target (when (seq all-characters) (rand-nth all-characters))]
                      (if (and attacker new-target)
                        (-> state
                            (update-in [:players player-id :secrets]
                                       (fn [secrets]
                                         (filter #(not= (:id %) id) secrets)))
                            ;; Here we'd handle redirecting the attack, but that requires
                            ;; deeper changes to the attack mechanics
                            )
                        state)))}

  "Raid Leader effect"
  {:name        "Raid Leader effect"
   :description "Your other minions have +1 Attack."
   :aura        (fn [state id player-id enemy-id target-id]
                  ;; This is similar to Dire Wolf Alpha but affects all friendly minions instead of just adjacent ones
                  (let [raid-leader (get-minion state id)
                        owner-id (:owner-id raid-leader)
                        friendly-minions (filter #(not= (:id %) id) (get-minions state owner-id))]
                    (reduce (fn [s minion]
                              (update-minion s (:id minion) :attack inc))
                            state
                            friendly-minions)))}

  "Shudderwock effect"
  {:name        "Shudderwock effect"
   :description "Battlecry: Repeat all other Battlecries from cards you played this game (targets chosen randomly)."
   :battlecry   (fn [state id player-id enemy-id target-id]
                  ;; This would require tracking all battlecries played,
                  ;; which isn't implemented in the current game state
                  ;; For a simplified version:
                  (let [minions-with-battlecries (filter #(contains? (get-definition %) :battlecry)
                                                         (keys (get-definitions)))]
                    ;; For demonstration, just trigger one random battlecry
                    (if (seq minions-with-battlecries)
                      (let [random-battlecry (rand-nth minions-with-battlecries)
                            battlecry-fn (get-in (get-definition random-battlecry) [:battlecry])]
                        (when battlecry-fn
                          (battlecry-fn state id player-id enemy-id nil)))
                      state)))}

  "Unearthed Raptor effect"
  {:name        "Unearthed Raptor effect"
   :description "Battlecry: Choose a friendly minion. Gain a copy of its Deathrattle effect."
   :battlecry   (fn [state id player-id enemy-id target-id]
                  (if (nil? target-id)
                    state
                    (let [target (get-minion state target-id)
                          target-effects (:effects target)]
                      (if (and target (= (:owner-id target) player-id))
                        ;; Find deathrattle effects and copy them
                        (let [deathrattle-effects (filter #(contains? (get-definition %) :deathrattle) target-effects)]
                          (if (seq deathrattle-effects)
                            (update-minion state id :effects
                                           (fn [effects] (concat (or effects []) deathrattle-effects)))
                            state))
                        state))))}
  })

;; Add all the definitions to the game
(add-definitions! card-definitions)
(add-definitions! effect-definitions)