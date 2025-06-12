(ns firestone.definition.card
  (:require [firestone.definitions :refer [add-definitions!
                                           get-definition
                                           get-definitions]]
            [firestone.core :refer [draw-card
                                    get-attack]]
            [firestone.construct :refer [create-minion
                                         get-minions
                                         get-heroes
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
                                         get-player-id-by-hero-id
                                         update-hero
                                         deal-damage-and-check-death]]))

;; Card definitions with embedded effect functions
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
    :description "Deathrattle: Deal 2 damage to the enemy hero."
    :deathrattle (fn [{:keys [state enemy-id]}]
                   (update-hero state enemy-id :damage-taken #(+ % 2)))}

   "Loot Hoarder"
   {:name        "Loot Hoarder"
    :attack      2
    :health      1
    :mana-cost   2
    :type        :minion
    :set         :classic
    :rarity      :common
    :effect      "Loot Hoarder effect"
    :description "Deathrattle: Draw a card."
    :deathrattle (fn [{:keys [state player-id]}]
                   (draw-card state player-id))}

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
    :type        :minion
    :end-of-turn (fn [{:keys [state id player-id]}]
                   (let [source-minion (get-minion state id)
                         source-position (:position source-minion)
                         player-minions (get-minions state player-id)
                         dire_wolves (filter #(= (:aura %) 1) player-minions)
                         best-position (if (seq dire_wolves)
                                         (let [wolf (first dire_wolves)
                                               wolf-position (:position wolf)]
                                           (if (> wolf-position source-position)
                                             (max 0 (min (inc source-position) wolf-position))
                                             (max 0 (min source-position (inc wolf-position)))))
                                         (inc source-position))]
                     (-> state
                         (add-minion-to-board player-id (create-minion "Steward") best-position)
                         (effects-parser (get-minions state player-id) player-id :process-auras))))}
   "Consecration"
   {:name         "Consecration"
    :mana-cost    4
    :description  "Deal 2 damage to all enemies."
    :class        :paladin
    :set          :basic
    :type         :spell
    :spell-effect (fn [{:keys [state player-id enemy-id]}]
                    (let [enemy-minions (get-minions state enemy-id)
                          enemy-hero (get-hero state enemy-id)
                          all-enemy-targets (cons enemy-hero enemy-minions)]
                      (reduce (fn [state target]
                                (deal-damage-and-check-death state (:id target) 2))
                              state
                              all-enemy-targets)))}

   "Kill Command"
   {:name         "Kill Command"
    :mana-cost    3
    :class        :hunter
    :set          :basic
    :type         :spell
    :description  "Deal 3 damage. If you control a Beast deal 5 damage instead."
    :spell-effect (fn [{:keys [state player-id target-id]}]
                    (when target-id
                      (let [has-beast (some #(= (:race %) :beast) (get-minions state player-id))
                            damage (if has-beast 5 3)]
                        (deal-damage-and-check-death state target-id damage))))}

   "Silver Hand Knight"
   {:name        "Silver Hand Knight"
    :attack      4
    :health      4
    :mana-cost   5
    :effect      "Silver Hand Knight effect"
    :description "Battlecry: Summon a 2/2 Squire."
    :rarity      :common
    :set         :classic
    :type        :minion
    :battlecry   (fn [{:keys [state player-id]}]
                   (add-minion-to-board state player-id (create-minion "Squire") 0))}

   "Abusive Sergeant"
   {:name        "Abusive Sergeant"
    :attack      1
    :health      1
    :mana-cost   1
    :effect      "Abusive Sergeant effect"
    :rarity      :common
    :set         :classic
    :type        :minion
    :description "Battlecry: Give a minion +2 Attack this turn."
    :battlecry (fn [{:keys [state id player-id target-id]}]
                 (when target-id
                   (let [target (get-minion state target-id)]
                     (when target
                       (let [state (update-minion state target-id :overrides
                                                  (fn [overrides]
                                                    (update overrides :attack-bonus
                                                            #(+ (or % 0) 2))))
                             state (update-minion state target-id :effects
                                                  (fn [effects]
                                                    (conj (or effects [])
                                                          {:this-turn
                                                           (fn [{:keys [state id]}]
                                                             (update-minion state id :overrides
                                                                            (fn [overrides]
                                                                              (let [current-bonus (get overrides :attack-bonus 0)
                                                                                    new-bonus (- current-bonus 2)]
                                                                                (if (<= new-bonus 0)
                                                                                  (dissoc overrides :attack-bonus)
                                                                                  (assoc overrides :attack-bonus new-bonus))))))})))]
                         state)))))}

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
    :rarity      :rare
    :battlecry   (fn [{:keys [state enemy-id]}]
                   (let [enemy-minions (get-minions state enemy-id)
                         eligible-minions (filter #(<= (get-attack state (:id %)) 2) enemy-minions)]
                     (when (seq eligible-minions)
                       (let [target (rand-nth eligible-minions)]
                         (remove-minion state (:id target))))))}

   "Mad Bomber"
   {:name        "Mad Bomber"
    :attack      3
    :health      2
    :mana-cost   2
    :effect      "Mad Bomber effect"
    :description "Battlecry: Deal 3 damage randomly split between all other characters."
    :rarity      :common
    :set         :classic
    :type        :minion
    :battlecry   (fn [{:keys [state id]}]
                   (let [all-characters (filter #(not= (:id %) id) (get-all-characters state))]
                     (loop [state state
                            remaining 3]
                       (if (zero? remaining)
                         state
                         (let [current-characters (filter #(not= (:id %) id) (get-all-characters state))
                               target (when (seq current-characters) (rand-nth current-characters))]
                           (if target
                             (recur (deal-damage-and-check-death state (:id target) 1)
                                    (dec remaining))
                             state))))))}

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
    :rarity      :rare
    :battlecry   (fn [{:keys [state id player-id]}]
                   (let [hand-size (count (get-hand state player-id))]
                     (update-minion state id :overrides
                                    #(assoc % :health-bonus (+ (get % :health-bonus 0) hand-size)))))}

   "Dr. Boom"
   {:name        "Dr. Boom"
    :attack      7
    :health      7
    :mana-cost   7
    :effect      "Dr. Boom effect"
    :description "Battlecry: Summon two 1/1 Boom Bots. WARNING: Bots may explode."
    :rarity      :legendary
    :set         :goblins-vs-gnomes
    :type        :minion
    :battlecry   (fn [{:keys [state player-id]}]
                   (-> state
                       (add-minion-to-board player-id (create-minion "Boom Bot") 0)
                       (add-minion-to-board player-id (create-minion "Boom Bot") 0)))}

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
    :rarity      :common
    :end-of-turn (fn [{:keys [state id player-id]}]
                   (let [friendly-minions (filter #(not= (:id %) id) (get-minions state player-id))]
                     (when (seq friendly-minions)
                       (let [random-minion (rand-nth friendly-minions)]
                         (update-minion state (:id random-minion) :overrides
                                        #(assoc % :health-bonus (+ (get % :health-bonus 0) 1)))))))}

   "Equality"
   {:name         "Equality"
    :mana-cost    2
    :description  "Change the Health of ALL minions to 1."
    :class        :paladin
    :set          :classic
    :type         :spell
    :spell-effect (fn [{:keys [state]}]
                    (let [all-minions (get-minions state)]
                      (reduce (fn [state minion]
                                (let [definition (get-definition (:name minion))
                                      max-health (or (:health definition) 1)
                                      new-damage (max 0 (- max-health 1))]
                                  (update-minion state (:id minion) :damage-taken new-damage)))
                              state
                              all-minions)))}

   "Feign Death"
   {:name         "Feign Death"
    :mana-cost    2
    :description  "Trigger all Deathrattles on your minions."
    :class        :hunter
    :rarity       :epic
    :set          :goblins-vs-gnomes
    :type         :spell
    :spell-effect (fn [{:keys [state player-id]}]
                    (let [player-minions (get-minions state player-id)]
                      (reduce (fn [state minion]
                                (effects-parser state [minion] player-id :deathrattle))
                              state
                              player-minions)))}

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
    :description "Deathrattle: Deal 1-4 damage to a random enemy."
    :deathrattle (fn [{:keys [state enemy-id]}]
                   (let [damage (inc (rand-int 4))
                         enemy-minions (get-minions state enemy-id)
                         enemy-hero (get-hero state enemy-id)
                         targets (cons enemy-hero enemy-minions)]
                     (when (seq targets)
                       (let [target (rand-nth targets)]
                         (deal-damage-and-check-death state (:id target) damage)))))}

   "Steward"
   {:name      "Steward"
    :attack    1
    :health    1
    :mana-cost 1
    :type      :minion
    :set       :one-night-in-karazhan}

   "Knife Juggler"
   {:name          "Knife Juggler"
    :attack        3
    :health        2
    :mana-cost     2
    :effect        "Knife Juggler effect"
    :rarity        :rare
    :set           :classic
    :type          :minion
    :description   "After you summon a minion, deal 1 damage to a random enemy."
    :summon-minion (fn [{:keys [state enemy-id]}]
                     (let [all-enemy-characters (cons (get-hero state enemy-id)
                                                      (get-minions state enemy-id))]
                       (when (seq all-enemy-characters)
                         (let [target (rand-nth all-enemy-characters)]
                           (deal-damage-and-check-death state (:id target) 1)))))}

   "Questing Adventurer"
   {:name        "Questing Adventurer"
    :attack      2
    :health      2
    :mana-cost   3
    :effect      "Questing Adventurer effect"
    :rarity      :rare
    :set         :classic
    :type        :minion
    :description "Whenever you play a card, gain +1/+1."
    :play-card   (fn [{:keys [state id]}]
                   (-> state
                       (update-minion id :overrides
                                      #(-> %
                                           (assoc :attack-bonus (+ (get % :attack-bonus 0) 1))
                                           (assoc :health-bonus (+ (get % :health-bonus 0) 1))))))}

   "Dire Wolf Alpha"
   {:name          "Dire Wolf Alpha"
    :attack        2
    :health        2
    :mana-cost     2
    :effect        "Dire Wolf Alpha effect"
    :set           :classic
    :race          :beast
    :type          :minion
    :rarity        :common
    :aura          1
    :description   "Adjacent minions have +1 Attack."
    :process-auras (fn [{:keys [state]}]
                     state)}

   "Explosive Trap"
   {:name           "Explosive Trap"
    :mana-cost      2
    :class          :hunter
    :rarity         :common
    :set            :classic
    :type           :spell
    :description    "Secret: When your hero is attacked deal 2 damage to all enemies."
    :spell-effect   (fn [{:keys [state player-id]}]
                      (add-card-to-secrets state player-id (create-card "Explosive Trap")))
    :hero-attacked  (fn [{:keys [state id player-id enemy-id]}]
                      (let [enemy-minions (get-minions state enemy-id)
                            state (update-in state [:players player-id :secrets]
                                             (fn [secrets]
                                               (filter #(not= (:id %) id) secrets)))
                            state (update-hero state enemy-id :damage-taken #(+ % 2))
                            state (reduce (fn [state minion]
                                            (deal-damage-and-check-death state (:id minion) 2))
                                          state
                                          enemy-minions)]
                        state))}

   "Cat Trick"
   {:name                "Cat Trick"
    :mana-cost           2
    :class               :hunter
    :rarity              :rare
    :set                 :one-night-in-karazhan
    :type                :spell
    :description         "Secret: After your opponent casts a spell, summon a 4/2 Panther with Stealth."
    :spell-effect        (fn [{:keys [state player-id]}]
                           (add-card-to-secrets state player-id (create-card "Cat Trick")))
    :opponent-play-spell (fn [{:keys [state id player-id]}]
                           (-> state
                               (update-in [:players player-id :secrets]
                                          (fn [secrets]
                                            (filter #(not= (:id %) id) secrets)))
                               (add-minion-to-board player-id (create-minion "Cat in a Hat") 0)))}

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
    :description "Battlecry: Set a hero's remaining Health to 15."
    :battlecry   (fn [{:keys [state target-id]}]
                   (when target-id
                     (let [player-id (get-player-id-by-hero-id state target-id)]
                       (when player-id
                         ;; Set hero health to 15 (30 max health - 15 damage = 15 health)
                         (update-hero state player-id :damage-taken (constantly 15))))))}
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
    :description "Battlecry: Give a friendly minion Stealth until your next turn."
    :battlecry   (fn [{:keys [state target-id player-id]}]
                   (when target-id
                     (let [target (get-minion state target-id)]
                       (when (and target (= (:owner-id target) player-id))
                         (let [state (update-minion state target-id :states
                                                    (fn [states]
                                                      (if (some #(= % :stealth) states)
                                                        states
                                                        (conj (or states []) :stealth))))
                               state (update-minion state target-id :effects
                                                    (fn [effects]
                                                      (conj (or effects [])
                                                            {:next-turn
                                                             (fn [{:keys [state id]}]
                                                               (update-minion state id :states
                                                                              (fn [states]
                                                                                (filter #(not= % :stealth) states))))})))]
                           state)))))}

   "Infest"
   {:name         "Infest"
    :mana-cost    3
    :class        :hunter
    :rarity       :rare
    :set          :whispers-of-the-old-gods
    :type         :spell
    :description  "Give your minions \"Deathrattle: Add a random Beast to your hand.\""
    :spell-effect (fn [{:keys [state player-id]}]
                    (let [player-minions (get-minions state player-id)

                          infest-deathrattle {:deathrattle
                                              (fn [{:keys [state player-id]}]
                                                (let [all-definitions (get-definitions)
                                                      beast-cards (filter #(= (:race %) :beast) all-definitions)]
                                                  (when (seq beast-cards)
                                                    (let [random-beast (rand-nth beast-cards)]
                                                      (add-card-to-hand state player-id (:name random-beast))))))}]


                      (reduce (fn [state minion]
                                (update-minion state (:id minion) :effects
                                               (fn [effects]
                                                 (conj (or effects []) infest-deathrattle))))
                              state
                              player-minions)))}

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
    :description "Can't attack. At the end of your turn, deal 8 damage to a random enemy."
    :end-of-turn (fn [{:keys [state enemy-id]}]
                   (let [enemy-characters (cons (get-hero state enemy-id)
                                                (get-minions state enemy-id))]
                     (when (seq enemy-characters)
                       (let [target (rand-nth enemy-characters)]
                         (deal-damage-and-check-death state (:id target) 8)))))}

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
    :description "Battlecry: Give a Stealthed minion +2/+2."
    :battlecry   (fn [{:keys [state target-id]}]
                   (when target-id
                     (let [target (get-minion state target-id)]
                       (when (and target
                                  (some #(= % :stealth) (:states target)))
                         (update-minion state target-id :overrides
                                        #(-> %
                                             (assoc :attack-bonus (+ (get % :attack-bonus 0) 2))
                                             (assoc :health-bonus (+ (get % :health-bonus 0) 2))))))))}

   "Frothing Berserker"
   {:name          "Frothing Berserker"
    :mana-cost     3
    :health        4
    :attack        2
    :effect        "Frothing Berserker effect"
    :type          :minion
    :class         :warrior
    :set           :classic
    :rarity        :rare
    :description   "Whenever a minion takes damage, gain +1 Attack."
    :damage-minion (fn [{:keys [state id]}]
                     (update-minion state id :overrides
                                    #(assoc % :attack-bonus (+ (get % :attack-bonus 0) 1))))}

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
    :description "At the end of your turn, deal 2 damage to ALL other characters."
    :end-of-turn (fn [{:keys [state id]}]
                   (let [all-heroes (get-heroes state)
                         all-minions (filter #(not= (:id %) id) (get-minions state))
                         all-targets (concat all-heroes all-minions)]
                     (reduce (fn [state target]
                               (deal-damage-and-check-death state (:id target) 2))
                             state
                             all-targets)))}

   "Misdirection"
   {:name          "Misdirection"
    :mana-cost     2
    :class         :hunter
    :rarity        :rare
    :set           :classic
    :type          :spell
    :description   "Secret: When a character attacks your hero instead he attacks another random character."
    :spell-effect  (fn [{:keys [state player-id]}]
                     (add-card-to-secrets state player-id (create-card "Misdirection")))
    :hero-attacked (fn [{:keys [state id player-id args]}]
                     (let [attacker-id (:attacker-id args)
                           attacker (get-minion state attacker-id)
                           all-characters (filter #(not= (:id %) attacker-id)
                                                  (filter #(not= (:id %) id)
                                                          (get-all-characters state)))
                           new-target (when (seq all-characters) (rand-nth all-characters))]
                       (when (and attacker new-target)
                         (update-in state [:players player-id :secrets]
                                    (fn [secrets]
                                      (filter #(not= (:id %) id) secrets))))))}


   "Raid Leader"
   {:name        "Raid Leader"
    :attack      2
    :health      2
    :mana-cost   3
    :effect      "Raid Leader effect"
    :set         :basic
    :type        :minion
    :description "Your other minions have +1 Attack."
    :aura        (fn [{:keys [state id]}]
                   (let [raid-leader (get-minion state id)
                         owner-id (:owner-id raid-leader)
                         friendly-minions (filter #(not= (:id %) id) (get-minions state owner-id))]
                     (reduce (fn [state minion]
                               (update-minion state (:id minion) :attack inc))
                             state
                             friendly-minions)))}

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
    :description "Battlecry: Repeat all other Battlecries from cards you played this game (targets chosen randomly)."
    :battlecry   (fn [{:keys [state id player-id enemy-id]}]
                   (let [minions-with-battlecries (filter #(contains? (get-definition %) :battlecry)
                                                          (keys (get-definitions)))]
                     (if (seq minions-with-battlecries)
                       (let [random-battlecry (rand-nth minions-with-battlecries)
                             battlecry-fn (get-in (get-definition random-battlecry) [:battlecry])]
                         (when battlecry-fn
                           (battlecry-fn {:state state :id id :player-id player-id :enemy-id enemy-id :target-id nil})))
                       state)))}

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
    :description "Battlecry: Choose a friendly minion. Gain a copy of its Deathrattle effect."
    :battlecry   (fn [{:keys [state id target-id player-id]}]
                   (when target-id
                     (let [target (get-minion state target-id)
                           target-effects (:effects target)]
                       (when (and target (= (:owner-id target) player-id))
                         (let [deathrattle-effects (filter #(contains? (get-definition %) :deathrattle) target-effects)]
                           (when (seq deathrattle-effects)
                             (update-minion state id :effects
                                            (fn [effects] (concat (or effects []) deathrattle-effects)))))))))}
   })

;; Add all the definitions to the game
(add-definitions! card-definitions)