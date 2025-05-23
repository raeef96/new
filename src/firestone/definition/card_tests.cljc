(ns firestone.definition.card-tests
  (:require [clojure.test :refer [deftest is testing]]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-minion
                                         create-hero
                                         get-minion
                                         get-minions
                                         get-health
                                         add-minion-to-board
                                         add-card-to-hand
                                         add-card-to-deck
                                         add-card-to-secrets
                                         remove-minion
                                         update-minion
                                         update-hero
                                         get-hero]]
            [firestone.core :refer [play-card get-attack]]
            [firestone.core-api :refer [end-turn]]))

(deftest test-boulderfist-ogre-stats
  (let [state (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
        ogre (get-minion state "bo")]
    (is (= (:attack ogre) 6))
    (is (= (:health ogre) 7))))

(deftest test-leper-gnome-effect
  (let [state (create-game [{:minions [(create-minion "Leper Gnome" :id "m1")]}
                            {:hero (create-hero "Jaina Proudmoore" :id "h2" :damage-taken 0)}])
        state-after-death (remove-minion state "m1")
        enemy-hero-health (get-health (get-hero state-after-death "p2"))]
    (is (= enemy-hero-health 28))))

(deftest test-loot-hoarder-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Loot Hoarder" :id "lh") 0)
                  (add-card-to-deck "p1" "Sheep"))
        initial-hand-count (count (get-in state [:players "p1" :hand]))
        state-after-death (remove-minion state "lh")
        final-hand-count (count (get-in state-after-death [:players "p1" :hand]))]
    (is (= final-hand-count (inc initial-hand-count)))))

(deftest test-sheep-stats
  (let [state (create-game [{:minions [(create-minion "Sheep" :id "sheep")]}])
        sheep (get-minion state "sheep")]
    (is (= (:attack sheep) 1))
    (is (= (:health sheep) 1))
    (is (= (:race sheep) :beast))))

(deftest test-moroes-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Moroes" :id "moroes") 0))
        initial-minion-count (count (get-minions state "p1"))
        state-after-turn (end-turn state "p1")
        final-minion-count (count (get-minions state-after-turn "p1"))]
    (is (= final-minion-count (inc initial-minion-count)))))

(deftest test-consecration-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Consecration" :id "cons"))
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "sheep1") 0)
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "sheep2") 1))
        state-after-spell (play-card state "p1" "cons" nil nil)
        enemy-minions (get-minions state-after-spell "p2")
        enemy-hero-health (get-health (get-hero state-after-spell "p2"))]
    (is (empty? enemy-minions))
    (is (= enemy-hero-health 28))))

(deftest test-kill-command-effect
  (let [state-without-beast (-> (create-game)
                                (add-card-to-hand "p1" (create-card "Kill Command" :id "kc"))
                                (add-minion-to-board "p2" (create-minion "Sheep" :id "target" :health 4 :damage-taken 0) 0))
        state-after-spell1 (play-card state-without-beast "p1" "kc" "target" nil)
        target1 (get-minion state-after-spell1 "target")

        state-with-beast (-> (create-game)
                             (add-card-to-hand "p1" (create-card "Kill Command" :id "kc"))
                             (add-minion-to-board "p1" (create-minion "Sheep" :id "beast" :race :beast) 0)
                             (add-minion-to-board "p2" (create-minion "Sheep" :id "target" :health 6 :damage-taken 0) 0))
        state-after-spell2 (play-card state-with-beast "p1" "kc" "target" nil)
        target2 (get-minion state-after-spell2 "target")]

    (is (= (:damage-taken target1) 3))
    (is (= (:damage-taken target2) 5))))

(deftest test-silver-hand-knight-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Silver Hand Knight" :id "shk")))
        initial-minion-count (count (get-minions state "p1"))
        state-after-play (play-card state "p1" "shk" nil 0)
        final-minion-count (count (get-minions state-after-play "p1"))]
    (is (= final-minion-count (+ initial-minion-count 2)))))

(deftest test-abusive-sergeant-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Abusive Sergeant" :id "as"))
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep" :attack 1) 0))
        initial-attack (get-attack state "sheep")
        state-after-play (play-card state "p1" "as" "sheep" 0)
        boosted-attack (get-attack state-after-play "sheep")
        state-after-turn (end-turn state-after-play "p1")
        state-after-turn2 (end-turn state-after-turn "p2")
        final-attack (get-attack state-after-turn2 "sheep")]

    (is (= boosted-attack (+ initial-attack 2)))
    (is (= final-attack initial-attack))))

(deftest test-stampeding-kodo-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Stampeding Kodo" :id "kodo"))
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "weak" :attack 1) 0)
                  (add-minion-to-board "p2" (create-minion "Boulderfist Ogre" :id "strong" :attack 6) 1))
        initial-enemy-count (count (get-minions state "p2"))
        state-after-play (play-card state "p1" "kodo" nil 0)
        final-enemy-count (count (get-minions state-after-play "p2"))]

    (is (= final-enemy-count (dec initial-enemy-count)))
    (is (not (nil? (get-minion state-after-play "strong"))))))

(deftest test-mad-bomber-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Mad Bomber" :id "mb"))
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep1" :damage-taken 0) 0)
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "sheep2" :damage-taken 0) 0))
        p1-hero-initial (get-in state [:players "p1" :hero :damage-taken])
        p2-hero-initial (get-in state [:players "p2" :hero :damage-taken])
        sheep1-initial (get-in state [:players "p1" :minions 0 :damage-taken])
        sheep2-initial (get-in state [:players "p2" :minions 0 :damage-taken])

        state-after-play (play-card state "p1" "mb" nil 0)

        p1-hero-final (get-in state-after-play [:players "p1" :hero :damage-taken])
        p2-hero-final (get-in state-after-play [:players "p2" :hero :damage-taken])
        sheep1-final (if-let [s (get-minion state-after-play "sheep1")] (:damage-taken s) 1)
        sheep2-final (if-let [s (get-minion state-after-play "sheep2")] (:damage-taken s) 1)

        total-damage (+ (- p1-hero-final p1-hero-initial)
                        (- p2-hero-final p2-hero-initial)
                        (- sheep1-final sheep1-initial)
                        (- sheep2-final sheep2-initial))]

    (is (= total-damage 3))))

(deftest test-twilight-drake-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Twilight Drake" :id "drake"))
                  (add-card-to-hand "p1" (create-card "Sheep" :id "c1"))
                  (add-card-to-hand "p1" (create-card "Sheep" :id "c2")))
        hand-size (dec (count (get-in state [:players "p1" :hand]))) ; -1 for the drake itself
        state-after-play (play-card state "p1" "drake" nil 0)
        drake (get-minion state-after-play "drake")]

    (is (= (get-health drake) (+ 1 hand-size)))))

(deftest test-dr-boom-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Dr. Boom" :id "boom")))
        initial-minion-count (count (get-minions state "p1"))
        state-after-play (play-card state "p1" "boom" nil 0)
        final-minion-count (count (get-minions state-after-play "p1"))
        boom-bots (filter #(= (:name %) "Boom Bot") (get-minions state-after-play "p1"))]

    (is (= final-minion-count (+ initial-minion-count 3)))
    (is (= (count boom-bots) 2))))

(deftest test-blood-imp-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Blood Imp" :id "imp") 0)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep" :health 1) 1))
        initial-health (get-health (get-minion state "sheep"))
        state-after-turn (end-turn state "p1")
        final-health (get-health (get-minion state-after-turn "sheep"))]

    (is (= final-health (inc initial-health)))))

(deftest test-equality-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Equality" :id "eq"))
                  (add-minion-to-board "p1" (create-minion "Boulderfist Ogre" :id "bo1" :health 7 :damage-taken 0) 0)
                  (add-minion-to-board "p2" (create-minion "Boulderfist Ogre" :id "bo2" :health 7 :damage-taken 0) 0))
        state-after-spell (play-card state "p1" "eq" nil nil)
        minion1-health (get-health (get-minion state-after-spell "bo1"))
        minion2-health (get-health (get-minion state-after-spell "bo2"))]

    (is (= minion1-health 1))
    (is (= minion2-health 1))))

(deftest test-feign-death-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Feign Death" :id "fd"))
                  (add-minion-to-board "p1" (create-minion "Leper Gnome" :id "lg") 0))
        initial-enemy-health (get-health (get-hero state "p2"))
        state-after-spell (play-card state "p1" "fd" nil nil)
        final-enemy-health (get-health (get-hero state-after-spell "p2"))]

    (is (= final-enemy-health (- initial-enemy-health 2)))))


(deftest test-squire-stats
  (let [state (create-game [{:minions [(create-minion "Squire" :id "sq")]}])
        squire (get-minion state "sq")]

    (is (= (:attack squire) 2))
    (is (= (:health squire) 2))))

(deftest test-boom-bot-effect
  (let [state (create-game [{:minions [(create-minion "Boom Bot" :id "bb")]}
                            {:hero (create-hero "Jaina Proudmoore" :id "h2" :damage-taken 0)}])
        initial-enemy-health (get-health (get-hero state "p2"))
        state-after-death (remove-minion state "bb")
        final-enemy-health (get-health (get-hero state-after-death "p2"))]

    (is (<= final-enemy-health (- initial-enemy-health 1)))
    (is (>= final-enemy-health (- initial-enemy-health 4)))))

(deftest test-steward-stats
  (let [state (create-game [{:minions [(create-minion "Steward" :id "st")]}])
        steward (get-minion state "st")]

    (is (= (:attack steward) 1))
    (is (= (:health steward) 1))))

(deftest test-knife-juggler-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Knife Juggler" :id "kj") 0)
                  (add-card-to-hand "p1" (create-card "Sheep" :id "sheep")))
        initial-enemy-health (get-health (get-hero state "p2"))
        state-after-play (play-card state "p1" "sheep" nil 1)
        final-enemy-health (get-health (get-hero state-after-play "p2"))]

    (is (= final-enemy-health (dec initial-enemy-health)))))

(deftest test-questing-adventurer-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Questing Adventurer" :id "qa" :attack 2 :health 2) 0)
                  (add-card-to-hand "p1" (create-card "Sheep" :id "sheep")))
        initial-stats {:attack (get-attack state "qa")
                       :health (get-health (get-minion state "qa"))}
        state-after-play (play-card state "p1" "sheep" nil 1)
        final-stats {:attack (get-attack state-after-play "qa")
                     :health (get-health (get-minion state-after-play "qa"))}]

    (is (= final-stats {:attack (inc (:attack initial-stats))
                        :health (inc (:health initial-stats))}))))

(deftest test-dire-wolf-alpha-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "s1" :attack 1) 0)
                  (add-minion-to-board "p1" (create-minion "Dire Wolf Alpha" :id "dw") 1)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "s2" :attack 1) 2))
        s1-attack (get-attack state "s1")
        s2-attack (get-attack state "s2")
        dw-attack (get-attack state "dw")]

    (is (= s1-attack 2))  ; Adjacent to wolf, gets +1
    (is (= s2-attack 2))  ; Adjacent to wolf, gets +1
    (is (= dw-attack 2)))) ; Wolf has base 2 attack

(deftest test-explosive-trap-effect
  (let [state (-> (create-game)
                  (add-card-to-secrets "p1" (create-card "Explosive Trap" :id "et"))
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "s1") 0)
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "s2") 1))
        attacker (create-minion "Boulderfist Ogre" :id "bo" :owner-id "p2")

        ;; Setup the attack on hero that triggers the trap
        attack-args {:attacker-id (:id attacker)
                     :attacker-player-id "p2"}

        ;; Manually trigger the hero-attacked effect since we're not using actual attack
        state-after-trap (firestone.construct/effects-parser
                           state
                           [(get-hero state "p1")]
                           "p1"
                           :hero-attacked
                           attack-args)

        enemy-minions (get-minions state-after-trap "p2")
        enemy-hero-health (get-health (get-hero state-after-trap "p2"))]

    (is (empty? enemy-minions))
    (is (= enemy-hero-health 28))))

(deftest test-cat-trick-effect
  (let [state (-> (create-game)
                  (add-card-to-secrets "p1" (create-card "Cat Trick" :id "ct"))
                  (add-card-to-hand "p2" (create-card "Equality" :id "eq")))
        initial-minion-count (count (get-minions state "p1"))

        ;; End turn and let p2 play a spell
        state-after-turn (end-turn state "p1")
        state-after-spell (play-card state-after-turn "p2" "eq" nil nil)

        final-minion-count (count (get-minions state-after-spell "p1"))
        summoned-cat (first (filter #(= (:name %) "Cat in a Hat")
                                    (get-minions state-after-spell "p1")))]

    (is (= final-minion-count (inc initial-minion-count)))
    (is (not (nil? summoned-cat)))
    (is (= (:attack summoned-cat) 4))
    (is (= (:health summoned-cat) 2))))

(deftest test-cat-in-a-hat-stats
  (let [state (create-game [{:minions [(create-minion "Cat in a Hat" :id "cat")]}])
        cat (get-minion state "cat")]

    (is (= (:attack cat) 4))
    (is (= (:health cat) 2))
    (is (some #(= % :stealth) (:states cat)))))

(deftest test-alexstrasza-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Alexstrasza" :id "alex"))
                  (update-hero "p2" :damage-taken 0)) ; Ensure full health
        initial-enemy-health (get-health (get-hero state "p2"))
        state-after-play (play-card state "p1" "alex" "h2" 0) ; Target enemy hero
        final-enemy-health (get-health (get-hero state-after-play "p2"))]

    (is (= initial-enemy-health 30))
    (is (= final-enemy-health 15))))

(deftest test-master-of-disguise-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Master of Disguise" :id "mod"))
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep") 0))
        sheep-before (get-minion state "sheep")
        state-after-play (play-card state "p1" "mod" "sheep" 1)
        sheep-after (get-minion state-after-play "sheep")

        state-after-turn1 (end-turn state-after-play "p1")
        state-after-turn2 (end-turn state-after-turn1 "p2")
        sheep-final (get-minion state-after-turn2 "sheep")]

    (is (not (some #(= % :stealth) (:states sheep-before))))
    (is (some #(= % :stealth) (:states sheep-after)))
    (is (not (some #(= % :stealth) (:states sheep-final))))))

(deftest test-infest-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Infest" :id "infest"))
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep") 0))
        initial-hand-count (count (get-in state [:players "p1" :hand]))
        state-after-spell (play-card state "p1" "infest" nil nil)
        state-after-death (remove-minion state-after-spell "sheep")
        final-hand-count (count (get-in state-after-death [:players "p1" :hand]))]

    (is (= final-hand-count initial-hand-count))))

(deftest test-ragnaros-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Ragnaros the Firelord" :id "rag") 0))
        initial-enemy-health (get-health (get-hero state "p2"))
        state-after-turn (end-turn state "p1")
        final-enemy-health (get-health (get-hero state-after-turn "p2"))]

    (is (= final-enemy-health (- initial-enemy-health 8)))))

(deftest test-shadow-sensei-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Shadow Sensei" :id "ss"))
                  (add-minion-to-board "p1" (create-minion "Cat in a Hat" :id "cat" :states [:stealth] :attack 4 :health 2) 0))
        initial-stats {:attack (get-attack state "cat")
                       :health (get-health (get-minion state "cat"))}
        state-after-play (play-card state "p1" "ss" "cat" 1)
        final-stats {:attack (get-attack state-after-play "cat")
                     :health (get-health (get-minion state-after-play "cat"))}]

    (is (= final-stats {:attack (+ (:attack initial-stats) 2)
                        :health (+ (:health initial-stats) 2)}))))

(deftest test-frothing-berserker-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Frothing Berserker" :id "fb" :attack 2) 0)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep1") 1)
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "sheep2") 0))
        initial-attack (get-attack state "fb")

        ;; Damage sheep1
        state-after-damage1 (update-minion state "sheep1" :damage-taken inc)

        ;; Manually trigger damage event
        state-after-event1 (firestone.construct/effects-parser
                             state-after-damage1
                             [(get-minion state-after-damage1 "fb")]
                             "p1"
                             :damage-minion)

        mid-attack (get-attack state-after-event1 "fb")

        ;; Damage sheep2
        state-after-damage2 (update-minion state-after-event1 "sheep2" :damage-taken inc)

        ;; Manually trigger damage event
        state-after-event2 (firestone.construct/effects-parser
                             state-after-damage2
                             [(get-minion state-after-damage2 "fb")]
                             "p1"
                             :damage-minion)

        final-attack (get-attack state-after-event2 "fb")]

    (is (= mid-attack (inc initial-attack)))
    (is (= final-attack (+ initial-attack 2)))))

(deftest test-baron-geddon-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Baron Geddon" :id "bg") 0)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep1") 1)
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "sheep2") 0))
        p1-initial-health (get-health (get-hero state "p1"))
        p2-initial-health (get-health (get-hero state "p2"))
        state-after-turn (end-turn state "p1")
        p1-final-health (get-health (get-hero state-after-turn "p1"))
        p2-final-health (get-health (get-hero state-after-turn "p2"))
        sheep1 (get-minion state-after-turn "sheep1")
        sheep2 (get-minion state-after-turn "sheep2")]

    (is (= p1-final-health (- p1-initial-health 2)))
    (is (= p2-final-health (- p2-initial-health 2)))
    (is (nil? sheep1))  ; Sheep should be dead from 2 damage
    (is (nil? sheep2))))

(deftest test-misdirection-effect
  (let [state (-> (create-game)
                  (add-card-to-secrets "p1" (create-card "Misdirection" :id "md"))
                  (add-minion-to-board "p2" (create-minion "Boulderfist Ogre" :id "bo" :owner-id "p2") 0))

        ;; Setup the attack on hero that triggers the secret
        attack-args {:attacker-id "bo"
                     :attacker-player-id "p2"}

        ;; Manually trigger the hero-attacked effect
        state-after-secret (firestone.construct/effects-parser
                             state
                             [(get-hero state "p1")]
                             "p1"
                             :hero-attacked
                             attack-args)

        ;; Check if secret was removed
        secrets-after (get-in state-after-secret [:players "p1" :secrets])]

    (is (empty? secrets-after))))

(deftest test-raid-leader-effect
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Raid Leader" :id "rl" :attack 2) 0)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep1" :attack 1) 1)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep2" :attack 1) 2))

        ;; Manually trigger aura calculation
        state-with-auras (firestone.construct/effects-parser
                           state
                           [(get-minion state "rl")]
                           "p1"
                           :aura)

        sheep1-attack (get-attack state-with-auras "sheep1")
        sheep2-attack (get-attack state-with-auras "sheep2")
        rl-attack (get-attack state-with-auras "rl")]

    (is (= sheep1-attack 2))  ; Should get +1 from Raid Leader
    (is (= sheep2-attack 2))  ; Should get +1 from Raid Leader
    (is (= rl-attack 2))))   ; Raid Leader's own attack is unchanged

(deftest test-shudderwock-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Shudderwock" :id "sw")))
        state-after-play (play-card state "p1" "sw" nil 0)
        shudderwock (first (filter #(= (:name %) "Shudderwock")
                                   (get-minions state-after-play "p1")))]

    ;; Simply verify that the card was played successfully
    (is (not (nil? shudderwock)))
    (is (= (:attack shudderwock) 6))
    (is (= (:health shudderwock) 6))))

(deftest test-unearthed-raptor-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Unearthed Raptor" :id "ur"))
                  (add-minion-to-board "p1" (create-minion "Leper Gnome" :id "lg" :effects ["Leper Gnome effect"]) 0))
        state-after-play (play-card state "p1" "ur" "lg" 1)
        raptor (get-minion state-after-play "ur")

        ;; Now kill the raptor and see if it triggers the copied deathrattle
        initial-enemy-health (get-health (get-hero state-after-play "p2"))
        state-after-death (remove-minion state-after-play "ur")
        final-enemy-health (get-health (get-hero state-after-death "p2"))]

    (is (some #(= % "Leper Gnome effect") (:effects raptor)))
    (is (= final-enemy-health (- initial-enemy-health 2)))))