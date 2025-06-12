(ns firestone.definition.card-tests
  (:require [clojure.test :refer [deftest is testing]]
            [firestone.construct :refer [create-game
                                         create-card
                                         create-minion
                                         create-hero
                                         get-minion
                                         get-hand
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

(deftest test-twilight-drake-debug
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Twilight Drake" :id "drake"))
                  (add-card-to-hand "p1" (create-card "Sheep" :id "c1"))
                  (add-card-to-hand "p1" (create-card "Sheep" :id "c2")))

        initial-hand (get-hand state "p1")
        _ (println "Initial hand size:" (count initial-hand))
        _ (println "Initial hand cards:" (map :name initial-hand))
        _ (println "Drake card ID in hand:" (:id (first (filter #(= (:name %) "Twilight Drake") initial-hand))))


        hand-size-after-playing (dec (count initial-hand)) ; -1 for the drake being played
        _ (println "Expected hand size after playing drake:" hand-size-after-playing)

        state-after-play (play-card state "p1" "drake" nil 0)


        final-hand (get-hand state-after-play "p1")
        _ (println "Final hand size:" (count final-hand))
        _ (println "Final hand cards:" (map :name final-hand))


        all-minions-p1 (get-minions state-after-play "p1")
        _ (println "P1 minions after play:" (map :name all-minions-p1))
        _ (println "P1 minion IDs:" (map :id all-minions-p1))

        drake-by-name (first (filter #(= (:name %) "Twilight Drake") all-minions-p1))
        _ (println "Drake found by name:" (not (nil? drake-by-name)))
        _ (when drake-by-name
            (println "Drake actual ID:" (:id drake-by-name))
            (println "Drake health override:" (get-in drake-by-name [:overrides :health-bonus]))
            (println "Drake damage taken:" (:damage-taken drake-by-name)))

        drake-by-id (get-minion state-after-play "drake")
        _ (println "Drake found by ID 'drake':" (not (nil? drake-by-id)))]

    (when drake-by-name
      (let [expected-health (+ 1 hand-size-after-playing)
            actual-health (get-health drake-by-name)]
        (println "Expected health:" expected-health)
        (println "Actual health:" actual-health)
        (is (= actual-health expected-health))))


    (is (not (nil? drake-by-name)) "Drake should exist after being played")))

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

                  (add-card-to-hand "p1" (create-card "Explosive Trap" :id "et"))
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "s1") 0)
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "s2") 1)
                  (add-minion-to-board "p2" (create-minion "Boulderfist Ogre" :id "attacker") 2))

        state-after-trap-play (play-card state "p1" "et" nil nil)
        state-p2-turn (assoc state-after-trap-play :player-id-in-turn "p2")
        state-after-attack (firestone.core/attack state-p2-turn "p2" "attacker" "h1")
        enemy-minions (get-minions state-after-attack "p2")
        enemy-hero-health (get-health (get-hero state-after-attack "p2"))
        p1-secrets (get-in state-after-attack [:players "p1" :secrets])]

    (is (= (count (filter #(= (:name %) "Sheep") enemy-minions)) 0)
        "Sheep should be dead from Explosive Trap damage")

    (is (= (count (filter #(= (:name %) "Boulderfist Ogre") enemy-minions)) 1)
        "Boulderfist Ogre should survive the trap damage")

    (is (= enemy-hero-health 28)
        "Enemy hero should take 2 damage from Explosive Trap")

    (is (empty? p1-secrets)
        "Explosive Trap should be consumed after triggering")))

(deftest test-cat-trick-effect
  (let [state (-> (create-game)
                  (add-card-to-hand "p1" (create-card "Cat Trick" :id "ct")))

        state-after-play (play-card state "p1" "ct" nil nil)
        p1-secrets (get-in state-after-play [:players "p1" :secrets])
        cat-trick-secret (first (filter #(= (:name %) "Cat Trick") p1-secrets))

        initial-minion-count (count (get-minions state-after-play "p1"))


        state-after-trigger (if cat-trick-secret
                              (firestone.construct/effects-parser
                                state-after-play
                                [cat-trick-secret]
                                "p1"
                                :opponent-play-spell
                                {})
                              state-after-play)

        final-minion-count (count (get-minions state-after-trigger "p1"))
        summoned-cat (first (filter #(= (:name %) "Cat in a Hat")
                                    (get-minions state-after-trigger "p1")))

        final-secrets (get-in state-after-trigger [:players "p1" :secrets])]

    (is (not (nil? cat-trick-secret)) "Cat Trick should be added to secrets when played")
    (is (= final-minion-count (inc initial-minion-count)) "Should summon one cat when triggered")
    (is (not (nil? summoned-cat)) "Cat in a Hat should be summoned")
    (is (= (get-attack state-after-trigger (:id summoned-cat)) 4) "Cat should have 4 attack")
    (is (= (get-health summoned-cat) 2) "Cat should have 2 health")
    (is (empty? final-secrets) "Cat Trick should be consumed after triggering")))



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

    (is (= final-enemy-health (- initial-enemy-health 9))))) ;; fatigue also

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
                  (add-card-to-hand "p1" (create-card "Misdirection" :id "md"))
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep1") 0)
                  (add-minion-to-board "p2" (create-minion "Boulderfist Ogre" :id "attacker") 0)
                  (add-minion-to-board "p2" (create-minion "Sheep" :id "sheep2") 1))
        state-with-secret (play-card state "p1" "md" nil nil)

        initial-secrets (get-in state-with-secret [:players "p1" :secrets])
        misdirection-secret (first (filter #(= (:name %) "Misdirection") initial-secrets))
        state-p2-turn (assoc state-with-secret :player-id-in-turn "p2")
        state-after-attack (firestone.core/attack state-p2-turn "p2" "attacker" "h1")
        final-secrets (get-in state-after-attack [:players "p1" :secrets])]

    (is (not (nil? misdirection-secret)) "Misdirection should be added to secrets when played")

    (is (empty? final-secrets) "Misdirection should be consumed after redirecting an attack")))

(deftest test-raid-leader-effect
  ;; Test Raid Leader aura: "Your other minions have +1 Attack"
  (let [state (-> (create-game)
                  (add-minion-to-board "p1" (create-minion "Raid Leader" :id "rl") 0)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep1") 1)
                  (add-minion-to-board "p1" (create-minion "Sheep" :id "sheep2") 2))

        sheep1-attack-before (get-attack state "sheep1")
        sheep2-attack-before (get-attack state "sheep2")
        rl-attack-before (get-attack state "rl")

        state-with-auras (firestone.construct/effects-parser
                           state
                           [(get-minion state "rl")]
                           "p1"
                           :process-auras)

        sheep1-attack-after (get-attack state-with-auras "sheep1")
        sheep2-attack-after (get-attack state-with-auras "sheep2")
        rl-attack-after (get-attack state-with-auras "rl")]

    (let [sheep1-after (get-minion state-with-auras "sheep1")
          sheep2-after (get-minion state-with-auras "sheep2")]
      (println "Sheep1 attack bonus:" (get-in sheep1-after [:overrides :attack-bonus]))
      (println "Sheep2 attack bonus:" (get-in sheep2-after [:overrides :attack-bonus])))

   
    (is (= sheep1-attack-after (+ sheep1-attack-before 1))
        (str "Sheep1 should get +1 attack from Raid Leader (" sheep1-attack-before " → " sheep1-attack-after ")"))
    (is (= sheep2-attack-after (+ sheep2-attack-before 1))
        (str "Sheep2 should get +1 attack from Raid Leader (" sheep2-attack-before " → " sheep2-attack-after ")"))
    (is (= rl-attack-after rl-attack-before)
        "Raid Leader's own attack should be unchanged")))

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