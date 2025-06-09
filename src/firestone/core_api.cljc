(ns firestone.core-api
  "The public api of the game."
  (:require [ysera.test :refer [is= error?]]
            [ysera.error :refer [error]]
            [firestone.core :refer [draw-card
                                    get-valid-attacks]]
            [firestone.construct :refer [get-minions
                                         effects-parser
                                         update-hero
                                         get-player-id-in-turn
                                         reset-player-mana
                                         should-take-fatigue?
                                         handle-fatigue
                                         create-game
                                         create-hero
                                         create-minion
                                         clear-sleepy-status-from-minions
                                         reset-turn-static-properties
                                         reset-attacks-performed-this-turn
                                         get-hero
                                         get-all-characters]]))

(defn end-turn
  {:test (fn []
           ; Test ending turn changes player in turn
           (let [state (create-game [])]
             (is= (-> state
                      (end-turn "p1")
                      (:player-id-in-turn))
                  "p2"))

           ; Test ending turn when it's not your turn
           (error? (-> (create-game [] :player-id-in-turn "p2")
                       (end-turn "p1")))

           ; Test end-turn effects (Moroes should summon a Steward)
           (let [state (create-game [{:minions [(create-minion "Moroes" :id "m1")]}])]
             (is= (-> state
                      (end-turn "p1")
                      (get-minions "p1")
                      (count))
                  2))

           ; Test mana increases for next player
           (let [state (create-game [{:hero (create-hero "Jaina Proudmoore" :max-mana 5 :mana 5)}
                                     {:hero (create-hero "Gul'dan" :max-mana 5 :mana 5)}])]
             (is= (-> state
                      (end-turn "p1")
                      (get-in [:players "p2" :hero :max-mana]))
                  6)))}
  [state player-id]
  (when-not (= (get-player-id-in-turn state) player-id)
    (error "The player with id " player-id " is not in turn."))

  (let [player-change-fn {"p1" "p2", "p2" "p1"}
        next-player-id (player-change-fn player-id)

        ;; Process end-of-turn effects
        current-player-minions (get-minions state player-id)
        current-player-hero (get-hero state player-id)
        state (effects-parser state
                              (cons current-player-hero current-player-minions)
                              player-id
                              :end-of-turn)

        ;; Process this-turn effects that need to expire
        all-characters (get-all-characters state)
        state (effects-parser state
                              all-characters
                              player-id
                              :this-turn)

        ;; Apply basic state updates
        state (-> state
                  (clear-sleepy-status-from-minions)
                  (reset-turn-static-properties next-player-id)
                  (reset-attacks-performed-this-turn next-player-id)
                  (update-hero next-player-id :has-used-your-turn false)
                  (assoc :minion-ids-summoned-this-turn [])
                  (assoc :player-id-in-turn next-player-id)
                  (reset-player-mana next-player-id))

        ;; Process next-turn effects for the player beginning their turn
        state (effects-parser state
                              (cons (get-hero state next-player-id)
                                    (get-minions state next-player-id))
                              next-player-id
                              :next-turn)

        ;; Handle fatigue or card drawing
        state (if (should-take-fatigue? state next-player-id)
                (handle-fatigue state next-player-id)
                (draw-card state next-player-id))

        ;; Process aura effects after all state changes
        state (effects-parser state
                              (concat (get-minions state "p1")
                                      (get-minions state "p2"))
                              next-player-id
                              :process-auras)

        ;; Update attack targets
        state (get-valid-attacks state)]

    state))






