(ns firestone.client.kth.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core :refer [attack play-card use-hero-power]]
            [firestone.core-api :as engine-api]
            [firestone.construct :as construct]
            [firestone.definitions-loader]
            [firestone.client.kth.mapper :as mapper]
            [ysera.error :refer [error]]))

;; Enhanced state storage for undo/redo functionality
;; - history: a vector of game states
;; - current-index: the index of the current state in the history
;; - max-index: the maximum valid index (for redo purposes)
(def game-states (atom {}))

;; State management helper functions
(defn add-new-game [initial-state game-id]
  (swap! game-states assoc game-id {:history [initial-state]
                                    :current-index 0
                                    :max-index 0})
  initial-state)

(defn get-game-state
  "Gets the current game state based on the current index"
  [game-id]
  (let [game-data (@game-states game-id)]
    (when game-data
      (get-in game-data [:history (:current-index game-data)]))))

(defn get-current-action-index
  "Gets the current action index for a game"
  [game-id]
  (get-in @game-states [game-id :current-index] 0))

(defn update-game-state
  "Updates game state, adding it to history and updating indices"
  [game-id updated-state]
  (let [current-data (@game-states game-id)
        current-index (:current-index current-data)
        new-history (conj (subvec (:history current-data) 0 (inc current-index)) updated-state)
        new-index (inc current-index)]
    (swap! game-states assoc game-id {:history new-history
                                      :current-index new-index
                                      :max-index new-index})))

(defn undo-game-state
  "Moves to previous state in history if available"
  [game-id]
  (let [current-data (@game-states game-id)
        current-index (:current-index current-data)]
    (if (> current-index 0)
      (do
        (swap! game-states assoc-in [game-id :current-index] (dec current-index))
        (get-in @game-states [game-id :history (dec current-index)]))
      (error "Cannot undo: already at earliest state"))))

(defn redo-game-state
  "Moves to next state in history if available"
  [game-id]
  (let [current-data (@game-states game-id)
        current-index (:current-index current-data)
        max-index (:max-index current-data)]
    (if (< current-index max-index)
      (do
        (swap! game-states assoc-in [game-id :current-index] (inc current-index))
        (get-in @game-states [game-id :history (inc current-index)]))
      (error "Cannot redo: already at latest state"))))

(defn map-game-input
  "Given a game state and a game-body vector (one entry per player), updates the state by
   processing the keys :board, :mana, and :max-mana.
   Expects game-body to be a vector whose entries will be paired with player ids in order,
   e.g. player \"p1\", then \"p2\"."
  [state game-body]
  (reduce
    (fn [acc [data player-id]]
      (let [board   (get data :board)
            mana    (get data :mana)
            max-mana (get data :max-mana)]
        (-> acc
            ;; If there is a :board key, add those minions to this player's board.
            (cond-> board (construct/add-minions-to-board player-id board))
            ;; If a max-mana is provided then update hero max mana.
            (cond-> max-mana (construct/set-max-mana player-id max-mana))
            ;; If a mana value is provided then update hero current mana.
            (cond-> mana (construct/update-hero-mana player-id mana)))))
    state
    (map vector game-body ["p1" "p2"])))

(defn create-game!
  [players-data]
  (let [;; Don't default to empty array - keep nil as nil to distinguish between "no data" and "empty data"
        state (create-game players-data)
        state' (map-game-input state (or players-data []))
        ;; FIXED: Only add default cards when players-data is truly nil (no request data provided)
        ;; If players-data is an empty array [], that means the client explicitly sent empty data
        state-with-defaults (if (nil? players-data)
                              (mapper/populate-default-cards state' nil)
                              state')
        game-id "the-game-id"]
    (add-new-game state-with-defaults game-id)
    [(mapper/game->client-game state-with-defaults 0)]))

(defn end-turn!
  [player-id]
  (let [game-id "the-game-id"
        current-game-state (get-game-state game-id)]
    (if current-game-state
      (let [;; Use provided player-id or get current player from game state
            current-player (or player-id (construct/get-player-id-in-turn current-game-state))
            updated-game-state (engine-api/end-turn current-game-state current-player)
            new-player-in-turn (get updated-game-state :player-id-in-turn)]
        (update-game-state game-id updated-game-state)
        (println "Turn ended. Player:" current-player "New player in turn:" new-player-in-turn)
        (let [new-action-index (get-current-action-index game-id)]
          [(mapper/game->client-game updated-game-state new-action-index)]))
      (error "No game found for game-id" game-id))))

(defn play-card!
  [player-id card-id position target-id]
  (let [game-id "the-game-id"
        current-game-state (get-game-state game-id)]
    (if current-game-state
      (let [updated-game-state (play-card current-game-state player-id card-id position target-id)]
        (update-game-state game-id updated-game-state)
        (let [new-action-index (get-current-action-index game-id)]
          [(mapper/game->client-game updated-game-state new-action-index)]))
      (error "No game found for game-id" game-id))))

(defn attack!
  [player-id attacker-id target-id]
  (let [game-id "the-game-id"
        current-game-state (get-game-state game-id)]
    (if current-game-state
      (let [updated-game-state (attack current-game-state player-id attacker-id target-id)]
        (update-game-state game-id updated-game-state)
        (let [new-action-index (get-current-action-index game-id)]
          [(mapper/game->client-game updated-game-state new-action-index)]))
      (error "No game found for game-id" game-id))))

(defn use-hero-power!
  [player-id target-id]
  (let [game-id "the-game-id"
        current-game-state (get-game-state game-id)]
    (if current-game-state
      (let [updated-game-state (use-hero-power current-game-state player-id target-id)]
        (update-game-state game-id updated-game-state)
        (let [new-action-index (get-current-action-index game-id)]
          [(mapper/game->client-game updated-game-state new-action-index)]))
      (error "No game found for game-id" game-id))))

(defn undo!
  []
  (let [game-id "the-game-id"]
    (try
      (let [previous-state (undo-game-state game-id)
            new-action-index (get-current-action-index game-id)]
        [(mapper/game->client-game previous-state new-action-index)])
      (catch Exception e
        (println "Undo error:" (.getMessage e))
        (error (.getMessage e))))))

(defn redo!
  []
  (let [game-id "the-game-id"]
    (try
      (let [next-state (redo-game-state game-id)
            new-action-index (get-current-action-index game-id)]
        [(mapper/game->client-game next-state new-action-index)])
      (catch Exception e
        (println "Redo error:" (.getMessage e))
        (error (.getMessage e))))))