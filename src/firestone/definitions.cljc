(ns firestone.definitions
  "A namespace for getting definitions. Its data is lazy loaded on demand."
  (:require [ysera.test :refer [is= error?]]
            [ysera.error :refer [error]]))


; Here is where the definitions are stored
; It is a reference to an immutable map of definitions that are loaded at runtime
(defonce definitions-atom (atom {}))


(defn add-definitions!
  "Adds the given definitions to the game."
  [definitions]
  (println "Adding" (count definitions) "definitions.")
  (swap! definitions-atom merge definitions))


(defn get-definitions
  "Returns all definitions in the game."
  []
  (vals (deref definitions-atom)))


(defn get-definition
  "Gets the definition identified by the name. Note that this is a none pure function. It depends on the definitions-atom."
  {:test (fn []
           (is= (get-definition "Sheep")
                {:name      "Sheep"
                 :attack    1
                 :health    1
                 :mana-cost 1
                 :race      :beast
                 :type      :minion
                 :set       :basic})
           ; The name can be present in a map with :name as a key
           (is= (get-definition {:name "Sheep"})
                (get-definition "Sheep"))

           (error? (get-definition "Something that does not exist")))}

  [name-or-entity]
  {:pre [(or (string? name-or-entity)
             (and (map? name-or-entity)
                  (contains? name-or-entity :name)))]}
  (let [definitions (do
                      ;; Checking if the definitions are loaded
                      (when (empty? (deref definitions-atom))
                        (require 'firestone.definitions-loader))
                      (deref definitions-atom))]
    (let [name (if (string? name-or-entity)
                 name-or-entity
                 (:name name-or-entity))
          definition (get definitions name)]
      (when (nil? definition)
        (error (str "The name " name-or-entity " does not exist. Are the definitions loaded?")))
      definition)))
