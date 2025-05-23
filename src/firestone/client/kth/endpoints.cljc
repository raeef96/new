(ns firestone.client.kth.endpoints
  (:require [clojure.data.json :as json]
            [firestone.client.kth.edn-api :refer [create-game! end-turn! play-card! attack! undo! redo!]]))

(def cors-headers {"Access-Control-Allow-Origin"  "*"
                   "Access-Control-Allow-Methods" "*"})

(defn handler
  [request]
  (let [uri (:uri request)
        request-method (:request-method request)]
    (when-not (= uri "/favicon.ico")
      (clojure.pprint/pprint request))

    (cond
      (= request-method :options)
      {:status 204}

      (= uri "/engine-settings")
      {:status  200
       :headers (merge cors-headers {"Content-Type" "application/json"})
       :body    (json/write-str {:supports-redo true
                                 :supports-undo true
                                 :audio         :auto})}

      (= uri "/create-game")
      (let [body (when (:body request)
                   (-> (:body request)
                       slurp
                       (json/read-str :key-fn keyword)))
            players-data (:game body) ; Just extract the :game key
            result (create-game! players-data)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      (= uri "/play-minion-card")
      (let [body (when (:body request)
                   (-> (:body request)
                       slurp
                       (json/read-str :key-fn keyword)))
            player-id (:player-id body)
            card-id   (:card-id body)
            position  (:position body)
            target-id (:target-id body)
            result    (play-card! player-id card-id position target-id)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      (= uri "/play-spell-card")
      (let [body (when (:body request)
                   (-> (:body request)
                       slurp
                       (json/read-str :key-fn keyword)))
            player-id (:player-id body)
            card-id   (:card-id body)
            position  (:position body)
            target-id (:target-id body)
            result    (play-card! player-id card-id position target-id)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      (= uri "/end-turn")
      (let [body (when (:body request)
                   (-> (:body request)
                       slurp
                       (json/read-str :key-fn keyword)))
            player-id (:player-id body)
            result (end-turn! player-id)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      (= uri "/attack")
      (let [body (when (:body request)
                   (-> (:body request)
                       slurp
                       (json/read-str :key-fn keyword)))
            attacker-id (:attacker-id body)
            player-id (:player-id body)
            target-id (:target-id body)
            result (attack! player-id attacker-id target-id)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      (= uri "/undo")
      (let [result (undo!)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      (= uri "/redo")
      (let [result (redo!)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      :else
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    "<h1>hello</h1>"})))