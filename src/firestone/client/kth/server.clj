(ns firestone.client.kth.server
  (:require [org.httpkit.server :as httpkit]
            [firestone.client.kth.endpoints :refer [handler]]))

(defonce server-atom (atom nil))

(defn create-server!
  []
  (if (deref server-atom)
    "do nothing - already running"
    (let [server-stop-fn (httpkit/run-server (fn [request]
                                               (handler request))
                                             {:port 8001})]
      (reset! server-atom server-stop-fn))))

(defn stop-server!
  []
  (let [server-stop-fn (deref server-atom)]
    (if server-stop-fn
      (do (server-stop-fn :timeout 100)
          (reset! server-atom nil))
      "The server is not running")))

(comment
  (create-server!)
  (stop-server!))
