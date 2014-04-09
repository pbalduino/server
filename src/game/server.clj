(ns game.server
  (:import [java.net ServerSocket]
           [java.io BufferedReader
                    InputStreamReader
                    PrintWriter]))

(def players (atom []))
(def board   (atom {}))

(defn wrap-connection [socket]
  {:socket socket
   :reader (BufferedReader. (InputStreamReader. (.getInputStream socket)))
   :writer (PrintWriter. (.getOutputStream socket) true)})

(defn add-player! [server-socket]
  (let [wrapped (wrap-connection (. server-socket accept))]
    (swap! players conj wrapped)
    (. (wrapped :writer) println "Seja bem vindo")
    wrapped))

(defn broadcast [message]
  (doseq [player @players]
    (let [writer (player :writer)]
      (.println writer message)))
  (println (str "> Broadcast: " message)))

(defn stop []
  (broadcast "Parando o servidor.")
  (broadcast "Tchau.")
  (doseq [player @players]
    (.close (player :reader))
    (.close (player :writer))
    (.close (player :socket))))

(defn start [handler]
  (broadcast "Aguardando jogadores")
  (let [server-socket (ServerSocket. 4567)]
    (broadcast "Servidor pronto na porta 4567")
    (add-player! server-socket)
    (broadcast "Um jogador conectado")

    (add-player! server-socket)
    (broadcast "Dois jogadores conectados")

    (handler)))