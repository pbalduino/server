(ns game.core
  (:require [game.server :as server]
            [game.tictactoe :as ttt]))

(defn -main [& args]
  (server/start ttt/handler))
