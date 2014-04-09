(ns game.tictactoe
  (:require [game.server :as server]))

(declare process)

(defn- player1 []
  (server/broadcast "É a vez do jogador 1")

  (let [p1 (first @server/players)
        move (.readLine (p1 :reader))]
    (server/broadcast (str "Jogador 1: " move))
    #(process move :p1)))

(defn- player2 []
  (server/broadcast "É a vez do jogador 2")

  (let [p2 (second @server/players)
        move (.readLine (p2 :reader))]
    (server/broadcast (str "Jogador 2: " move))
    #(process move :p2)))

(defn- repeat-player [who]
  (if (= who :p1)
    #(player1)
    #(player2)))

(defn- next-player [who]
  (if (= who :p1)
    #(player2)
    #(player1)))

(defn- invalid [who]
  (server/broadcast "Jogada inválida")
  (repeat-player who))

(defn f [value]
  (cond (= value :p1) "O"
        (= value :p2) "X"
        (nil? value)  " "
        :else value))

(defn- draw-board []
  (let [{p0 0 p1 1 p2 2 p3 3 p4 4 p5 5 p6 6 p7 7 p8 8} @server/board]
    (server/broadcast (str "\n "
                           (f p0) " | " (f p1) " | " (f p2)
                           "\n---+---+--\n "
                           (f p3) " | " (f p4) " | " (f p5)
                           "\n---+---+--\n "
                           (f p6) " | " (f p7) " | " (f p8)))))

(defn- win [who]
  (if (= who :p1)
    (server/broadcast "O jogador 1 ganhou")
    (server/broadcast "O jogador 2 ganhou"))
  (server/stop))

(defn- draw []
  (server/broadcast "O jogo terminou empatado")
  (server/stop))

(defn- game-over? []
  (let [{p0 0 p1 1 p2 2 p3 3 p4 4 p5 5 p6 6 p7 7 p8 8} @server/board]
    (cond
     (and (keyword? p0)
          (or (= p0 p1 p2) (= p0 p3 p6) (= p0 p4 p8)))
        #(win p0)
     (and (keyword? p1) (= p1 p4 p7))
        #(win p1)
     (and (keyword? p2)
          (or (= p2 p5 p8) (= p2 p4 p6)))
        #(win p2)
     (and (keyword? p3)
          (= p3 p4 p5))
        #(win p3)
     (and (keyword? p6)
          (= p6 p7 p8))
        #(win p6)
     (keyword? (and p0 p1 p2 p3 p4 p5 p6 p7 p8))
       draw)))

;; FIXME: aqui não seria o caso de DRY com if-let?
(defn- process [move who]
  (if (re-matches #"^[012345678]$" move)
    (let [cell (Integer/parseInt move)]
      (if (@server/board cell)
        (invalid who)
        (do
          (swap! server/board assoc cell who)
          (draw-board)
          (if-let [end (game-over?)]
            end
            (next-player who)))))
    (invalid who)))

(defn- reset-board! []
  (reset! server/board {0 nil 1 nil 2 nil
                        3 nil 4 nil 5 nil
                        6 nil 7 nil 8 nil}))

(defn handler []
  (server/broadcast "Jogando o jogo da velha")
  (reset-board!)
  (trampoline player1))