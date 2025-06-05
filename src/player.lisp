;;; src/player.lisp
(in-package #:biblio-almas-game)

(defvar *localizacao-atual-jogador* :atrio-de-chegada
  "O ID da sala onde o jogador está atualmente.")

(defvar *inventario-jogador* nil
  "Uma lista dos IDs dos objetos que o jogador carrega.")

(defun adicionar-ao-inventario (objeto-id)
  "Adiciona um OBJETO-ID ao inventário do jogador."
  (pushnew objeto-id *inventario-jogador* :test #'eq))

(defun remover-do-inventario (objeto-id)
  "Remove um OBJETO-ID do inventário do jogador."
  (setf *inventario-jogador* (remove objeto-id *inventario-jogador* :test #'eq)))

(defun jogador-tem-objeto-p (objeto-id)
  "Verifica se o jogador possui o OBJETO-ID no inventário."
  (member objeto-id *inventario-jogador* :test #'eq))

;; Definições do jogador e inventário irão aqui.
