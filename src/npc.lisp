;;; src/npc.lisp
(in-package #:biblio-almas-game)

(defstruct npc
  (id nil :type symbol)
  (nome "" :type string)
  (descricao-curta "" :type string)
  (dialogo nil) ; Pode ser uma string, uma lista de strings, ou uma função
  (localizacao-atual nil :type (or null symbol))) ; ID da sala onde o NPC está

(defvar *npcs-definidos* (make-hash-table :test 'eq)
  "Tabela hash para armazenar os 'modelos' de todos os NPCs definíveis no jogo, indexados por ID.")

(defun definir-npc-modelo (id nome descricao-curta &key dialogo)
  "Define um modelo de NPC e o armazena em *npcs-definidos*."
  (let ((novo-npc-modelo (make-npc :id id
                                   :nome nome
                                   :descricao-curta descricao-curta
                                   :dialogo dialogo)))
    (setf (gethash id *npcs-definidos*) novo-npc-modelo)
    novo-npc-modelo))

(defun obter-npc-modelo (id)
  "Retorna o modelo de NPC do *npcs-definidos* pelo seu ID."
  (gethash id *npcs-definidos*))

(defun posicionar-npc (npc-id sala-id)
  "Coloca uma instância de um NPC (referenciado por npc-id) em uma sala (sala-id).
   Atualiza a lista de npcs da sala e a localização no struct do NPC."
  (let ((npc-modelo (obter-npc-modelo npc-id))
        (sala (obter-sala sala-id))) ; obter-sala é de world.lisp
    (when (and npc-modelo sala)
      ;; Adiciona o ID do NPC à lista de npcs da sala, evitando duplicatas
      (pushnew npc-id (sala-npcs sala) :test #'eq)
      ;; Atualiza a localização no struct do NPC (modelo)
      (setf (npc-localizacao-atual npc-modelo) sala-id)
      t)))

(defun remover-npc-da-sala (npc-id sala-id)
  "Remove um NPC de uma sala e define sua localização como nil."
  (let ((npc-modelo (obter-npc-modelo npc-id))
        (sala (obter-sala sala-id)))
    (when (and npc-modelo sala)
      (setf (sala-npcs sala) (remove npc-id (sala-npcs sala) :test #'eq))
      (setf (npc-localizacao-atual npc-modelo) nil)
      t)))

;; Definição do NPC Arquivista Chefe
(definir-npc-modelo :arquivista-chefe
  "O Arquivista Chefe"
  "uma figura alta e encapuzada, curvada sobre um livro antigo"
  :dialogo "O Arquivista Chefe ergue os olhos lentamente do pesado tomo. Sua voz é como o virar de páginas antigas. \"Outro Achado... perdido entre as estantes. O que buscas, ou do que foges?\"")
