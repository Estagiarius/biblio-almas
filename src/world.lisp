;;; src/world.lisp
(in-package #:biblio-almas-game)

(defstruct objeto
  id          ; symbol (keyword)
  nome        ; string (e.g., "um livro vermelho")
  descricao   ; string (e.g., "Um tomo antigo com capa de couro.")
  pegavel     ; boolean
  )

(defstruct sala
  (id nil :type symbol)
  (nome "" :type string)
  (descricao "" :type string)
  (saidas (make-hash-table) :type hash-table)
  (objetos (make-hash-table :test 'eq) :type hash-table) ; Mapeia objeto-id para o objeto-id (ou para o objeto em si, se preferir)
  (npcs nil :type list) ; Lista de IDs dos NPCs presentes na sala
  )

(defvar *salas* (make-hash-table)
  "Uma hash-table que mapeia IDs de salas (keywords) para objetos SALA.")

(defvar *objetos-definidos* (make-hash-table :test 'eq)
  "Uma hash-table que mapeia IDs de objetos (keywords) para objetos OBJETO.")

(defun definir-objeto (id nome descricao &key (pegavel nil))
  "Cria e armazena uma definição de objeto."
  (let ((novo-objeto (make-objeto :id id
                                  :nome nome
                                  :descricao descricao
                                  :pegavel pegavel)))
    (setf (gethash id *objetos-definidos*) novo-objeto)
    novo-objeto))

(defun obter-objeto-definido (id)
  "Retorna a struct OBJETO da definição global."
  (gethash id *objetos-definidos*))

(defun definir-sala (id nome descricao &key (saidas nil))
  "Cria e armazena uma nova sala."
  (let ((nova-sala (make-sala :id id
                              :nome nome
                              :descricao descricao
                              :saidas (make-hash-table)
                              :objetos (make-hash-table :test 'eq) ; Inicializa o hash-table de objetos
                              :npcs nil))) ; NPCs inicializado como nil por padrão pelo defstruct
    (loop :for (direcao destino-id) :on saidas :by #'cddr
          :do (setf (gethash direcao (sala-saidas nova-sala)) destino-id))
    (setf (gethash id *salas*) nova-sala)
    nova-sala))

(defun obter-sala (id)
  "Retorna o objeto SALA correspondente ao ID, ou NIL se não encontrado."
  (gethash id *salas*))

(defun adicionar-objeto-a-sala (objeto-id sala-id)
  "Adiciona um OBJETO-ID à lista de objetos de uma SALA-ID."
  (let ((sala (obter-sala sala-id))
        (objeto (obter-objeto-definido objeto-id)))
    (if (and sala objeto)
        (setf (gethash objeto-id (sala-objetos sala)) objeto-id) ; Armazena o ID do objeto
        (format t "Erro: Não foi possível adicionar ~a à sala ~a. Sala ou objeto não definido.~%" objeto-id sala-id))))

(defun remover-objeto-da-sala (objeto-id sala-id)
    "Remove um OBJETO-ID da lista de objetos de uma SALA-ID."
    (let ((sala (obter-sala sala-id)))
        (if sala
            (remhash objeto-id (sala-objetos sala))
            (format t "Erro: Sala ~a não encontrada para remover objeto.~%" sala-id))))

(defun objeto-esta-na-sala-p (objeto-id sala-id)
    (let ((sala (obter-sala sala-id)))
        (and sala (gethash objeto-id (sala-objetos sala)))))


(definir-sala :atrio-de-chegada
              "Átrio de Chegada"
              "Você está em um átrio majestoso e um pouco intimidante. Poeira antiga paira no ar, iluminada por feixes de luz que emanam de altas janelas empoeiradas. Grandes estantes vazias se alinham nas paredes, sugerindo um conhecimento perdido ou esquecido. Um único corredor segue para o norte."
              :saidas (:norte :corredor-leste))

(definir-sala :corredor-leste
              "Corredor Leste"
              "Um corredor longo e silencioso se estende diante de você. O ar aqui é mais frio e o silêncio é quase palpável. As paredes são revestidas de pedra lisa e fria ao toque. Ao sul, você pode ver a luz fraca do Átrio de Chegada."
              :saidas (:sul :atrio-de-chegada))

;; Definir objetos
(definir-objeto :livro-vermelho "um livro vermelho" "Um tomo antigo com capa de couro vermelho desbotado. Parece muito velho." :pegavel t)
(definir-objeto :chave-pequena "uma chave pequena" "Uma chave de bronze pequena e ornamentada. Parece que poderia abrir algo delicado." :pegavel t)
(definir-objeto :estatua-gargula "uma estátua de gárgula" "Uma estátua de gárgula de pedra, com uma expressão carrancuda e asas recolhidas. Está coberta de poeira." :pegavel nil)

(defun obter-descricoes-curtas-npcs-na-sala (sala-id)
  (let* ((sala (obter-sala sala-id))
         (npc-ids (when sala (sala-npcs sala)))
         (descricoes-curtas nil))
    (dolist (npc-id npc-ids)
      (let ((npc-modelo (obter-npc-modelo npc-id))) ; obter-npc-modelo de npc.lisp
        (when npc-modelo
          (push (npc-descricao-curta npc-modelo) descricoes-curtas))))
    (nreverse descricoes-curtas)))

(defun popular-mundo-inicial ()
  ;; Objetos
  (addicionar-objeto-a-sala :livro-vermelho :atrio-de-chegada)
  (addicionar-objeto-a-sala :estatua-gargula :atrio-de-chegada)
  (addicionar-objeto-a-sala :chave-pequena :corredor-leste)

  ;; NPCs
  (posicionar-npc :arquivista-chefe :atrio-de-chegada))


(defun obter-nomes-objetos-na-sala (sala-id)
  "Retorna uma lista dos nomes dos objetos visíveis na sala."
  (let ((sala (obter-sala sala-id)))
    (if sala
        (loop :for obj-id :being :the :hash-keys :of (sala-objetos sala)
              :collect (let ((obj-def (obter-objeto-definido obj-id)))
                         (if obj-def (objeto-nome obj-def) "um objeto desconhecido")))
        nil)))

;; Popular o mundo
(popular-mundo-inicial)
