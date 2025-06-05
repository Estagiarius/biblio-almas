;;; src/engine.lisp
(in-package #:biblio-almas-game)

(defun mostrar-saidas (sala)
  "Imprime as saídas disponíveis da sala."
  (if (hash-table-count (sala-saidas sala))
      (progn
        (format t "Saídas óbvias: ")
        (loop :for direcao :being :the :hash-keys :of (sala-saidas sala)
              :do (format t "~a " (string-downcase (symbol-name direcao))))
        (format t "~%"))
      (format t "Não há saídas óbvias daqui.~%")))

(defun mostrar-objetos-na-sala (sala-id)
  "Imprime os nomes dos objetos visíveis na sala."
  (let ((nomes-objetos (obter-nomes-objetos-na-sala sala-id)))
    (if nomes-objetos
        (format t "Você vê aqui: ~{~a~^, ~}.~%" nomes-objetos)
        )))

(defun mostrar-npcs-na-sala (sala-id)
  "Imprime as descrições curtas dos NPCs na sala."
  (let ((descricoes-npcs (obter-descricoes-curtas-npcs-na-sala sala-id)))
    (when descricoes-npcs
      (if (= (length descricoes-npcs) 1)
          (format t "Perto dali, você nota ~A.~%" (first descricoes-npcs))
          (format t "Perto dali, você nota: ~{~A~^; ~}.~%" descricoes-npcs)))))


(defun descrever-localizacao-atual ()
  "Descreve a localização atual do jogador."
  (let ((sala-atual (obter-sala *localizacao-atual-jogador*)))
    (if sala-atual
        (progn
          (format t "~%~a~%" (sala-nome sala-atual))
          (format t "~a~%" (sala-descricao sala-atual))
          (mostrar-objetos-na-sala *localizacao-atual-jogador*)
          (mostrar-npcs-na-sala *localizacao-atual-jogador*)
          (mostrar-saidas sala-atual))
        (format t "Erro: O jogador está em uma localização desconhecida (~a).~%" *localizacao-atual-jogador*))))

(defun tentar-mover (direcao)
  "Tenta mover o jogador para a DIRECAO especificada."
  (let* ((sala-atual (obter-sala *localizacao-atual-jogador*))
         (id-sala-destino (gethash direcao (sala-saidas sala-atual))))
    (if id-sala-destino
        (let ((sala-destino (obter-sala id-sala-destino)))
          (if sala-destino
              (progn
                (setf *localizacao-atual-jogador* id-sala-destino)
                t)
              (progn
                (format t "Erro: A sala de destino '~a' não existe! Verifique a definição das salas.~%" id-sala-destino)
                nil)))
        (progn
          (format t "Você não pode ir nessa direção.~%")
          nil))))

(defun tentar-examinar (alvo-id)
  "Tenta examinar um ALVO-ID (pode ser :SALA, um objeto na sala ou no inventário, ou um NPC)."
  (cond
    ((or (null alvo-id) (eq alvo-id :sala))
     (descrever-localizacao-atual)
     t)
    (t
     (let* ((objeto-def (obter-objeto-definido alvo-id))
            (npc-modelo (obter-npc-modelo alvo-id))
            (objeto-na-sala (and objeto-def (objeto-esta-na-sala-p alvo-id *localizacao-atual-jogador*)))
            (objeto-no-inventario (and objeto-def (jogador-tem-objeto-p alvo-id)))
            (npc-na-sala (and npc-modelo (find alvo-id (sala-npcs (obter-sala *localizacao-atual-jogador*))))))
       (cond
         (objeto-na-sala
          (format t "~a~%" (objeto-descricao objeto-def)) t)
         (objeto-no-inventario
          (format t "~a~%" (objeto-descricao objeto-def)) t)
         (npc-na-sala
          (format t "Você olha para ~a. ~a.~%" (npc-nome npc-modelo) (npc-descricao-curta npc-modelo))
          t)
         (t
          (format t "Você não vê nenhum(a) ~a por aqui para examinar.~%" (string-downcase (symbol-name alvo-id)))
          nil))))))

(defun tentar-pegar (objeto-id)
  "Tenta pegar um OBJETO-ID da sala atual."
  (unless objeto-id (format t "Pegar o quê?~%") (return-from tentar-pegar nil))
  (let ((objeto-def (obter-objeto-definido objeto-id)))
    (cond
      ((not (objeto-esta-na-sala-p objeto-id *localizacao-atual-jogador*))
       (format t "Não há nenhum(a) ~a aqui.~%" (string-downcase (symbol-name objeto-id))) nil)
      ((not objeto-def)
       (format t "Isso não é um objeto que possa ser definido (~a).~%" objeto-id) nil)
      ((not (objeto-pegavel objeto-def))
       (format t "Você não pode pegar ~a.~%" (objeto-nome objeto-def)) nil)
      (t
       (remover-objeto-da-sala objeto-id *localizacao-atual-jogador*)
       (adicionar-ao-inventario objeto-id)
       (format t "Você pegou ~a.~%" (objeto-nome objeto-def))
       t))))

(defun tentar-largar (objeto-id)
  "Tenta largar um OBJETO-ID do inventário na sala atual."
  (unless objeto-id (format t "Largar o quê?~%") (return-from tentar-largar nil))
  (let ((objeto-def (obter-objeto-definido objeto-id)))
    (cond
      ((not (jogador-tem-objeto-p objeto-id))
       (format t "Você não está carregando nenhum(a) ~a.~%" (string-downcase (symbol-name objeto-id))) nil)
      ((not objeto-def)
        (format t "Este objeto (~a) não tem uma definição.~%" objeto-id) nil)
      (t
       (remover-do-inventario objeto-id)
       (addicionar-objeto-a-sala objeto-id *localizacao-atual-jogador*)
       (format t "Você largou ~a.~%" (objeto-nome objeto-def))
       t)))))

(defun mostrar-inventario ()
  "Mostra os objetos no inventário do jogador."
  (if *inventario-jogador*
      (progn
        (format t "Você está carregando:~%")
        (loop :for obj-id :in *inventario-jogador*
              :do (let ((obj-def (obter-objeto-definido obj-id)))
                    (if obj-def
                        (format t "- ~a~%" (objeto-nome obj-def))
                        (format t "- um objeto desconhecido (ID: ~a)~%" obj-id))))))
      (format t "Você não está carregando nada.~%"))
  t)

(defun tentar-falar (npc-id)
  (if (not npc-id)
      (progn (format t "Falar com quem?~%") nil)
      (let* ((sala-atual-id *localizacao-atual-jogador*)
             (npc-modelo (obter-npc-modelo npc-id)))
        (cond
          ((not npc-modelo)
           (format t "Não conheço ninguém chamado '~A'.~%" (string-capitalize (symbol-name npc-id))) nil)
          ((not (eq (npc-localizacao-atual npc-modelo) sala-atual-id))
           (format t "~A não está aqui.~%" (npc-nome npc-modelo)) nil)
          ((not (npc-dialogo npc-modelo))
           (format t "~A não parece ter nada a dizer.~%" (npc-nome npc-modelo)) nil)
          (t
           (if (stringp (npc-dialogo npc-modelo))
               (format t "~A~%" (npc-dialogo npc-modelo))
               (format t "~A murmura algo incompreensível.~%" (npc-nome npc-modelo))) ; Placeholder
           t))))))

(defun process-command (parsed-command-list)
  (if parsed-command-list
      (let ((verbo (first parsed-command-list))
            (params (rest parsed-command-list))
            (argumento (first params))) ; O argumento principal (ID do objeto/NPC/direção)
        (cond
          ((eql verbo :mover)
           (if argumento (tentar-mover argumento) (progn (format t "Mover para onde?~%") nil)))
          ((eql verbo :examinar)
           (tentar-examinar argumento))
          ((eql verbo :pegar)
           (tentar-pegar argumento))
          ((eql verbo :largar)
           (tentar-largar argumento))
          ((eql verbo :inventario)
           (mostrar-inventario))
          ((eql verbo :falar)
           (tentar-falar argumento))
          ((eql verbo :desconhecido)
           (format t "Não entendi o que você quis dizer com '~{~a~^ ~}'.~%" params) nil)
          (t
           (format t "Comando não implementado ainda: ~a~%" parsed-command-list) nil)))
      (progn
        (format t "Não entendi isso.~%")
        nil)))

(defun game-loop ()
  (descrever-localizacao-atual)
  (loop
    (format t "~%> ")
    (finish-output)
    (let ((input (read-line)))
      (if (string-equal input "sair")
          (progn
            (format t "Até logo!~%")
            (return))
          (let* ((parsed-command (parse-command input))
                 (resultado-comando (process-command parsed-command)))
            (when (and parsed-command (eql (first parsed-command) :mover) resultado-comando)
              (descrever-localizacao-atual))
            (when (and parsed-command (eql (first parsed-command) :examinar)
                       (or (null (second parsed-command)) (eql (second parsed-command) :sala))
                       resultado-comando)
              ))))))
