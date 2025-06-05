;;; src/parser.lisp
(in-package #:biblio-almas-game)

(defun split-string (string &optional (separator " "))
  "Divide uma string por um separador (string ou caractere).
   Retorna uma lista de substrings."
  (let ((sep-char (if (characterp separator) separator (char separator 0))))
    (loop :for i = 0 :then (1+ j)
          :as j = (position sep-char string :start i :test #'char=)
          :collect (subseq string i j)
          :while j)))

(defun palavras-para-id-alvo (lista-de-palavras)
  "Converte uma lista de palavras (strings) para um ID de alvo (keyword).
   Ex: (\"livro\" \"vermelho\") -> :LIVRO-VERMELHO
   Ex: (\"arquivista\" \"chefe\") -> :ARQUIVISTA-CHEFE"
  (when lista-de-palavras
    (intern (string-upcase (format nil "~{~A~^-~}" lista-de-palavras)) :keyword)))

(defun parse-command (command-string)
  "Converte a string de comando para minúsculas, divide em palavras,
   remove palavras vazias e retorna uma lista representando o comando."
  (let* ((normalized-string (string-downcase command-string))
         (words (split-string normalized-string " "))
         (cleaned-words (remove-if (lambda (s) (string= s "")) words)))
    (if cleaned-words
        (let* ((verbo-str (first cleaned-words))
               (argumentos-str (rest cleaned-words))
               (verbo (intern (string-upcase verbo-str) :keyword)))
          (cond
            ;; Comandos de movimento
            ((member verbo '(:ir :vá :seguir :go :norte :sul :leste :oeste :n :s :l :o :e :w))
             (let ((direcao-arg-str (first argumentos-str))
                   (direcao-final nil))
               (cond
                 (direcao-arg-str (setf direcao-final (intern (string-upcase direcao-arg-str) :keyword)))
                 ((member verbo '(:norte :n)) (setf direcao-final :norte))
                 ((member verbo '(:sul :s)) (setf direcao-final :sul))
                 ((member verbo '(:leste :l :e)) (setf direcao-final :leste))
                 ((member verbo '(:oeste :o :w)) (setf direcao-final :oeste)))

               (if direcao-final
                   (list :mover direcao-final)
                   (if (member verbo '(:ir :vá :seguir :go)) ; "ir" sem direção
                       (list :desconhecido cleaned-words)
                       (list :mover verbo))))) ; Trata "norte" (sem argumento) como "ir norte"

            ;; Examinar
            ((member verbo '(:examinar :olhar :ler :x))
             (if argumentos-str
                 (list :examinar (palavras-para-id-alvo argumentos-str))
                 (list :examinar :sala)))

            ;; Pegar
            ((member verbo '(:pegar :apanhar :coletar :get :take))
             (if argumentos-str
                 (list :pegar (palavras-para-id-alvo argumentos-str))
                 (list :pegar nil)))

            ;; Largar
            ((member verbo '(:largar :soltar :deixar :drop))
             (if argumentos-str
                 (list :largar (palavras-para-id-alvo argumentos-str))
                 (list :largar nil)))

            ;; Inventário
            ((member verbo '(:inventario :inv :i :pertences))
             (list :inventario))

            ;; Falar com NPC
            ((member verbo '(:falar :conversar :perguntar :talk :ask))
             (if argumentos-str
                 (list :falar (palavras-para-id-alvo argumentos-str))
                 (list :falar nil))) ; Falar sem especificar com quem

            ;; Outros comandos
            (t (list :desconhecido cleaned-words))))
        nil))) ; Retorna nil se não houver palavras após a limpeza
