;;; run.lisp
;; Este script carrega e inicia o jogo "A Biblioteca das Almas Perdidas".
;; Para executar a partir da linha de comando: sbcl --script run.lisp

;; --- Carregamento Robusto do Quicklisp ---
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; --- Verificação do Quicklisp ---
#-quicklisp
(progn
  (format *error-output*
          "Quicklisp não está configurado ou não foi encontrado.~%~
           Por favor, instale o Quicklisp seguindo as instruções em: ~
           https://www.quicklisp.org/beta/~%~
           Certifique-se também de que o ASDF está configurado para encontrar este projeto.~%")
  (uiop:quit 1))

;; --- Carregar o Sistema do Jogo ---
(format t "Carregando A Biblioteca das Almas Perdidas...~%")
(handler-case (ql:quickload :biblio-almas)
  (error (c)
    (format *error-output*
            "Erro ao carregar o sistema :biblio-almas. ~
             Certifique-se de que o ASDF está configurado corretamente para encontrar o projeto.~%~
             Detalhes do erro: ~A~%" c)
    (uiop:quit 1)))

;; --- Iniciar o Jogo ---
(format t "Iniciando o jogo...~%~%")
(handler-case (biblio-almas-game:start-game)
  (error (c)
    (format *error-output* "Ocorreu um erro durante a execução do jogo: ~A~%" c)
    (uiop:quit 1)))

;; --- Fim do Jogo ---
(format t "~%Obrigado por jogar A Biblioteca das Almas Perdidas!~%")
(uiop:quit 0)
