(asdf:defsystem #:biblio-almas
  :description "Um protótipo de um jogo adventure - A Biblioteca das Almas Perdidas."
  :version "0.0.1"
  :author "Jules <ai.dev.google@example.com>" ; Placeholder author
  :license "MIT"
  :depends-on (:cl-store)
  :serial t
  :components ((:file "src/package")
               (:file "src/config" :depends-on ("src/package"))
               (:file "src/utils" :depends-on ("src/package"))
               (:file "src/world" :depends-on ("src/package"))
               (:file "src/npc" :depends-on ("src/package" "src/world"))
               (:file "src/player" :depends-on ("src/package" "src/world"))
               (:file "src/parser" :depends-on ("src/package"))
               (:file "src/engine" :depends-on ("src/package" "src/world" "src/player" "src/parser" "src/npc"))
               (:file "src/main" :depends-on ("src/package" "src/engine"))))
