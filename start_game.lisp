(load (merge-pathnames ".sbclrc" (user-homedir-pathname)))
(ql:quickload :biblio-almas)
(biblio-almas-game:start-game)
