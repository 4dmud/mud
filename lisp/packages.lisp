(defpackage :4d-internal
  (:use :cl)
  (:export mud-log))

(defpackage :4d
  (:use :cl)
  (:shadow room)
  (:export game-loop-fn player-login player-logout))

