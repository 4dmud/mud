(defpackage :4d
  (:use :cl)
  (:shadow room))
(defpackage :4d-event
  (:use :cl :alexandria)
  (:export trigger-condition player-login player-logout game-loop-fn))
(defpackage :4d-internal
  (:use :cl))

