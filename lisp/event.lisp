;; Events are interesting things that happen in 4d, that we might or might not want to dynamically respond to.
;; Rather than hardcoding a particular function to be run at the point where the event happens,
;; this event point should instead signal one of the conditions defined here.
(ffi:clines "#include \"../lisp-internal.h\"")
(in-package :4d-event)

(defun game-loop-fn (data)
  (ffi:c-inline (data) (:pointer-void) :void
		"game_loop_fn((struct game_loop_data*) #0);"
		:side-effects t))
		
		 
  
