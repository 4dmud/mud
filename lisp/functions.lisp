;;;when there's an inline function call, ecl can't recompile the file.
;;;Therefore, all inline function calls are put here, making sure everything else remains recompilable.
(in-package :4d-internal)

(ffi:clines #.(format nil "#include \"~a\"" (asdf:system-relative-pathname :4d-lisp "../lisp-internal.h")))
(ffi:clines 
	    "void send_to_char(const char*, Character*);"
	    "void page_string (Descriptor*, char*, int);")

(defun send-to-char (ch string)
  (ffi:c-inline ((ffi:convert-to-cstring string) ch)
		(:cstring :pointer-void) :void
		"send_to_char(#0, (Character *)#1);"
		:side-effects t))

(defun page-string (ch string)
  (ffi:c-inline (ch (ffi:convert-to-cstring string))(:pointer-void :cstring) :void
		"page_string (((Character *) #0)->desc, #1, TRUE);"))

(ffi:clines "void basic_mud_log ( const char *format, ... );")
(defun mud-log (&rest args)
  (ffi:c-inline ((apply #'format nil args)) (:cstring) :void
		"basic_mud_log(#0);"))

(ffi:clines "void add_var ( struct trig_var_data **var_list,const char *name,const char *value, long id );")
(defun dg-add-global-var (script-ptr name value context)
  (ffi:c-inline (script-ptr name value context) (:pointer-void :cstring :cstring :long) :void
		"add_var(&(((struct script_data*)#0)->global_vars),#1,#2,#3);"
		:side-effects t))

(defun dg-add-local-var (trig-ptr name value context)
  (ffi:c-inline (trig-ptr name value context) (:pointer-void :cstring :cstring :long) :void
		"add_var(&(((struct trig_data*)#0)->var_list),#1,#2,#3);"
		:side-effects t))

(ffi:clines "void save_char_vars ( Character *ch );")

(defun player-save-vars (player-ptr)
  (ffi:c-inline (player-ptr) (:pointer-void) :void
		"save_char_vars((Character*)#0);"
		:side-effects t))

(defun room-get-description (room-ptr)
  (ffi:c-inline (room-ptr) (:pointer-void) :cstring
		"((Room*)#0)->GetDescription()"
		:one-liner t
		:side-effects nil))
		

(defun command-interpeter (ch-ptr command)
  (ffi:c-inline (ch-ptr command) (:pointer-void :cstring) :void
		"command_interpreter ((Character *)#0, #1);"))

(defun c-game-loop-fn (data)
    (ffi:c-inline (data) (:pointer-void) :void
		"game_loop_fn((struct game_loop_data*) #0);"
		:side-effects t))
