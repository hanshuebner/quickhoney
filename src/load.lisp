(push :cl-gd-gif *features*)

(asdf:oos 'asdf:load-op :quickhoney)
(asdf:oos 'asdf:load-op :swank)

(swank::create-server :port 4085)

;; disable ldb
#+sbcl
(sb-alien:define-alien-routine ("disable_lossage_handler" disable-sbcl-ldb) sb-alien:void)
#+sbcl
(disable-sbcl-ldb)

#+cmu
(mp::startup-idle-and-top-level-loops)
