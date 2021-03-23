(require 'asdf)

(pushnew "./" asdf:*central-registry* :test 'equal)

(asdf:operate 'asdf:load-op 'minimal-player)

(sb-ext:save-lisp-and-die
  "sbcl-minimal-player"
  :toplevel #'player:main
  :executable t :purify t)
  
