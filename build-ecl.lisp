(require 'asdf)

(pushnew "./" asdf:*central-registry* :test 'equal)

(asdf:operate 'asdf:load-op 'minimal-player)

(asdf:make-build
  :minimal-player
  :type :program
  :move-here #P"./"
  :monolithic t
  :prologue-code '(progn
                    (require 'asdf)
                    (require 'sb-bsd-sockets))
  :epilogue-code '(player:main))
