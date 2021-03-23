(require 'asdf)

(pushnew "./" asdf:*central-registry* :test 'equal)

(asdf:operate 'asdf:load-op 'minimal-player)

(asdf:make-build
  :minimal-player
  :type :program
  :move-here #P"./"
  :monolithic t
  :prologue-code '(progn
                    (require 'sb-bsd-sockets)
                    (require 'asdf))
  :epilogue-code '(player:main))
