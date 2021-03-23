(require 'asdf)

(pushnew "./" asdf:*central-registry* :test 'equal)

(asdf:operate 'asdf:load-op 'minimal-player)

(asdf:make-build :usocket
                 :type :static-library
                 :move-here #P"./"
                 :init-name "init_usocket")

(asdf:make-build :split-sequence
                 :type :static-library
                 :move-here #P"./"
                 :init-name "init_split_sequence")

(asdf:make-build :minimal-player
                 :type :static-library
                 :move-here #P"./"
                 :init-name "init_player")

(c:build-program "minimal-player-ecl"
                 :lisp-files '("split-sequence.a" "usocket.a" "minimal-player.a")
                 :prologue-code '(progn
                                   (require 'sb-bsd-sockets)
                                   (require 'asdf))
                 :epilogue-code '(player:main))

#|

(asdf:make-build
  :minimal-player
  :type :program
  :move-here #P"./"
  :monolithic t
  :prologue-code '(progn
                    (require 'sb-bsd-sockets)
                    (require 'asdf))
  :epilogue-code '(player:main))
|#
