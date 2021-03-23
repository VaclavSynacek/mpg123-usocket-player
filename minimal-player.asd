(defsystem minimal-player 
  :serial t
  :components ((:file "player"))
  :depends-on ("uiop" "usocket"))
