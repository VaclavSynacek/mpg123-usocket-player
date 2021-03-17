(ql:quickload :usocket)

(in-package :cl)
(defpackage :player
  (:use :cl :usocket :uiop))
(in-package :player)

(defun serve (port)
  (usocket:with-socket-listener (socket "127.0.0.1" port :reuse-address t)
    (loop 
       (usocket:with-server-socket (connection (usocket:socket-accept socket))
        (with-open-stream (stream (usocket:socket-stream connection))
           (let* ((first-line (read-line stream)))
                  ;(*standard-output* stream))       
              (princ first-line)))))))

(serve 9900)
