(ql:quickload :usocket)

(in-package :cl)
(defpackage :player
  (:use :cl :usocket :uiop))
(in-package :player)

(defun h200 ()
    (format t "HTTP/1.0 200 OK~%~%"))

(defun h400 ()
    (format t "HTTP/1.0 400 Bad Request~%~%"))

(defun index ()
  (h200)
  (format t "<html>
<head><title>Minimal Player</title></head>
<body>
  <h1>Minimal Player</h1>
  <ul>
    <li><form action=\"\\\" method=\"post\">
          <input type=\"submit\" value=\"Play\" />
        </form> <span>bla</span></li>
    <li>fuj</li>
  </ul>
</body></html>"))

(index)

(defun play ()
  (h200))

(defun serve (port)
  (usocket:with-socket-listener (socket "0.0.0.0" port :reuse-address t)
    (loop 
       (usocket:with-server-socket (connection (usocket:socket-accept socket))
        (with-open-stream (stream (usocket:socket-stream connection))
           (let* ((first-line (read-line stream))
                  (*standard-output* stream))       
             (cond
               ((string= "GET / " (subseq first-line 0 6))  (index))
               ((string= "POST / " (subseq first-line 0 7)) (play))
               (t (h400)))))))))


;(princ first-line)))))))

(serve 9904)
