(ql:quickload :usocket)

(in-package :cl)
(defpackage :player
  (:use :cl :usocket :uiop))
(in-package :player)


(defvar *base-dir*)
(setf *base-dir* "~/music")

(defvar *mp3s*)

(defun scan ()
  (setf *mp3s*
        (loop for file in (directory (format nil "~a~a" *base-dir* "/*.mp3"))
              collect (cons
                        (symbol-name (gensym))
                        file))))

(defun h200 ()
    (format t "HTTP/1.0 200 OK~%~%"))

(defun h303 ()
    (format t "HTTP/1.0 303 See Other~%Location: /~%~%"))

(defun h400 ()
    (format t "HTTP/1.0 400 Bad Request~%~%"))

(defun li (mp3)
  (format t
    "
<li><form action=\"/~a/\" method=\"post\">
      <input type=\"submit\" value=\"Play\" />
    </form>
    <span>~a</span>
</li>"
   (car mp3)
   (cdr mp3)))


(defun index ()
  (h200)
  (format t "<html>
<head>
  <title>Minimal Player</title>
  <style>form {display: inline;}</style>
</head>
<body>
  <h1>Minimal Player</h1>
  <ul>")
  (loop for mp3 in *mp3s*
        do (li mp3))
  (format t "  </ul>
</body></html>"))

(defun mpg123 (filename)
   (ignore-errors     
     (uiop:run-program "killall mpg123"))
   (let
     ((command (format nil "mpg123 '~a' &" (namestring filename))))
     (uiop:run-program command)))

(defun play (mp3id)
  (let
    ((filename (cdr (assoc mp3id *mp3s* :test #'equal))))
    (mpg123 filename)
    (h303)))

(defun serve (port)
  (usocket:with-socket-listener (socket "0.0.0.0" port :reuse-address t)
    (ignore-errors
     (loop 
       (usocket:with-server-socket (connection (usocket:socket-accept socket))
        (with-open-stream (stream (usocket:socket-stream connection))
           (let* ((first-line (read-line stream))
                  (*standard-output* stream))       
             (cond
               ((string= "GET / " (subseq first-line 0 6))  (index))
               ((string= "POST /" (subseq first-line 0 6))
                (play (let
                        ((r (subseq first-line 6)))
                        (subseq r 0 (position #\/ r)))))
               (t (h400))))))))))


(defun main ()
  (scan)
  (serve 9999))

(main)
