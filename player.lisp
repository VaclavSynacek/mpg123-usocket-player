(ql:quickload :usocket)

(in-package :cl)
(defpackage :player
  (:use :cl :usocket :uiop))
(in-package :player)


(defvar *base-dir*)
(setf *base-dir* (first (directory "~/music/")))

(defvar *items* '())

(defun make-item (label action)
  (list
    (symbol-name (gensym))
    label
    action))

(defun item-id (item)
  (first item))

(defun item-label (item)
  (second item))

(defun item-action (item)
  (third item))

(defun item-act (item)
  (funcall (item-action item)))

(defun find-item-by-id (id items)
  (find id items :key #'item-id :test #'string=))

(defun h200 ()
    (format t "HTTP/1.0 200 OK~%")
    (format t "Content-Type: text/html; charset=utf-8~%~%"))

(defun h303 ()
    (format t "HTTP/1.0 303 See Other~%Location: /~%~%"))

(defun h400 ()
    (format t "HTTP/1.0 400 Bad Request~%~%"))

(defun li (item)
  (format t
    "
<li><form action=\"/~a/\" method=\"post\">
      <input type=\"submit\" value=\"----&gt;\" />
    </form>
    <span>~a</span>
</li>"
   (item-id item)
   (item-label item)))


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
  (loop for item in *items*
        do (li item))
  (format t "  </ul>
</body></html>"))

(defun stop-playing ()
  (uiop:run-program "killall mpg123" :ignore-error-status t))

(defun pause-playing ()
  (uiop:run-program "killall -STOP mpg123" :ignore-error-status t))

(defun resume-playing ()
  (uiop:run-program "killall -CONT mpg123" :ignore-error-status t))

(defun mpg123 (&rest filenames)
   (stop-playing)
   (let
     ((command (format nil "mpg123 ~{~a~^ ~} &~%" filenames)))
     (format *error-output* "COMMAND IS:~a~%" command)
     (uiop:run-program command)))

(defun play (id)
  (item-act (find-item-by-id id *items*))
  (h303))

(defun serve (port)
  (usocket:with-socket-listener (socket "0.0.0.0" port :reuse-address t)
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
                (t (h400)))))))))


(defun scan ()
  (setf *items* '())
  (push (make-item
          "Stop everything!!!"
          (lambda () (stop-playing)))
        *items*)
  (push (make-item
          "Pause"
          (lambda () (pause-playing)))
        *items*)
  (push (make-item
          "Resume"
          (lambda () (resume-playing)))
        *items*)
  (dolist (file (directory (format nil "~a~a" *base-dir* "/*.mp3")))
    (push (make-item
            file
            (lambda ()
              (mpg123 (format nil "'~a'" (namestring file)))))
          *items*))
  (dolist (file (directory (format nil "~a~a" *base-dir* "/*/")))
    (push (make-item
            file
            (lambda ()
              (mpg123 (format nil "'~a'*.mp3" (namestring file)))))
          *items*))
  (setf *items* (reverse *items*)))


(defun main ()
  (format t "Minimal Player starting~%")
  (scan)
  (serve 9999)
  (format t "Minimal Player ends~%"))

(main)
