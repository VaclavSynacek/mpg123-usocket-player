(ql:quickload :usocket)

(in-package :cl)
(defpackage :player
  (:use :cl :usocket :uiop))
(in-package :player)

;; ****************************************************
;; Section wrapping command line tools this is based on
;; ****************************************************

(defvar *base-dir*)
(setf *base-dir* 
  (uiop:ensure-pathname (uiop:truenamize "~/music/")
                        :ensure-directory t
                        :want-non-wild t
                        :ensure-absolute t))

(defun execute (command &key (ignore-error-status nil))
  (format *error-output* "EXECUTING: ~a~%" command)
  (uiop:run-program command :ignore-error-status ignore-error-status))

(defun stop-playing ()
  (execute "killall mpg123" :ignore-error-status t))

(defun pause-playing ()
  (execute "killall -STOP mpg123" :ignore-error-status t))

(defun resume-playing ()
  (execute "killall -CONT mpg123" :ignore-error-status t))

(defun mpg123 (&rest filenames)
  (stop-playing)
  (execute (format nil "mpg123 ~{~a~^ ~} &" filenames)))


;; ************************************************************
;; Section implementing internal state and data representations
;; ************************************************************

(defvar *items* '())
(defvar *special-items* '())

(defun make-item (label action)
  (list
    (remove-if-not (lambda (ch)
                     (and
                       (alphanumericp ch)
                       (< (char-code ch) 124)))
                   label)
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

(defun find-item (id)
  (find-item-by-id id (append *items* *special-items*)))

(defun init-special-items ()
  (setf *special-items*
    (list (make-item "Stop everything!!!"
            (lambda () (stop-playing)))
          (make-item "Pause"
            (lambda () (pause-playing)))
          (make-item "Resume"
            (lambda () (resume-playing))))))

(defun init-music-items ()
  (let*
    ((base-dir-length (length (namestring *base-dir*)))
     (item-paths
       (append
         (uiop:subdirectories *base-dir*)
         (remove-if-not
           (lambda (p)
             (string-equal "mp3" (pathname-type p)))
           (uiop:directory-files *base-dir*))))
     (sorted-item-paths (sort item-paths #'string-lessp :key #'namestring))
     (items (mapcar
              (lambda (file)
                (make-item
                  (subseq (namestring file) base-dir-length)
                  (let
                    ((file-to-close-over file))
                    (lambda ()
                      (mpg123 (format nil "'~a'~a"
                                      (namestring file-to-close-over)
                                      (if (directory-pathname-p file-to-close-over)
                                        "*.mp3"
                                        "")))))))
             sorted-item-paths)))
    (setf *items* items)))


;; ***********************************
;; Section implementing http interface
;; ***********************************

(defun h200 ()
    (format t "HTTP/1.0 200 OK~%")
    (format t "Content-Type: text/html; charset=utf-8~%~%"))

(defun h303 ()
    (format t "HTTP/1.0 303 See Other~%Location: /~%~%"))

(defun h400 ()
    (format t "HTTP/1.0 400 Bad Request~%~%"))

(defun special-item-render (item)
  (format t
    "<form action=\"/~a/\" method=\"post\">
      <input type=\"submit\" value=\"~a\" />
    </form>"
   (item-id item)
   (item-label item)))

(defun item-render (item)
  (format t
    "
<li><form action=\"/~a/\" method=\"post\">
      <input type=\"submit\" value=\"Play\" />
    </form>
    <span>~a</span>
</li>"
   (item-id item)
   (item-label item)))


(defun index ()
  (h200)
  (init-music-items)
  (format t "<html>
<head>
  <title>Minimal Player</title>
  <style>form {display: inline;}</style>
</head>
<body>
  <h1>Minimal Player</h1>
  <ul>
    <li>")
  (loop for item in *special-items*
          do (special-item-render item))
  (format t "</li>")
  (loop for item in *items*
        do (item-render item))
  (format t "  </ul>
</body></html>"))

(defun play (id)
  (let
    ((item (find-item id)))
    (if item
      (progn
        (item-act item)
        (h303))
      (h400))))

(defun serve (port)
  (usocket:with-socket-listener (socket "0.0.0.0" port :reuse-address t)
    (loop 
      (usocket:with-server-socket (connection (usocket:socket-accept socket))
        (ignore-errors
          (with-open-stream (stream (usocket:socket-stream connection))
            (let
              ((first-line (read-line stream))
               (*standard-output* stream))
              (cond
                ((string= "GET / " (subseq first-line 0 6))  (index))
                ((string= "POST /" (subseq first-line 0 6))
                 (play (let
                         ((r (subseq first-line 6)))
                         (subseq r 0 (position #\/ r)))))
                (t (h400)))
              (force-output stream)
              (finish-output stream)
              (sleep 1))))))))

(defun main ()
  (format t "Minimal Player starting~%")
  (init-special-items)
  (serve 9999)
  (format t "Minimal Player ends~%"))

(main)


