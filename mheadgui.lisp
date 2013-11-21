(ql:quickload '(:hunchentoot :cl-who :parenscript))

(defpackage #:mheadgui
  (:use :cl  :cl-who :parenscript :hunchentoot))

(in-package #:mheadgui)

(defmacro with-html (&body body)
    `(with-html-output (*standard-output* nil)
       ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil)
     ,@body))

(setf *js-string-delimiter* #\")

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/heavy.css" "heavy.css") hunchentoot:*dispatch-table*)
(push (create-static-file-dispatcher-and-handler
       "/hmw.jpg" "hmw.jpg") *dispatch-table*)

(defun main-page ()
  (with-html-string
    (:head
     (:title "MetalHead!")
     (:link :type "text/css" :rel "stylesheet"
	    :href "/heavy.css"))
    (:body
     (:h3 :class "header" "MetalHead!")
     (:br)
     (:div :class "textarea"
	   (:form :method :post
		  (:textarea :rows "50" :cols "70" :name "tarea" :class "tarea")
		  (:div  (:input :type "text" :width "30px" :name "inptext" :class "inptext")
			 (:input :type "submit" :name "submit" :onclick
				 (ps (alert "Thanks for clicking!")))))))))

(define-easy-handler (mainpage :uri "/MetalHead" )
    ()
  (main-page))

(define-easy-handler (tutorial2 :uri "/tutorial2") ()
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Parenscript tutorial: 2nd example")
      (:script :type "text/javascript"
               (str (ps
                      (defun greeting-callback ()
                        (alert "Hello World"))))))
     (:body
      (:h2 "Parenscript tutorial: 2nd example")
      (:a :href "#" :onclick (ps (greeting-callback))
          "Hello World")))))
    

(define-easy-handler (tutorial1 :uri "/tutorial1") ()
  (with-html-output-to-string (s)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body (:h2 "Parenscript tutorial: 1st example")
            "Please click the link below." :br
            (:a :href "#" :onclick (ps (alert "Hello World"))
                "Hello World")))))
    

(defvar *web-server* (make-instance 'easy-acceptor :port 4343))









