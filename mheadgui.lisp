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


(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/heavy.css" "heavy.css") hunchentoot:*dispatch-table*)

(defun main-page ()
  (with-html
    (:head
     (:title "MetalHead!")
     (:link :type "text/css" :rel "stylesheet"
	    :href "/heavy.css"))
    (:body
     (:h3 :class "header" "MetalHead!")
     (:div :class "textarea"
	   (:form :method :post
		  (:textarea :rows "30" :cols "40" :name "tarea" :class "tarea"))))))

(define-easy-handler (mainpage :uri "/MetalHead" )
    ()
  (main-page))

(defvar *web-server* (make-instance 'easy-acceptor :port 4343))







