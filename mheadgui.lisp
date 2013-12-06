;;; mheadgui.lisp
;;; Web Gui for MetalHead

(in-package #:metalhead-gui)

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
       "/hmw.jpg" "mhbg.jpg") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
       "/mhead.js" "mhead.js") *dispatch-table*)

(defvar *some-text*
  "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way...")

(defun main-page ()
  (with-html-string
    (:head
     (:title "MetalHead!")
     (:link :type "text/css" :rel "stylesheet"
	    :href "/heavy.css")
     (:script :src "/mhead.js"))
    (:body
     (:h1 :class "header" "MetalHead!")
     (:br)
     (:div :class "textarea"
	   (:form :method :post
		  (:textarea :rows "50" :cols "70" :name "tarea"
			     :class "tarea" :id "tarea"
			    (fmt "du bist der groesste~%oder nicht"))
		  (:div  (:input :type "text" :width "30px" :name "inptext"
				 :class "inptext")
			 (:div 	 (:input :type "submit" :name "submit"
					 :class "submit")))))
     (:script (str (ps (set-text "tarea" (lisp *some-text*)))))
     (:script (str (ps (append-text (lisp *some-text*))))))))


(define-easy-handler (mainpage :uri "/MetalHead" )
    ()
  (main-page))

    

(defvar *web-server* (make-instance 'easy-acceptor :port 4343))


(defun main ()
  (start *web-server*))








