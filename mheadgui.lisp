;;; mheadgui.lisp
;;; Web Gui for MetalHead

(load "world.lisp")
(load "/home/martin/mhead-gui/actions.lisp")
(in-package #:metalhead-gui)

(defmacro with-html (&body body)
    `(with-html-output (*standard-output* nil)
       ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil)
     ,@body))

(setf *js-string-delimiter* #\")

;; register static files with Hunchentoot.
(dolist (i '("heavy.css" "mhbg.jpg" "mhead.js"))
  (push (create-static-file-dispatcher-and-handler
	 (format nil "/~A" i) i) *dispatch-table*))


(defparameter *store-string* (format nil  (print-list *intro*)))

(defun main-page (output)
  (with-html-string
    (:head
     (:title "MetalHead!")
     (:link :type "text/css" :rel "stylesheet"
	    :href "/heavy.css")
     (:script :src "//ajax.googleapis.com/ajax/libs/jquery/2.0.3/jquery.min.js")
     (:script :src "/mhead.js"))
    (:body
     (:h1 :class "header" "MetalHead!")
     (:div :class "textarea"
	   (:form :name "form" :method :post :action "/parsing"
		  (:textarea :rows "30" :cols "70" :name "tarea"
			     :class "tarea" :id "tarea"
                             (str output))
		  (:div  (:input :type "text" :width "30px" :name "input"
				 :class "inptext" :id "inptext"
				 :autofocus "autofocus")
			 (:div 	 (:input :type "submit" :name "submit" 
					 :class "submit")))))
     ;; (:script "$('<div id=\"overlay\"><p>some text</p></div>').appendTo(document.body).fadeIn('slow');")
     (:p (fmt "~{~A ~}" (post-parameters*)))
     (:p (str (ps::ps-inline (input.value))))

     ;; (:script (str (ps (append-text (get-input-text)))))
     )))


(define-easy-handler (mainpage :uri "/MetalHead" :default-request-type :post)
    ()
  (main-page *store-string*))

(define-easy-handler (parsing :uri "/parsing" :default-request-type :post)
    ((input))
  (append-text input)
  (redirect "/MetalHead"))

(defvar *web-server* (make-instance 'easy-acceptor :port 4343))

(defun main ()
  (start *web-server*))

(defparameter *running-pub-quiz* nil)
(defparameter *turns* 0)
(defparameter *score* 0)
(defparameter *quiz-size* 10) ; number of questions to be asked for pub-quiz
(defparameter *quiz-win* 3)   ; limit of questions to get right.

(defparameter *questions*
  (question-list *quiz-size*))




(defun append-text (source)
  "set text of tarea to *store-string*. When form is submitted parse
   value on inptext to set value of tarea to returned value of parsed-command."
  (setf *store-string* (format nil (print-list (parse-command (split-string source))))))

(defun parse-command (commandlist)
  "parse entered player input. If entered command is <help> print help screen,
 if command is a <go in direction> command call walk-direction function. If it 
 refers to a  object which is not in current location, return 'not-here' string. If
 cmd is a <examine object> and not a valid action commnad return the :sdescription
 of the <object>. If it is a valid action cmd, call the function in (:action <item>)
 if it is a <examine object> cmd call the look-command-p function. If it is a 
 is-take-p command call take-command function."
  (cond
    ((is-help-p (first commandlist))
     (print-help))
    ((is-direction-p commandlist)
     (walk-direction (is-direction-p commandlist)))
    ((not-here commandlist)
     (list (concatenate 'string "you cannot see "
			(last-element commandlist)
			" here")))
    ((and (eql :look-closer-v (is-action-p commandlist))
	  (not (action-for-symbol (is-action-p commandlist))))
     (:sdescription (find-synonym-in-location (last-element commandlist))))
    ((is-action-p commandlist)
     (funcall (action-for-symbol (is-action-p commandlist))))
    ((look-command-p commandlist)
     (look-command-p commandlist))
    ((is-take-p (first commandlist))
     (take-command commandlist))
    ((inventory-command-p commandlist)
     (inventory-command-p commandlist))
    (t (no-action))))



;; (defun format-quiz (source target)
;;   "string-right-trim  question and answer, call parse-quiz function."
;;   (let ((answer (string-right-trim '(#\Space #\Newline) (text source)))
;; 	(question (string-right-trim '(#\Newline) (text target))))
;;     (clear-text target)
;;     (append-text target (format nil (parse-quiz question answer)))
;;     (clear-text source)
;;     (if (= *turns* *quiz-size*)
;; 	(append-text target
;; 		     (format nil "Your score is ~D in ~D turns." *score* *turns*)))))


(defun parse-quiz (question answer)
  "If answer is correct increase score and turns variables, 
   else only increase turns variable, finally print score."
  (if (correct-answer-p question answer)
      (progn
	(incf *score*)
	(incf *turns*))
      (incf *turns*))
  (if (> (length *questions*) 0)
      (pop *questions*)
      (progn
	(setf (:things *pub*)
	      (delete '*ticket-table* (:things *pub*)))
	(if (>= *score* *quiz-win*)
	    (print-list (won-ticket-f))
	    (print-list (lost-ticket-f))))))

(defparameter *pubquiz-turns* 0)
(defparameter *pubquiz-score* 0)
 

(defun split-string (string)
  "split string by space."
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))


(defun entnewlinify (list)
  "remove Newline Character at end of string list."
   (mapcar #'(lambda (x) (string-right-trim '(#\Newline) x)) (first  list)))    








