#lang scheme

(require (planet "leftparen.scm" ("vegashacker" "leftparen.plt" 4 (= 1))) 
	 "app.scm")

(define (page-links)
  (list 
   `(p ,(web-link "Home" (page-url index-page)))
   `(p ,(web-link "Bookmarks" (page-url bm-page)))
   `(p ,(web-link "Tags" (page-url tag-page)))))

(define-page (test-page req)
  #:design pgdesign
  "hello world")
		 
(define pgdesign (design 
		  #:css '("css/reset-fonts-grids.css" "css/main.css")
		  #:title "timeTrack"
		  #:body-wrap (lambda (bd)
				(**
				 `(div ((class "yui-t4") (id "doc"))
				       (div ((id "hd"))
					    (p ((class "t1")) "timeTrack"))
				       (div ((id "bd"))
					    (div ((id "yui-main"))
						 ,bd)
					    ;;(p "this is the main content"))
					    (div ((class "yui-b"))
						 ,@(page-links)))
				       (div ((id "ft"))
					    (p "brought to you by cddar.com")))))))

   
(define-page (bm-page req)
  #:design pgdesign
  (**
   `(p ((class "t2")) "Bookmarks")
   (let ((msgs (filter (lambda (m) (match-url (msg-text m))) (get-msgs))))
      (if (empty? msgs)
	  `(p)
	  `(p ,@(disp-messages msgs))))))
;;   `(p ,@(disp-messages (filter (lambda (m) (match-url (msg-text m))) (get-msgs))))))
 
(define-page (tag-page req)
  #:design pgdesign
  `(p ((class "t2")) "Tags"))

(define-page (index-page req)
  #:design pgdesign
  (** 
   `(p ((class "t2")) "What say you?")
   (form '((body "" long-text))
	 #:on-done (lambda (post)
		     (when (rec-prop post 'body)
			   (add-msg (make-msg-text (rec-prop post 'body))))
		     (redirect-to-page index-page))
	 #:submit-label "update"
	 #:skip-save #t)
   `(p  ,@(disp-messages (get-msgs)))))

(define (disp-messages msgs)
  (list 
   `(div ((class "msgs"))
     ,@(map (lambda (m) (disp-message m)) msgs))))
 
(define (disp-message msg)
  (**
   `(div ((class "msg"))
     (p ((class "txt")) ,(msg-text msg))
     (p ((class "dt")) ,(pp-datetime (seconds->date (msg-datetime msg)))))))

;;this should really go into a different page
(require scheme/date)

(define *messages* (list))

(define (get-msgs) *messages*)

(define-struct msg (datetime text for)
  #:prefab)

(define (pp-datetime dt)
  (string-join (list (date->string dt) 
		     (string-append 
		      (number->string (date-hour dt)) ":"
		      (number->string (date-minute dt)) ":"
		      (number->string (date-second dt))) ) " "))

;;refactor to utility
(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
            ((char=? ch (string-ref str b)) (if (= a b)
                                              (split (+ 1 a) (+ 1 b))
                                              (cons (substring str a b) (split b b))))
            (else (split a (+ 1 b)))))))
      (split 0 0))))


(define (make-msg-text txt)
  (define (extract-for txt)
  (and (string? txt)
       (let ((f (car (str-split txt #\space))))
         (if (equal? (string-ref f 0) #\@) 
             (string->symbol f) 
            '@root))))
  (make-msg (current-seconds) txt (extract-for txt)))

(define (add-msg msg) (set! *messages* (cons msg *messages*)))

(add-msg (make-msg-text "here goes nothing"))

(add-msg (make-msg-text "another job"))


(define (match-url str)
  (regexp-match #px"(https?://)?[\\w]*.?[\\w]+.(com|net|org|edu)" str))
