#lang scheme

(require (planet "leftparen.scm" ("vegashacker" "leftparen.plt" 4 (= 1))) "app.scm")

(define (page-links)
  (**
   `(p ,(web-link "Home" (page-url index-page)))
   `(p ,(web-link "Bookmarks" (page-url bm-page)))))

(define (pagelayout yeild)
  (**
   `(h1 "timeTrack")
   `(br)
   (page-links)
   (yeild)))
   
(define-page (bm-page req)
  (**
   `(h1 "timeTrack")
   `(br)
   (page-links)
   `(p "Bookmarks")
   `(p ,@(disp-messages (filter (lambda (m) (match-url (msg-text m))) (get-msgs))))))

(define-page (index-page req)
  (**
   `(h1 "timeTrack")
   `(br)
   (page-links)
   `(p "What say you?")
   (form '((body "" long-text))
	 #:on-done (lambda (post)
		     (add-msg (make-msg-text (rec-prop post 'body)))
		     (redirect-to-page index-page)))
   `(p  ,@(disp-messages (get-msgs)))
   ))
   



(define (disp-messages msgs)
  (map (lambda (m) (disp-message m)) msgs))
 

(define (disp-message msg)
  (**
  `(p ,(msg-text msg))
  `(p ,(pp-datetime (seconds->date (msg-datetime msg))))))

;;this should really go into a different page
(require scheme/date)

(define *messages* (list))


(define (get-msgs) *messages*)
(define-struct msg (datetime text for)
  #:prefab)

(define (pp-datetime dt)
  (string-join (list (date->string dt) 
                              (number->string (date-hour dt)) ":"
                              (number->string (date-minute dt)) ":"
                              (number->string (date-second dt))) " "))

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
