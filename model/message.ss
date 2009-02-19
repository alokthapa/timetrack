#lang scheme
(require scheme/date)

;;message 

(provide (all-defined-out))


(require scheme/date)

(define (rep-newline str)
  str)
  ;(regexp-replace* #rx"\r\n" str " <br /> "))

(define *messages* (list))

(define (get-msgs) *messages*)

(define-struct msg (datetime text tags)
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

(define (extract-tags txt)
  (let ((tokens (regexp-split #rx"[ \n]" txt)))
    (filter (lambda (t)
              (and (> (string-length t) 0)
                   (equal? (string-ref t 0) #\@)))
	    tokens)))
  
(define (make-msg-text txt)
  (make-msg (current-seconds) txt (extract-tags txt)))

(define (add-msg msg) (set! *messages* (cons msg *messages*)))

(define (match-url str)
  (regexp-match #px"(https?://)?[\\w]*.?[\\w]+.(com|net|org|edu)" str))

(define (get-all-tags msgs)
  (remove-duplicates 
   (flatten (map (lambda (m) (msg-tags m)) msgs))))

;;for testing purposes only

(add-msg (make-msg-text "here goes nothing"))

(add-msg (make-msg-text "another job for @work"))

(add-msg (make-msg-text "I can't find any @work"))

(add-msg (make-msg-text "m @sad today really"))
