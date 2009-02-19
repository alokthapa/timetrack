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


(define (extract-tags txt)
  (let ((tokens (regexp-split #rx"[ \n]" txt)))
    (filter (lambda (t)
              (and (> (string-length t) 0)
                   (equal? (string-ref t 0) #\@)))
	    tokens)))
  
(define outputfile "outputfile")

;; read and write from a file
(define (write-messages)
  (with-output-to-file outputfile
    (lambda () (write *messages*))
    #:exists 'replace))

(define (make-msg-text txt)
  (make-msg (current-seconds) txt (extract-tags txt)))

(define (add-msg msg) 
  (set! *messages* (cons msg *messages*))
  (write-messages))

(define (match-url str)
  (regexp-match #px"(https?://)?[\\w]*.?[\\w]+.(com|net|org|edu)" str))

(define (get-all-tags msgs)
  (remove-duplicates 
   (flatten (map (lambda (m) (msg-tags m)) msgs))))

(define (read-messages)
  (define (log-missing-exn-handler exn)
    (add-msg (make-msg-text "first post")))
  (with-handlers ((exn? log-missing-exn-handler))
    (set! *messages* (with-input-from-file outputfile read))))
  