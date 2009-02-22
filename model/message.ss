#lang scheme
(require scheme/date)

;;message 

(provide (all-defined-out))


(require scheme/date)

(define (rep-newline str)
  str)
  ;(regexp-replace* #rx"\r\n" str " <br /> "))

(define *messages* (list))

(define (delete-msg msg)
  (set! *messages* (remove msg *messages*))
  (write-messages))

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
    (add-msg (make-msg-text "Welcome to timeTrack"))
    (add-msg (make-msg-text "just post a url like www.cdadar.com and it will show up in bookmarks"))
    (add-msg (make-msg-text "@todo you can add a new tag"))
    (add-msg (make-msg-text "the tags will show up dynamically as you add them, for eg., @home working on @timetrack will create two tags"))
    (add-msg (make-msg-text "you can also create schedules like @sch 5 pm pick up the kids")))
  (with-handlers ((exn? log-missing-exn-handler))
    (set! *messages* (with-input-from-file outputfile read))))
  