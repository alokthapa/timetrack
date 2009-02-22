#lang scheme

(require (planet "leftparen.scm" ("vegashacker" "leftparen.plt" 4 (= 1))) 
	 "app.scm" 
	 "model/message.ss")

;;layout 
(define pgdesign (design 
		  #:css '("/css/reset-fonts-grids.css" "/css/main.css")
		  #:title "timeTrack"
		  #:body-wrap (lambda (bd)
				(**
				 `(div ((class "yui-t4") (id "doc"))
				       (div ((id "hd"))
					    (p ((class "t1")) ,(web-link "timeTrack" (page-url index-page))))
				       (div ((id "bd"))
					    (div ((id "yui-main"))
						 ,bd)
					    ;;(p "this is the main content"))
					    (div ((class "yui-b"))
						 ,@(page-links)))
				       (div ((id "ft"))
					    (p "brought to you by cdadar.com")))))))

;;pages
(define-page (index-page req)
  #:design pgdesign
  (read-messages)
  (** 
   `(p ((class "t2")) "What say you?")
   (form '((body "" long-text))
	 #:on-done (lambda (post)
		     (when (rec-prop post 'body)
			   (add-msg (make-msg-text (rec-prop post 'body))))
		     (redirect-to-page index-page))
	 #:submit-label "update"
	 #:skip-save #t)
   (disp-messages (get-msgs))))

(define-page (bm-page req)
  #:design pgdesign
  (read-messages)
  (**
   `(p ((class "t2")) "Bookmarks")
   (disp-messages (filter (lambda (m) (member "@bm" (msg-tags m))) (get-msgs)))))
 
(define-page (tag-page req tag)
  #:design pgdesign
  (read-messages)
  (**
   `(p ((class "t2")) "Tags")
   (disp-messages (filter (lambda (m) (member tag (msg-tags m))) (get-msgs)))))

(define-page (test-page req)
  #:design pgdesign
  (** "hello" '(br) " world"))

;; partials

(define (page-links)
  (list 
   `(p ,(web-link "Home" (page-url index-page)))
   `(p ,(web-link "Bookmarks" (page-url bm-page)))
   `(br)
   `(div ,@(map (lambda (t)
                  `(p ,(web-link t (page-url tag-page t)))) (get-all-tags (get-msgs))))))

(define (disp-messages msgs)
  (if (empty? msgs)
      `(p)
      `(div ((class "msgs"))
              ,@(map (lambda (m) (disp-message m)) msgs))))
 
(define (disp-message msg)
  (**
   `(div ((class "msg"))
     (p ((class "txt")) ,(rep-newline (msg-text msg)))
     (p ((class "dt")) ,(pp-datetime (seconds->date (msg-datetime msg)))))))


