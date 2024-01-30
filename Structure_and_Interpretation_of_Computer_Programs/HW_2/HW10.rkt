#lang simply-scheme
(define (im message . who)
  ;;;Send message to who.
  (cond ((empty? who))
         #t)
        (else
         (if (not
                  (send-request (make-request whoiam (car who) 'send-msg message) port-to-server))
             (close-connection)
             #t)
         (apply im (cons message (cdr who)))))

;upon looking at the solution, if there is a problem the connection will close
;before trying the rest of the connections

 (define (client-request-handler)
    (let* ((port-from-client (socket-input client-sock))
	   (port-to-client (socket-output client-sock))
	   (req (get-request port-from-client)))
      (if (not req)
	  (remove-client name)
	  (begin	    
	    (format logging "Received request: ~S~%" req)
	    (cond
	     ((equal? 'send-msg (request-action req))
 ;begin modifications
              (if (equal? "broadcast" (request-dst req))
                  (for-each (lambda (client-list-pair) ;uswitched map to for-each after looking at
                                                    ;solution
                         (format logging "Delivering message from ~A to ~A.~%"
                                 (request-src req)
                                 (car client-list-pair))
                         (if (not
                              (send-request (make-request (request-src req)
                                                          (car client-list-pair)
                                                          'receive-msg
                                                          (request-data req))
                                            (cdr client-list-pair)))
                             (remove-client (car client-list-pair))))
                       (cdr client-list))

                  (let ((port-to-dst (find-port-to-client (request-dst req))))
                    (if port-to-dst
                        (begin
                          (format logging "Delivering message from ~A to ~A.~%"
                                  (request-src req)
                                  (request-dst req))
                          (if (not
                               (send-request (make-request (request-src req)
                                                           (request-dst req)
                                                           'receive-msg
                                                           (request-data req))
                                             port-to-dst))
                              (remove-client (request-dst req))))
                        (begin
                          (format logging "User not found: ~A. Letting sender know.~%"
                                  (request-dst req))
                          (if (not
                               (send-request (make-request 'server
                                                           name
                                                           'receive-msg
                                                           (format #f "User not found: ~A"
                                                                   (request-dst req)))
                                             port-to-client))
                              (remove-client name)) )))
              )
;end modifications	     
	     ((equal? 'logout (request-action req))
	      (remove-client name))
	     
	     (else
	      (format logging "Unrecognized action requested: ~A. Letting sender know.~%" (request-action req))
	      (if (not
		   (send-request (make-request 'server
					       (request-dst req)
					       'receive-msg
					       (format #f "Unrecognized action: ~A"
						       (request-action req)))
				 port-to-client))
		  (remove-client name)) ))))
    ;; if other data ready, handle them now    
    (if (and (not (port-closed? port-from-client))
	     (char-ready? port-from-client))
	(client-request-handler)) ))

  ;; Set up the handler
  (when-port-readable (socket-input client-sock) client-request-handler))

;3 Yes, the server could recieve a list of destination to send to instead of individual requests
;(whoops, read solution before discussing benefits: making changes on the client side is less likely
;to introduce bugs and will promote backwards capability between the server and old versions.
; If the server can provide the client list, yes, otherwise, the client might not have the info
; to make a request to every port (after looking at the solution: making changes on the server
; can reduce traffic.


;4 All done in client side so the server doesn't have to store data. It would be nice for privacy if
;the server could block the messages instead of forwarding them. After looking at the solution, I
; modified the send def so that person A can't send a message to person B if person B is banned by
; person A, thus avoiding an ifinite loop of refusal messages if person B also banned person A

(define (im who message)
  ;;;Send message to who.
  (cond ((member? who banned-list)
         (error "Can't message someone on your banned list.")
        ((not
         (send-request (make-request whoiam who 'send-msg message) port-to-server))
         (close-connection))
        #t)))

(define banned-list '())

(define (get-clients-list)
  ;;;Return a list of known client names.
  (map key banned-list))

(define (add-client-to-banned-list name)
   (if (not (member name banned-list))
       (begin set! banned-list (cons name banned-list) #t)
       #f))

(define (remove-client-from-table name)
   (define (helper who lis)
    (cond ((null? lis) #f)
	      ((equal? who (car lis))
	       (set! lis (cdr lis))
	         result)
          ((equal? who (cadr lis))
	       (set-cdr! lis (cddr lis))
	         result)
	      (else (helper who (cdr lis)))))
  (helper name banned-list))


(define (setup-request-handler port-from-server)  
  ;;;Handle messages from the server.
  ;
  ;Only handles "receive-msg", "client-list", and "goodbye".
  ;

  
  (define (request-handler)
    (let ((req (get-request port-from-server)))
      (if (not req)
	  (close-connection)
	  (begin
	    (format logging "Received request: ~S~%" req)
	    (cond
	     ((equal? 'receive-msg (request-action req))
          (if (not (member? (request-src req) banned-list))
	          (received-msg (request-src req) (request-data req))
              (im (request-src req) "you are banned, message blocked"))
	     ((equal? 'client-list (request-action req))
	      (update-client-list (request-data req)))
	     ((equal? 'goodbye (request-action req))
	      (close-connection))
	     (else
	      (format #t "Unknown action requested: ~A~%" (request-action req))
	      (close-connection)))) ))
    ;; if there is more data to handle.
    (if (and (not (port-closed? port-from-server))
	     (char-ready? port-from-server))
	(request-handler)))
  ;; Now set up handler
  (when-port-readable port-from-server request-handler))

  ;5 the 3 way handshake is to let the the server know that it can successful send messages to the
  ; client.

  ;3.38
  ;a
  ;Peter Paul Mary 45
  ;Peter Mary Paul 35
  ;Paul Peter Mary 45
  ;Paul Mary Peter 50
  ;Mary Paul Peter 40
  ;Mary Peter Paul 40
  
  ;b If they all get the value of balance at 100:
  ; Mary sets it last to 50
  ; Peter sets it last to 110
  ; Paul sets it last to 80

  ;3.39
;121 set x + 1 -> xx -> set x from xx
;100 xx -> set x + 1 -> set x from xx
;101 xx -> set x from xx -> set x + 1
;Oh from solutions: 11 xx -> calc x + 1 -> set x from xx -> set x from x + 1


;3.40
;1000000: xx -> set xx -> xxx -> set xxx
;1000: xx -> xxx -> set xx -> set xxx
;100: xx -> xxx -> set xxx -> set xx 
; as well as 10000 if the xx is split by the xxx -> set xxx
; as well as 100000 if the xxx is split by the xx -> set xx

;after serialization, only 1000000


;3.41 I agree since if you need to know what your balance is at any moment in a shared account,
;there is the chance that you could parallelize a deposit/witdraw with a balance inquiry and get an
;inconsistance result. Sure you could reget the deposit, but if you only check deposits when withdrawing
; or depositing, you could go for a month with bad info.
;Answer: if you get accounts with bignums then reading the balance could mess things up

;3.42 Ben's way would only cause an issue if the serializer couldn't distinguish between two calls
;to the same procedure. If is can't tell for some reason, then two withdrawls might run concurrently
;and cause bugs. Aswer: no ben's modification is fine.

;3.44 Ben is right again. The exchange program relied on reading the balances which gave way to
;concurrency issues. The transfer simply takes the transfer amount and uses the individual accounts
;serialization call to make the transfer.

;3.46 The first call to the mutex could read the value of the cell and then the other call could
; read the call and then they both think it is free and allow their processes to perform.

;3.48
;If every account has a unique id number, then serializing with the lower number first will prevent
;deadlock. Even if they run simulataneosly, they will still serialize the lower account first and then
; the other account. This will allow the faster one to check out the lower account when making the
; exchange and then check out the other account. The second call won't try to check out the other
;account until it has checked out the first account. And the first call won't release the lower
;account until it is done with the second account. Thus it will be impossible to check out one account
;but not the other, thus preventing deadlock.

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (> (account1 'number) (account2 'number))
           ((serializer1 (serializer2 exchange))
            account1
            account2)
           ((serializer2 (serializer1 exchange))
            account1
            account2))))

(define (make-account-and-serializer balance account-num)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'number) account-num)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

    ;better implementation of account num from solutions
    (define make-account-and-serializer
  (let ((next-account-num 1))
    (lambda (balance)
      (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (let ((balance-serializer (make-serializer))
            (account-num next-account-num))
        (define (dispatch m)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'number) account-num)
                ((eq? m 'serializer) balance-serializer)
                (else (error "Unknown request: MAKE-ACCOUNT" m))))
        (set! next-id-num (+ 1 next-id-num))
        dispatch))))