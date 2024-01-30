

(define-class (person name)
  (instance-vars (last-said '()))
  (method (say stuff)
          (set! last-said stuff)
          stuff)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (repeat) (ask self 'say last-said))
  (method (greet) (ask self 'say (se '(hello my name is) name))) )]

(define-class (random-generator number)
  (instance-vars (count 0))
  (method (number)
    (set! count (+ count 1))
    (random number))
  (method (count) ;unneed since we automatically get access to count in this implementation of OOP
    count))

(define-class (coke-machine capacity price)
  (instance-vars (inventory 0) (cash-inside 0)(change 0))
  (method (deposit money)
    (set! cash-inside (+ cash-inside money))
    (set! change 0))
  (method (fill cans)
    (if (<= (+ inventory cans) capacity)
           (set! inventory (+ inventory cans))
          "Too many cans"))
  (method (coke)
    (cond ((and (>= cash-inside price) (< 0 inventory))
           (set! inventory (- inventory 1))
           (set! change (- cash-inside price))
           (set! cash-inside 0)
           change)
          ((< cash-inside price)
           "Not enough money")
          ((= 0 inventory)
           "Machine empty")
          (else "hmmmm"))))


(define ordered-deck '(AH 2H)); 3H 4H 5H 6H 7H 8H 9H 10H JH QH KH AD 2D 3D 4D 5D 6D 7D 8D 9D 10D JD QD KD AS 2S 3S 4S 5S 6S 7S 8S 9S 10S JS QS KS AC 2C 3C 4C 5C 6C 7C 8C 9C 10C JC QC KC))

(define-class (deck)
  (instance-vars (current-deck (shuffle ordered-deck)) (top-card '()))
  (method (deal)
          (cond ((empty? current-deck)
                 '())
                (else
                 (set! top-card (car current-deck))
                 (set! current-deck (cdr current-deck))
                 top-card)))
  (method (empty?)
    (empty? current-deck)))

(define-class (miss-manners name)
  (instance-vars (unessessary '())) ;needed to avoid an error
  (method (please message) (ask name message)))