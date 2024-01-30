#lang simply-scheme

;2.44
#| (define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
           (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
           (beside painter (below smaller smaller))))) |#

;2.45
(define (split t1 t2))
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split t1 t2) painter (- n 1))))
             (t1 painter (t2 smaller smaller)))))

;2.46
(define (make-vect x y)
  (list x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cadr vect))
  
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

(define (make-frameb origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frameb frame)
  (car frame))
(define (edge1-frameb frame)
  (cadr frame))
(define (edge2-frameb frame)
  (cddr frame))

;2.48
(define (make-segment v1 v2)
  (list v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
   
(define (segments->painter segment-list)
  (lambda (frame)
          (for-each
            (lambda (segment)
                    (draw-line
                      ((frame-coord-map frame)
                       (start-segment segment))
                      ((frame-coord-map frame)
                       (end-segment segment))))
            segment-list)))

;2.49
(define (draw-frame-outline frame)
  (let ((edge1 (make-segment (origin-frame frame)
                             (add-vect (origin-frame frame) (edge1-frame frame))))
        (edge2 (make-segment (origin-frame frame)
                             (add-vect (origin-frame frame) (edge2-frame frame))))
        (edge3 (make-segment (add-vect (origin-frame frame) (edge1-frame frame))
                             (add-vect (add-vect (origin-frame frame) (edge1-frame frame))
                                       (edge2-frame frame))))
        (edge4 (make-segment (add-vect (origin-frame frame) (edge2-frame frame))
                             (add-vect (add-vect (origin-frame frame) (edge1-frame frame))
                                       (edge2-frame frame))))
        (frame-segment-list (list  edge1 edge2 edge3 edge4)))
       (segments->painter frame-segment-list)))

(define (draw-frame-x frame)
  (let ((edge1 (make-segment (origin-frame frame)
                             (add-vect (add-vect (origin-frame frame) (edge1-frame frame))
                                       (edge2-frame frame))))
        (edge2 (make-segment (add-vect (origin-frame frame) (edge1-frame frame))
                             (add-vect (origin-frame frame) (edge2-frame frame))))
        (x-segment-list (list  edge1 edge2)))
       (segments->painter x-segment-list)))

(define (draw-frame-diamond frame)
  (let ((edge1 (make-segment (add-vect (origin-frame frame)
                                       (scale-vect (edge1-frame frame) 0.5))
                             (add-vect (origin-frame frame)
                                       (scale-vect (edge2-frame frame) 0.5))))
        (edge2 (make-segment (add-vect (origin-frame frame)
                                       (scale-vect (edge1-frame frame) 0.5))
                             (add-vect (add-vect (origin-frame frame) (edge1-frame frame))
                                       (scale-vect (edge2-frame frame) 2))))
        (edge3 (make-segment (add-vect (origin-frame frame)
                                       (scale-vect (edge2-frame frame) 0.5))
                             (add-vect (add-vect (origin-frame frame) (edge2-frame frame))
                                       (scale-vect (edge1-frame frame) 2))))
        (edge3 (make-segment (add-vect (add-vect (origin-frame frame) (edge1-frame frame))
                                       (scale-vect (edge2-frame frame) 2))
                             (add-vect (add-vect (origin-frame frame) (edge2-frame frame))
                                       (scale-vect (edge1-frame frame) 2))))
        (diamond-segment-list (list  edge1 edge2 edge3 edge4)))
       (segments->painter diamond-segment-list)))

(define (wave-painter frame)
  (segments->painter (wave-segments-list)))



(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
           (let ((top-left (beside up up))
                 (bottom-right (below right right))
                 (corner (corner-split painter (- n 1))))
                (beside (below painter top-left)
                (below bottom-right corner))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
          (let ((m (frame-coord-map frame)))
               (let ((new-origin (m origin)))
                    (painter (make-frame
                               new-origin
                               (sub-vect (m corner1) new-origin)
                               (sub-vect (m corner2) new-origin)))))))

;2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0) ; new origin
                     (make-vect 0.0 0.0) ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) ; new origin
                     (make-vect 1.0 1.0) ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)))

;2.51
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
       (let ((paint-left
               (transform-painter
                 painter1
                 (make-vect 0.0 0.0)
                 split-point
                 (make-vect 0.0 1.0)))
             (paint-right
               (transform-painter
                 painter2
                 split-point
                 (make-vect 1.0 0.0)
                 (make-vect 0.5 1.0))))
            (lambda (frame)
              (paint-left frame)
              (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
       (let ((paint-up
               (transform-painter
                 painter1
                 split-point
                 (make-vect 1.0 0.5)
                 (make-vect 0.0 1.0)))
             (paint-down
               (transform-painter
                 painter2
                 (make-vect 0.0 0.0)
                 (make-vect 0.5 1.0)
                 split-point)))
            (lambda (frame)
              (paint-up frame)
              (paint-down frame)))))

(define (belowb painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))





(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
           (beside painter (below smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
           (let ((top-left (beside up up))
                 (bottom-right (below right right))
                 (corner (corner-split painter (- n 1))))
                (beside (below painter top-left)
                        (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
       (let ((half (beside (flip-horiz quarter) quarter)))
            (below (flip-vert half) half))))