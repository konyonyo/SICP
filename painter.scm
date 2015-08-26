(use gl)
(use gl.glut)

(define (display)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-begin GL_LINES)
  (gl-color 1.0 0.0 0.0)
  ((square-limit painter-wave 5) (make-frame (make-vect -1.0 -1.0)
					     (make-vect 2.0 0.0)
					     (make-vect 0.0 2.0)))
  (gl-end)
  (gl-flush))

(define (init)
  (gl-clear-color 1.0 1.0 1.0 1.0))

(define (main args)
  (glut-init-window-position 100 100)
  (glut-init-window-size 500 500)
  (glut-init args)
  (glut-init-display-mode GLUT_RGBA)
  (glut-create-window "test")
  (glut-display-func display)
  (init)
  (glut-main-loop)
  0)

(define (draw-line v1 v2)
  (gl-vertex (xcor-vect v1) (ycor-vect v1))
  (gl-vertex (xcor-vect v2) (ycor-vect v2)))

;; vector
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;; frame
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;; vector‚Ìframe‚Ö‚ÌÊ‘œ
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;; segment
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; segment‚ÌƒŠƒXƒg‚ğ•`‚­painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; wave painter
(define painter-wave
  (segments->painter
   (list (make-segment (make-vect 0.3 0.0) (make-vect 0.4 0.5))
	 (make-segment (make-vect 0.4 0.5) (make-vect 0.3 0.6))
	 (make-segment (make-vect 0.3 0.6) (make-vect 0.2 0.4))
	 (make-segment (make-vect 0.2 0.4) (make-vect 0.0 0.7))
	 (make-segment (make-vect 0.0 0.8) (make-vect 0.2 0.7))
	 (make-segment (make-vect 0.2 0.7) (make-vect 0.3 0.8))
	 (make-segment (make-vect 0.3 0.8) (make-vect 0.4 0.8))
	 (make-segment (make-vect 0.4 0.8) (make-vect 0.3 0.9))
	 (make-segment (make-vect 0.3 0.9) (make-vect 0.4 1.0))
	 (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.9))
	 (make-segment (make-vect 0.7 0.9) (make-vect 0.6 0.8))
	 (make-segment (make-vect 0.6 0.8) (make-vect 0.7 0.8))
	 (make-segment (make-vect 0.7 0.8) (make-vect 1.0 0.4))
	 (make-segment (make-vect 1.0 0.3) (make-vect 0.6 0.6))
	 (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0.0))
	 (make-segment (make-vect 0.6 0.0) (make-vect 0.5 0.3))
	 (make-segment (make-vect 0.5 0.3) (make-vect 0.4 0.0))
	 (make-segment (make-vect 0.4 0.95) (make-vect 0.4 0.9))
	 (make-segment (make-vect 0.6 0.95) (make-vect 0.6 0.9))
	 (make-segment (make-vect 0.4 0.85) (make-vect 0.5 0.8))
	 (make-segment (make-vect 0.5 0.8) (make-vect 0.6 0.85)))))

;; painter‚Ì•ÏŠ·
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (identity painter) painter)

(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

;; painter“¯m‚Ì‰‰Z
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-top
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-bottom frame)
	(paint-top frame)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

;; painter‚Ìsplit
(define (up-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (zero? n)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (zero? n)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180  flip-vert)))
    (combine4 (corner-split painter n))))

