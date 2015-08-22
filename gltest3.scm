;; ü‚ğˆø‚­
(use gl)
(use gl.glut)

(define (display)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-begin GL_LINE_LOOP)
  (gl-vertex -0.9 -0.9)
  (gl-vertex 0.9 -0.9)
  (gl-vertex 0.9 0.9)
  (gl-vertex -0.9 0.9)
  (gl-end)
  (gl-flush))


(define (init)
  (gl-clear-color 0.0 0.0 1.0 1.0))
    

(define (main args)
  (glut-init args)
  (glut-init-display-mode GLUT_RGBA)
  (glut-create-window "test")
  (glut-display-func display)
  (init)
  (glut-main-loop)
  0)
