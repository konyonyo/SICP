;; ��̃E�B���h�E���J��
(use gl)
(use gl.glut)

(define (display))

(define (main args)
  (glut-init args)
  (glut-create-window "test")
  (glut-display-func display)
  (glut-main-loop)
  0)
