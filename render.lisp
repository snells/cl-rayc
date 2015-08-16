(in-package :cepl)

(defvar shapes '(:rect nil))


;I think this is not needed
(defstruct-g pos-obj ()
  (position :vec3 :accessor pos))

(defun-g vert ((vert pos-obj) &uniform (color :vec3) (to-world :mat4))
  (values (* to-world (v! (pos vert) 1.0))
	  (v! color 1.0)))

(defun-g frag ((color :vec4))
  color)

(defpipeline render-program ()
    (g-> #'vert #'frag))


;;; to-world matrice transforms position 2d space where viewport height = 100 and width = 200
;;;
;;;    ^|
;;;    y|
;;;     ----- x>
;;;    0
;;; the position is originally inside rectangle (-1, -1) (1, 1) and it's width is 0.01, heigh 0.02
(defun draw-rect (x y w h &optional (color (v! 1 1 1)))
  (map-g #'render-program (getf shapes :rect)
	 :color color
	 :to-world (reduce #'m4:m* (list (m4:translation (v! (+ (/ x 100) -0.995
								(/ w 200))
							     (+ (/ y 50) -0.99
								(/ h 100))
							     0))
					 (m4:scale (v! w h 1))))))

(defun choosecolor (val)
  (if (zerop val)
      (setf val 1))
  (case val
    (1 (v! 1 0 0))
    (2 (v! 1 0 1))
    (3 (v! 0 1 1))))

(defun darken (c)
  (setf (aref c 0) (/ (aref c 0) 2)
	(aref c 1) (/ (aref c 1) 2)
	(aref c 2) (/ (aref c 2) 2)))


(defun draw-ray (x pos-x pos-y h c)
  (let* ((dist (calc-dist player-x player-y pos-x pos-y)) ; distance in game units
	 (line-h (/ h (/ dist 64))) ; needs proper way to do this
	 ;; bad hack to get object of height 32 to draw on the ground instead of middle of the screen
	 (start-h (clamp (+ (if (= h 64) (/ (- line-h) 2) (- line-h))
			    (/ view-height 2))
			 0 view-height)))
    (draw-rect x start-h 1 line-h c)))

(defun game-loop ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
    (let ((angle (- player-angle (/ player-fov 2))))
  (dotimes (i view-width)
    (let ((p (cast-ray angle)))
      (incf angle (/ player-fov view-width))
      (multiple-value-bind (x y val side low-box) (apply #'values p)
	(if low-box
	    (let ((c (choosecolor 2)))
	      (if (caddr low-box) (darken c))
	      (draw-ray i (car low-box) (cadr low-box) 32 c)))
	(let ((c (choosecolor val)))
	  (if side (darken c))
	  (draw-ray i x y 64 c)))))
	
  (draw-rect 0 0 200 50 (v! 0 1 0)) ; ground
  (draw-rect 0 50 200 50 (v! 0 0 1)) ; sky
  (move-player)
  (update-display)))


(defun move-player ()
  (let ((new-x (+ player-x (* player-speed (cos player-angle))))
	(new-y (+ player-y (* player-speed (sin player-angle)))))
    (if (zerop (get-scaled-map new-x new-y))
	(setf player-x new-x
	      player-y new-y))))



;;; I couldn't get it to draw rectangle by just having verts of dimension 4
;;; so we create rectangle using 2 triangle
;;; those color are never used and I don't why it runs when they are defined
;;; because element-type is pos-obj and it has only one slot whict is type vec3
;;; Maybe because the position and color are in same list?
;;;
;;; this draws rectangle with width 0.01 and heigh 0.02 centered at origin 0,0
;;; we use to-world uniform to place and scale it properly
(let ((running nil))
  (defun run-loop ()
    (make-map)
    (setf running t)
    (let* ((verts (make-gpu-array (list (list (v!  -0.005 -0.01 0) (v! 0 1 0 1))
					(list (v!    -0.005 0.01 0) (v! 1 0 0 1))
					(list (v! 0.005 0.01 0) (v! 0 0 1 1))
					(list (v!  0.005 -0.01 0) (v! 1 0 0 1)))
				  :element-type 'pos-obj :dimensions 4))
	   (ind (make-gpu-array '(2 1 0   0 3 2) :element-type :unsigned-short :dimensions 6)))
      (setf (getf shapes :rect) (make-buffer-stream verts :index-array ind))
      (loop :while running :do (continuable (game-loop)))))
  
  (defun stop-loop ()
    (setf running nil)))






(evt:def-event-listener sys-listener (e :sys)
  (when (typep e 'evt:will-quit) (stop-loop)))

;;; cepl:reshape is not defined?
(evt:def-event-listener window-listener (e :window)
  (when (eq (evt:action e) :resized)))
    ;(reshape (evt:data e))))


;;; if you press key and keep pressing, it will register the first key press
;;; after delay it will register that it is being kept pressed
;;; How to shorten that delay?
(evt:def-event-listener keyboard-listener (e :keyboard)
  (cond ((eq (evt:key-state :w) :down)
	 (setf player-speed 10))
	((eq (evt:key-state :s) :down)
	 (setf player-speed -10))
	(t (setf player-speed 0)))
  (if (eq (evt:key-state :d) :down)
      (incf player-angle 0.05))
  (if (eq (evt:key-state :a) :down)
      (decf player-angle 0.05)))
	   


  

