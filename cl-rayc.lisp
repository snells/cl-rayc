;;;; cl-rayc.lisp

(in-package #:cepl)

(defvar tile-size 64)
(defvar wall-height 64)
(defvar view-width 200)
(defvar view-height 100)

(defvar pi2 (* pi 2))
(defvar player-x 100)
(defvar player-y 160)
(defvar player-angle 0)
(defvar player-height 32)
(defvar player-speed 16)
(defvar player-fov (/ pi 3)) ; 60 degree
(defvar level) 
(defvar map-width 32)
(defvar map-height 32)

; 0 nothing, 1 red wall, 2 blue wall
(defun make-map ()
  (setf level (make-array `(,map-height ,map-width)))
  (dotimes (y map-height)
    (dotimes (x map-width)
      (setf (aref level y x)
	    (cond ((or (zerop y) (zerop x)
		       (= y (- map-height 1)) (= x (- map-width 1)))
		   1)
		  ((and (zerop (mod x 7))
			(zerop (mod y 7)))
		   3)
 		  ((and (zerop (mod  x 5))
			(zerop (mod y 5)))
		   
		   2)
		  (t 0))))))

(defun get-map (x y)
  (aref level y x))

; get map slot from world x and world y
(defun get-scaled-map (x y)
  (aref level (floor y tile-size) (floor x tile-size)))
  
(defun calc-view-dist ()
  (/ (/ view-width 2) (tan (/ player-fov 2))))



;;;    x+^
;;;      |
;;;   ---|---> y+
;;;      |
(defun angle-to-positive (angle)
  (if (< angle 0)
      (+ angle pi2)
      angle))

(defun angle-upp (angle)
  (setf angle (angle-to-positive angle))
  (if (and (> angle 0)
	   (< angle pi))
      t nil))
	
(defun angle-downp (angle)
  (if (not (angle-upp angle)) t nil))

(defun angle-leftp (angle)
  (setf angle (angle-to-positive angle))
  (if (and (> angle (/ pi 2))
	   (< angle (* pi 1.5)))
      t nil))

(defun angle-rightp (angle)
  (if (not (angle-leftp angle)) t nil))

(defun horizontal-dist (angle v-dist)
  (* (/ 1 (tan angle)) v-dist))

(defun near-zerop (num)
  (if (< (abs num) 0.001) t nil))

(defun near-num (x num)
  (if (< (abs (- num x)) 0.001) t nil))

(defun get-grid (x)
  (* (floor x tile-size) tile-size))

;;; a^2 + b^2 = c^2
(defun calc-dist (x0 y0 x1 y1)
  (sqrt (+ (expt (- y1 y0) 2)
	   (expt (- x1 x0) 2))))

(defun clamp (val min max)
  (cond ((< val min) min)
	((> val max) max)
	(t val)))


;;; get x step and y step

;;;
;;;        |y
;;;      a |
;;;      ---      a = player-angle, x (cos angle), y = (sin angle)
;;;       x
;;; divide distance to next wall by steps 
;;; if distance to x wall is shorter player goes there and delta y = (* y-step count-of-x-steps)
;;; and vice versa
;;;     -------
;;;     |   |
;;;     |---p
;;;     |
;;; so player will always hit
;;; need to figure how to add objects with different hight



(defun cast-ray (angle)
  (if (< angle 0)
      (setf angle (+ angle pi2)))
  (let* ((step-x (cos angle))
	 (step-y (sin angle))
	 (grid-x (get-grid player-x))
	 (grid-y (get-grid player-y))
	 (tmp-x (if (< step-x 0) t nil)) ; t if looking left, nil if looking right
	 (tmp-y (if (< step-y 0) t nil)) ; t if looking down, nil if looking up
	 (next-grid-x (if tmp-x grid-x (+ grid-x tile-size)))
	 (next-grid-y (if tmp-y grid-y (+ grid-y tile-size)))
	 (grid-step-x (if tmp-x (- tile-size) tile-size))
	 (grid-step-y (if tmp-y (- tile-size) tile-size))
	 (ray-x player-x)
	 (ray-y player-y)
	 (side-hit nil) 
	 (low-box nil)) ;experimental for short object

; 0, pi/2, pi and pi*1.5 could be optimized but currently are not
    (if (zerop step-x)
	(setf step-x 0.001))
    (if (zerop step-y)
	(setf step-y 0.001))
    (do ((end nil))
	(end)
      (let ((hit-x (abs (/ (- ray-x next-grid-x) step-x))) ; hit-x = how many x-steps needed to hit next wall
	    (hit-y (abs (/ (- ray-y next-grid-y) step-y)))) ; hit-y = how many y-steps needed to hit next wall
	(cond ((< hit-x hit-y)
	       (setf side-hit t
		     ray-x next-grid-x
		     ray-y (+ ray-y (* step-y hit-x))
		     next-grid-x (+ grid-x grid-step-x))
	       (setf grid-x (if tmp-x next-grid-x ray-x))) ; if ray is moving left the tile we hit is (- current-grid size-of-1-grid)
	      (t
	       (setf side-hit nil
		     ray-x (+ ray-x (* step-x hit-y))
		     ray-y next-grid-y
		     next-grid-y (+ grid-y grid-step-y))
	       (setf grid-y (if tmp-y next-grid-y ray-y)))) ; if ray is moving down the tile we hit is (- current-grid size-of-1-grid) 

	(let ((ret (get-map (truncate (/ grid-x tile-size)) (truncate (/ grid-y tile-size))))) ; value in the map slot that ray hit
	(cond ((or (<= grid-x 0) (>= grid-x (* tile-size map-width))  ; bound checks 
		   (<= grid-y 0) (>= grid-y (* tile-size map-height))
		   (= ret 1) ; for simple one sized object we could test (/= ret 0)
		   (= ret 3)); but this is for experimental testing if object is shorter 
	       (return-from cast-ray (list ray-x ray-y ret side-hit low-box)))
	      ((not (null low-box))) ; so we can't see trough short object if we are close
	      ((= ret 2)
	       (setf low-box (list ray-x ray-y side-hit))))))))) ; ray hit short object and keeps moving 



