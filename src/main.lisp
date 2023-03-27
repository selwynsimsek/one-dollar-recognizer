(defpackage one-dollar-recognizer
  (:use :cl))
(in-package :one-dollar-recognizer)

(defun x (a) (aref a 0))
(defun y (a) (aref a 1))

;;; step 1
(defun resample (points n)
  (let* ((d 0)
         (big-i (/ (path-length points)
               (1- n)))
         (new-points (make-array '(1) :adjustable t :initial-element (aref points 0))))
    (loop for i from 1 below (length points) do
          (let ((small-d (distance (aref points (1- i))
                                   (aref points i))))
            (if (<= big-i (+ d small-d))
                (let ((q (vector (+ (x (aref points (1- i)))
                                    (* (/ (- big-i d)
                                          small-d)
                                       (- (x (aref points i))
                                          (x (aref points (1- i))))))
                                 (+ (y (aref points (1- i)))
                                    (* (/ (- big-i d)
                                          small-d)
                                       (- (y (aref points i))
                                          (y (aref points (1- i)))))))))
                  (vector-push-extend q new-points)
                  (setf (aref points i) q
                        d 0))
                (incf d small-d))))
    new-points))

(defun path-length (a)
  (loop for i from 1 below (length a) collect (distance (aref a (1- i)) (aref a i))))

(defun distance (a b)
  (sqrt (+ (expt (- (x a) (x b)) 2)
           (expt (- (y a) (y b)) 2))))

;;; step 2

(defun rotate-to-zero (points)
  (let* ((c (centroid points))
         (theta (atan (- (y c)
                         (y (aref points 0)))
                      (- (x c)
                         (x (aref points 0))))))
    (rotate-by points theta)))

(defun rotate-by (points theta)
  (let ((c (centroid points)))
    (map 'vector (lambda (p)
                   (vector (+ (* (cos theta) (- (x p) (x c)))
                              (- (* (sin theta) (- (y p) (y c))))
                              (x c))
                           (+ (* (sin theta) (- (x p) (x c)))
                              (* (cos theta) (- (y p) (y c)))
                              (y c))))
         points)))

(defun centroid (points)
  (loop for p across points
        summing (x p) into x-sum
        summing (y p) into y-sum
        finally (return (vector (/ x-sum (length points))
                                (/ y-sum (length points))))))

;;; step 3

(defun bounding-box (points)
  (loop for p across points
        maximizing (x p) into x-max
        maximizing (y p) into y-max
        minimizing (x p) into x-min
        minimizing (y p) into y-min
        finally (return (values (- x-max x-min) (- y-max y-min)))))

(defun scale-to-square (points size)
  (multiple-value-bind (b-width b-height) (bounding-box points)
    (map 'vector
         (lambda (p)
           (vector (* (/ size b-width) (x p))
                   (* (/ size b-height) (y p))))
         points)))

(defun translate-to-origin (points)
  (let ((c (centroid points)))
    (map 'vector
         (lambda (p)
           (vector (- (x p) (x c))
                   (- (y p) (y c))))
         points)))

;;; step 4

(defun recognize (points templates)
  (let ((b most-positive-long-float)
        (t-prime))
    (loop for template in templates do
          (let ((d (distance-at-best-angle points template)))
            (when (< d b)
              (setf b d
                    t-prime template))))
    (values t-prime (- 1 (/ b (* 1/2 (sqrt (* 2 size size))))))))

(defun distance-at-best-angle (points template &key (theta-a (/ pi -2))
                                                 (theta-b (/ pi 2))
                                                 (theta-delta (/ pi 90)))
  (let* ((phi (* 1/2 (1- (sqrt 5))))
         (x-1 (alexandria:lerp phi theta-b theta-a))
         (f-1 (distance-at-angle points template x-1))
         (x-2 (alexandria:lerp phi theta-a theta-b))
         (f-2 (distance-at-angle points template x-2)))
    (loop while (< theta-delta (abs (- theta-b theta-a)))
          do (if (< f-1 f-2)
                 (setf theta-b x-2
                       x-2 x-1
                       f-2 f-1
                       x-1 (alexandria:lerp phi theta-b theta-a)
                       f-1 (distance-at-angle points template x-1))
                 (setf theta-a x-1
                       x-1 x-2
                       f-1 f-2
                       x-2 (alexandria:lerp phi theta-a theta-b)
                       f-2 (distance-at-angle points template x-2))))
    (min f-1 f-2)))

(defun distance-at-angle (points template theta)
  (path-distance (rotate-by points theta) template))

(defun path-distance (a b)
  (loop for i from 0 below (length a)
        summing (distance (aref a i) (aref b i)) into d
        finally (return (/ d (length a)))))
