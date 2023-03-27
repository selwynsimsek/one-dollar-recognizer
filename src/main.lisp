(defpackage one-dollar-recognizer
  (:use :cl))
(in-package :one-dollar-recognizer)

;;; step 1
(defun resample (points))
(defun path-length (a))

;;; step 2
(defun rotate-to-zero (points))
(defun rotate-by (points theta))

;;; step 3

(defun scale-to-square (points size))
(defun translate-to-origin (points))

;;; step 4

(defun recognize (points templates))
(defun distance-at-best-angle (points template theta-a theta-b theta-delta))
(defun distance-at-angle (points template theta))
(defun path-distance (a b))
