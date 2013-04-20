
;;; ===================================================
;;;
;;;  Program to find the price of making a sing
;;;
;;; ===================================================

;;; ===================================================
;;;		class deffinition
;;; ===================================================

(defclass SIGN
     (is-a USER)
     (slot type)
     (slot color)
     (slot size)
     (slot shape)
     (slot material)
     (slot order)
     (slot price)
     (slot printed))
     
(defclass BANNER
	(is-a SIGN)
	(slot hem)
	(slot rope)
	(slot grommet))
	
(defclass HANG_SIGN
	(is-a SIGN)
	(slot thickness)
	(slot indoor_outdoor)
	(slot engraved)
	(slot hanger_type))
	
;;; =====================================================
;;; 	instance declarations
;;; =====================================================

(definstances myInstances
	(sign of SIGN)
	(banner of BANNER)
	(h_sign of HANG_SIGN))
	
;;; =====================================================
;;; 	rules
;;; =====================================================

