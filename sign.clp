
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
;;; 	functions
;;; 	taken from the auto.clp example
;;; =====================================================

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))
       
;;; =======================================================
;;; 	ask questions and set values
;;; =======================================================

; asking to see if it is a floor sign
; if is not defined then ask the question
(defrule determine_floor_sign ""
	(declare (salience 10))
	(not (floor_sign ?))
	=>
	(assert (floor_sign (yes-or-no "Will this sign be put on the floor (yes/no)? ")))) 

; same as above for sing type	
(defrule determine_sign_type ""
	(declare (salience 10))
	(not (sign_type ?))
	=>
	(assert (sign_type (ask-question "What is the sign type (banner/hanging)? " banner hanging))))
	
; if it is a floor sign 	
; send it to the instance [sing] and fill in the type slot
; print the instance information
(defrule determine_floor ""
	(declare (salience 12))
	(floor_sign yes)
	=>
	(printout t "This is a sign for the floor")
	(send [sign] put-type floor)
	(send [sign] print)
	(printout t crlf))

; pass an object of type SIGN to ?ins with type floor
; print the object
(defrule sign_is_floor
	?ins <- (object (is-a SIGN) (type floor))
	=>
	(printout t "this is an instance sign" crlf)
	(send ?ins print)
	(printout t crlf))
	
; test choosing an object of type SIGN and some arbitrary type
; print the type of the object
(defrule sign_type
	(object (is-a SIGN) (type ?tp))
	=>
	(printout t "The chosen type is " ?tp crlf))
	
; testing passing in more info from user
(defrule determine_banner_length ""
	(not (banner_length ?))
	=>
	(assert (banner_length (ask-question "Enter the length of the banner (1/2/3)? " 1 2 3)))) 
