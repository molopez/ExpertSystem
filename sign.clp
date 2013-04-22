
;;; ===================================================
;;;
;;;  Program to suggest costumer type of sign he/she 
;;;  should buy for the occasion
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
     (slot quantity)
     (slot wearability)
     (slot indoor)
     (slot thickness)
     (slot printed))
     
(defclass BANNER
	(is-a SIGN)
	(slot hem)
	(slot rope)
	(slot grommet))
	
(defclass HANG_SIGN
	(is-a SIGN)
	(slot hand_painted)
	(slot engraved))
	
;;; =====================================================
;;; 	instance declarations
;;; =====================================================

(definstances myInstances
	(banner of BANNER)
	(hanging of HANG_SIGN))
	
;;; =====================================================
;;; 	functions to get input from user
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

;;; asking to see if it is a floor sign
;;; if is not defined then ask the question
(defrule determine_sign_type ""
	(declare (salience 10))
	(not (type ?))
	=>
	(assert (type (ask-question "What type of sign (banner/hanging)? " banner hanging)))) 
	
(defrule set_sign_type ""
	(type banner)
	=>
	(send [banner] put-type banner)
	(send [banner] print)
	(printout t crlf))

(defrule set_sign_hanging ""
	(type hanging)
	=>
	(send [hanging] put-type hanging)
	(send [hanging] print)
	(printout t crlf))
	
(defrule print_sign_hanging ""
	?ins <- (object (is-a SIGN) (type hanging))
	=>
	(send ?ins put-color blue)
	(send ?ins print)
	(printout t crlf))
	
(defrule print_sign_banner ""
	?ins <- (object (is-a SIGN) (type banner))
	=>
	(send ?ins put-color red)
	(send ?ins print)
	(printout t crlf))
	
;;; determine used indoor or outdoor	
(defrule determine_indoor ""
	(declare (salience 10))
	(not (indoor ?))
	=>
	(assert (indoor (yes-or-no "Will this sign be used inside (yes/no)? "))))
	
;;; if it is a floor sign 	
;;; send it to the instance [sing] and fill in the type slot
;;; print the instance information
(defrule set-indoor ""
	(declare (salience 12))
	(indoor ?test)
	=>
	(printout t "This is a sign for the floor")
	(send [banner] put-indoor ?test)
	(send [banner] print)
	(printout t crlf))

;;; pass an object of type SIGN to ?ins with type floor
;;; print the object
(defrule sign_is_floor
	?ins <- (object (is-a SIGN) (color full))
	=>
	(printout t "this is an instance sign" crlf)
	(send ?ins print)
	(printout t crlf))
	
;;; test choosing an object of type SIGN and some arbitrary type
;;; print the type of the object
;(defrule sign_type
;	(object (is-a SIGN) (type ?tp))
;	=>
;	(printout t "The chosen type is " ?tp crlf))
	
;;; testing passing in more info from user
(defrule determine_banner_length ""
	(not (banner_length ?))
	=>
	(assert (banner_length (ask-question "Enter the length of the banner (1/2/3)? " 1 2 3)))) 
	
	
;;; =============================================================
;;; 	set rules for suggestions
;;; =============================================================
