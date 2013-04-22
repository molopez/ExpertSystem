
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
     (slot shape)
     (slot wearability)
     (slot engraved)
     (slot hand_painted)
     (slot indoor))
     
(defclass MATERIAL
	(is-a USER)
	(slot thickness)
	(slot size)
	(slot structure)
	(slot type))
	
;;; =====================================================
;;; 	instance declarations
;;; =====================================================

(definstances myInstances
	(sign of SIGN)
	(material of MATERIAL))
	
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
;(defrule determine_sign_type ""
;	(declare (salience 10))
;	(not (type ?))
;	=>
;	(assert (type (ask-question "What type of sign (banner/hanging)? " banner hanging)))) 
	
;(defrule set_sign_type ""
;	(type banner)
;	=>
;	(send [banner] put-type banner)
;	(send [banner] print)
;	(printout t crlf))

;(defrule set_sign_hanging ""
;	(type hanging)
;	=>
;	(send [hanging] put-type hanging)
;	(send [hanging] print)
;	(printout t crlf))
	
;(defrule print_sign_hanging ""
;	?ins <- (object (is-a SIGN) (type hanging))
;	=>
;	(send ?ins put-color blue)
;	(send ?ins print)
;	(printout t crlf))
	
;(defrule print_sign_banner ""
;	?ins <- (object (is-a SIGN) (type banner))
;	=>
;	(send ?ins put-color red)
;	(send ?ins print)
;	(printout t crlf))
	
;;; determine used indoor or outdoor	
;(defrule determine_indoor ""
;	(declare (salience 10))
;	(not (indoor ?))
;	=>
;	(assert (indoor (yes-or-no "Will this sign be used inside (yes/no)? "))))
	
;;; if it is a floor sign 	
;;; send it to the instance [sing] and fill in the type slot
;;; print the instance information
;(defrule set-indoor ""
;	(declare (salience 12))
;	(indoor ?test)
;	=>
;	(printout t "This is a sign for the floor")
;	(send [banner] put-indoor ?test)
;	(send [banner] print)
;	(printout t crlf))

;;; pass an object of type SIGN to ?ins with type floor
;;; print the object
;(defrule sign_is_floor
;	?ins <- (object (is-a SIGN) (color full))
;	=>
;	(printout t "this is an instance sign" crlf)
;	(send ?ins print)
;	(printout t crlf))
	
;;; test choosing an object of type SIGN and some arbitrary type
;;; print the type of the object
;(defrule sign_type
;	(object (is-a SIGN) (type ?tp))
;	=>
;	(printout t "The chosen type is " ?tp crlf))
	
;;; testing passing in more info from user
;(defrule determine_banner_length ""
;	(not (banner_length ?))
;	=>
;	(assert (banner_length (ask-question "Enter the length of the banner (1/2/3)? " 1 2 3))))
	
;;; =============================================================
;;; 	Questions to user
;;; ============================================================= 
	
(defrule determine_indoor ""
	(not (indoor ?))
	=>
	(send [sign] put-indoor (yes-or-no "Will the sign be used indoor? (yes/no) "))
	(send [sign] print)
	)
	
(defrule determine_material_structure ""
	(not (structure ?))
	=>
	(send [material] put-structure (ask-question "What type of material structure do you want? (rigid/flexible) " rigid flexible))
	(send [material] print)
	)
	
(defrule determine_engraved ""
	?ins<-(object (is-a MATERIAL) (structure rigid))
	=>
	(send [sign] put-engraved (yes-or-no "Would you like the sign to be engraved? (yes/no) "))
	(send [sign] print)
	)
	
(defrule determine_hand_painted ""
	?ins<-(object (is-a SIGN) (engraved yes))
	=>
	(send ?ins put-hand_painted (yes-or-no "Would you like to have hand painting on the sign? (yes/no) "))
	(send ?ins print)
	)
	
(defrule determine_shape ""
	?ins<-(object (is-a MATERIAL) (structure flexible))
	=>
	(send [sign] put-shape (ask-question "What shape would you like for your sign? (rectangular/circular/irregular) " 
	rectangular circular irregular))
	(send [sign] print)
	)
	
(defrule determine_color ""
	(not (color ?))
	=>
	(send [sign] put-color (ask-question "What color scheme would you like? (full/multi) " full multi))
	(send [sign] print)
	)
	
;;; =============================================================
;;; 	set rules for suggestions
;;; =============================================================

(defrule suggest_banner
	(declare (salience -10))
	?ins<-(object (is-a SIGN) (shape rectangular))
	=>
	(printout t "You should use a sign of type " crlf)
	(printout t "Banner or Hanging sign " crlf))
	
(defrule suggest_material 
	(declare (salience -10))
	?ins<-(object (is-a SIGN) (engraved yes))
	=>
	(printout t "material: ")
	(printout t "Wood " crlf))
		
