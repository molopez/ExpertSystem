
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
     (slot business)
     (slot indoor))
     
(defclass MATERIAL
    (is-a USER)
    (slot thickness)
    (slot size)
    (slot structure)
    (slot reflective)
    (slot glow)
    (slot adhesive)
    (slot type))
	
;;; =====================================================
;;; 	instance declarations
;;; =====================================================

(definstances myInstances
	(sign of SIGN)
	(material of MATERIAL))
	;(metal of MATERIAL)
	;(wood of MATERIAL)
	;(plastic of MATERIAL))
	
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
       	
;;; =============================================================
;;; 	Questions to user
;;; ============================================================= 
	
(defrule 1_determine_indoor ""
	(not (indoor ?))
	=>
	(send [sign] put-indoor (yes-or-no "Will the sign be used indoor? (yes/no) "))
	;(send [sign] print)
	)
	
(defrule 2_determine_business ""
	(not (business ?))
	=>
	(send [sign] put-business (yes-or-no "Will the sign be used for a business? (yes/no) "))
	;(send [sign] print)
	)
		
(defrule 3_determine_material_structure ""
	(not (structure ?))
	=>
	(send [material] put-structure (ask-question "What type of material structure do you want? (rigid/flexible) " rigid flexible))
	;(send [material] print)
	)
    
(defrule 4_determine_reflective ""
	?ins<-(object (is-a MATERIAL) (structure rigid))
	=>
	(send [material] put-reflective (yes-or-no "Would you like the sign to be reflective? (yes/no) "))
	;(send [material] print)
	)
    
(defrule 5_determine_glow ""
	?ins<-(object (is-a MATERIAL) (reflective no))
	=>
	(send [material] put-glow (yes-or-no "Would you like the sign to glow? (yes/no) "))
	;(send [material] print)
	)
	
(defrule 6_determine_engraved ""
	?ins<-(object (is-a MATERIAL) (glow no))
	=>
	(send [sign] put-engraved (yes-or-no "Would you like the sign to be engraved? (yes/no) "))
	;(send [sign] print)
	)
	
(defrule 7_determine_hand_painted ""
	?ins<-(object (is-a SIGN) (engraved yes))
	=>
	(send ?ins put-hand_painted (yes-or-no "Would you like to have hand painting on the sign? (yes/no) "))
	;(send ?ins print)
	)
    
(defrule 8_determine_adhesive ""
	?ins<-(object (is-a MATERIAL) (structure flexible))
	=>
	(send [material] put-adhesive (yes-or-no "Is it going to be directly applied to any surface? (yes/no) "))
	;(send [material] print)
	)
	
(defrule 9_determine_shape ""
	(not (shape ?))
	=>
	(send [sign] put-shape (ask-question "What shape would you like for your sign? (rectangular/circular/irregular) " 
	rectangular circular irregular))
	;(send [sign] print)
	)
	
(defrule 10_determine_color ""
	(not (color ?))
	=>
	(send [sign] put-color (ask-question "What color scheme would you like? (full/multi) " full multi))
	;(send [sign] print)
	)
	
;;; =============================================================
;;; 	set rules for suggestions
;;; =============================================================

(defrule 11_suggest_banner
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
		
