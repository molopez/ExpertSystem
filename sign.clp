
;;; ===================================================
;;;
;;;  Program to suggest costumer type of sign he/she 
;;;  should buy for the occasion
;;;
;;; 	Ivory Hernandez and Mario Lopez
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
	(sign of SIGN (hand_painted no))
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
  
;;; if the structure of the sign is rigid then it could be reflective  
(defrule 4_determine_reflective_rigid ""
	?ins<-(object (is-a MATERIAL) (structure rigid))
	=>
	(send [material] put-reflective (yes-or-no "Would you like the sign to be reflective? (yes/no) "))
	;(send [material] print)
	)
    
;;; if not reflective then maybe sign will glow
(defrule 5_determine_glow ""
	?ins<-(object (is-a MATERIAL) (reflective no) (structure rigid))
	=>
	(send [material] put-glow (yes-or-no "Would you like the sign to glow? (yes/no) "))
	;(send [material] print)
	)

;;; if not a glow sign then maybe engraved
(defrule 6_determine_engraved ""
	(object (is-a MATERIAL) (glow no) (structure rigid))
	=>
	(send [sign] put-engraved (yes-or-no "Would you like the sign to be engraved? (yes/no) "))
	;(send [sign] print)
	)
	
;;; if engraved then it may be hand painted
(defrule 7_determine_hand_painted ""
	?ins<-(object (is-a SIGN) (engraved yes))
	=>
	(send ?ins put-hand_painted (yes-or-no "Would you like to have hand painting on the sign? (yes/no) "))
	;(send ?ins print)
	)
    
;;; flexible signs could be stuck to surfaces 
(defrule 8_determine_adhesive ""
	?ins<-(object (is-a MATERIAL) (structure flexible))
	=>
	(send [material] put-adhesive (yes-or-no "Is it going to be directly applied to any surface? (yes/no) "))
	;(send [material] print)
	)
	
;;; if the structure of the sign is flexible then it could be reflective
(defrule 9_determine_reflective ""
	?ins<-(object (is-a MATERIAL) (structure flexible))
	=>
	(send [material] put-reflective (yes-or-no "Would you like the sign to be reflective? (yes/no) "))
	;(send [material] print)
	;(printout t "Rule 16 " crlf)
	)
	
(defrule 10_determine_shape ""
	(not (shape ?))
	=>
	(send [sign] put-shape (ask-question "What shape would you like for your sign? (rectangular/circular/irregular) " 
	rectangular circular irregular))
	;(send [sign] print)
	)

;;; asks if sign should have a color is is not hand painted
(defrule 11_determine_color ""
	(declare (salience -1))
	(object (is-a SIGN) (hand_painted no))
	=>
	(send [sign] put-color (ask-question "What color scheme would you like? (full/multi) " full multi))
	;(send [sign] print)
	)
	
;;; =============================================================
;;; 	set rules for suggestions
;;; =============================================================

;;; suggestion only applies to signs that are not engraved or 
;;; are used for outdoor
(defrule 12_suggest_not_engraved
	(declare (salience -10))
	(object (is-a SIGN) (indoor no) (engraved no))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "1 = " crlf)
	(printout t "Sign:Hanging, Pole or 3D " crlf)
	(printout t "Materials: Wood, Plastic, Metal, Stone " crlf))

;;; suggestion if there is an engraved sign
(defrule 13_suggest_engraved
	(declare (salience -10))
	(object (is-a SIGN) (engraved yes))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "2 = " crlf)
	(printout t "Sign:Hanging, Pole or 3D " crlf)
	(printout t "Materials: Wood or Plastic " crlf))

;;; suggestion for sign that customer want 
;;; them to glow	
(defrule 14_suggest_glow
	(declare (salience -10))
	(object (is-a MATERIAL) (structure rigid) (glow yes))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "3 = " crlf)
	(printout t "Sign:Hanging, Pole or 3D " crlf)
	(printout t "Materials: Neon, Channel Letters or Illuminated Box" crlf))

;;; suggest signs based on reflectivity and rigid structure
(defrule 15_suggest_reflective
	(declare (salience -10))
	(object (is-a MATERIAL) (reflective yes) (structure rigid))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "4 = " crlf)
	(printout t "Sign:Hanging, Pole, 3D " crlf)
	(printout t "Materials: Metal, Plastic or Wood " crlf))

;;; suggestion for a sign that is reflective and flexible
;;; can be used indoor or outdoor
(defrule 16_suggest_reflective
	(declare (salience -10))
	(object (is-a MATERIAL) (reflective yes) (structure flexible))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "4 = " crlf)
	(printout t "Sign:Hanging or Graphic " crlf)
	(printout t "Materials: Adhesive, Vinyl, Cloth or Paper  " crlf))

;;; suggestion made for signs that will be directly
;;; installed on surface	
(defrule 17_suggest_adhesive
	(declare (salience -10))
	(object (is-a MATERIAL) (adhesive yes))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "5 = " crlf)
	(printout t "Sign:Graphic, Floor, Vehicle Graphics " crlf)
	(printout t "Materials: Adhesive, Reflective " crlf))

;;; suggestion only applies to banners	
(defrule 18_suggest_banner
	(declare (salience -10))
	(object (is-a SIGN) (shape rectangular))
	(object (is-a MATERIAL) (structure flexible) (adhesive no))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "6 = " crlf)
	(printout t "Sign:Banner" crlf)
	(printout t "Materials: Vinyl or Cloth  " crlf))

;;; suggest for graphic or floor signs only
(defrule 19_suggest_circular
	(declare (salience -10))
	(object (is-a SIGN) (shape circular) (indoor yes))
	(object (is-a MATERIAL) (structure flexible) (adhesive yes))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "7 = " crlf)
	(printout t "Sign:Graphics or Floor Sign" crlf)
	(printout t "Materials: Adhesive " crlf))

;;; apply only to hanging signs
(defrule 20_suggest_indoor_flex
	(declare (salience -10))
	(object (is-a SIGN) (indoor yes))
	(object (is-a MATERIAL) (structure flexible) (adhesive no))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "8 = " crlf)
	(printout t "Sign:Hanging" crlf)
	(printout t "Materials:Paper, Vinyl or Cloth " crlf))

;;; for used by businesses with flexible signs
(defrule 21_suggest_circular
	(declare (salience -10))
	(object (is-a SIGN) (business yes))
	(object (is-a MATERIAL) (structure flexible) (adhesive no))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "9 = " crlf)
	(printout t "Sign:Hanging" crlf)
	(printout t "Materials:Paper, Vinyl or Cloth " crlf))

;;; for business that want to use signs that might be
;;; glued to cars or other surfaces	
(defrule 22_suggest_circular
	(declare (salience -10))
	(object (is-a SIGN) (business yes))
	(object (is-a MATERIAL) (structure flexible) (adhesive yes))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "10 = " crlf)
	(printout t "Sign:Graphics or Floor Sign" crlf)
	(printout t "Materials: Adhesive " crlf))

;;; indoor signs for businesses	
(defrule 23_suggest_business_indoor
	(declare (salience -10))
	(object (is-a SIGN) (business yes))
	(object (is-a MATERIAL) (structure rigid) (glow no))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "11 = " crlf)
	(printout t "Sign: Hanging, Pole" crlf)
	(printout t "Materials: Metal, Plastic or Wood " crlf))
	
;;; any indoor sign that is not rigid
(defrule 24_suggest_business_indoor
	(declare (salience -10))
	(object (is-a SIGN) (indoor yes) (shape ?shp))
	(object (is-a MATERIAL) (structure flexible))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "12 = " crlf)
	(printout t "Sign: Hanging, Pole, Graphic or Floor Sign " crlf)
	(printout t "Materials: Metal, Plastic or Wood " crlf))

;;; big structured signs for businesses	
(defrule 25_suggest_rigid_indoor
	(declare (salience -10))
	(object (is-a SIGN) (indoor yes) (shape ?shp))
	(object (is-a MATERIAL) (structure rigid) (glow no))
	=>
	(printout t "Suggestions: " crlf)
	;(printout t "13 = " crlf)
	(printout t "Sign: Hanging, Pole or 3D " crlf)
	(printout t "Materials: Metal, Plastic or Wood " crlf))

	

	
	

		
