;crossings.LSP done by  IBRAHIM EL SHARR DATE: 09-06-2015,FOR PROFILE CROSSINGS WITH V-SCALE 1:100 AND H-SCALE 1:500
;-------------------------------------------------------------------------------------------------------------------

(defun c:CRP7 (/  q j c p d xp0 yp0 xp1 yp1 xp2 yp2  xp3 yp3 xp4 yp4 xp5 yp5 r L)
(SETVAR "OSMODE" 16383)
(vl-load-com)
(setq flag 0)
(setq pflag 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
(*activeDoc*)
((setq *activeDoc* (vla-get-activedocument (vlax-get-acad-object)))))
(prompt "\n >> Select Ground Level Polyline: ")
(if (setq ss (ssget ":S:E" '((0 . "*POLYLINE"))))
(progn
(vlax-for x (setq ss (vla-get-activeselectionset *activeDoc*))
(setq coords
(vlax-safearray->list
(vlax-variant-value
(vla-get-coordinates x)))))
(vla-delete ss)))
(setq n 0)                                               ;set up counter
;while not at the end of the list do...
(while (not (equal (setq item (nth n coords)) nil))
;(setq n (1+ n) ) 
(setq n (+ n 1))   
                                  ;add 1 t counter
   );end WHILE
   (princ n)
   (princ coords)   
   (setq datumpt (getpoint "\nPick a point on the profile datum level: "))
   (setq datum (getreal "\nEnter datum Level: "))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(SETVAR "OSMODE" 16383)
   (COMMAND   "STYLE" "RS2-"  "ROMANS" "2.25" "" "" "" "" "" )
   (COMMAND "LAYER" "M" "C-PRFL-CROSS-TEXT" "C" "7"   "" "L" "CONTINUOUS" "" "")
   (COMMAND "LAYER" "M" "C-PRFL-CROSS-MLDR" "C" "7"   "" "L" "CONTINUOUS" "" "")
   (COMMAND "LAYER" "M" "C-PRFL-CROSS-WET" "C" "7"  "" "L" "CONTINUOUS" "" "")
   (COMMAND "LAYER" "M" "C-PRFL-CROSS-DRY"   "C" "7" "" "L" "CONTINUOUS" "" "")
   (COMMAND "LAYER" "M" "C-PRFL-CROSS-BEND"   "C" "7" "" "L" "CONTINUOUS" "" "")
(setq VF 10)
(setq XF 2)
(setq q (getpoint "
Pick starting Point on the Profile : "))
(setq qq q)
(setq j (getpoint "
Pick starting Point on the Utility : "))
(while ;start while
  (SETVAR "OSMODE" 16383)
  (setvar "LTSCALE" 1000)
  (vla-Regen (vla-get-activedocument (vlax-get-acad-object)) acActiveViewport)
  (setq c (getpoint "\nPick next crossing point/bend : "))
  (setvar "LTSCALE" 1)
  (vla-Regen (vla-get-activedocument (vlax-get-acad-object)) acActiveViewport)
  ;(setq r (getreal "Enter crossing Diameter or enter 0 if bend or -1 for Dry Crossing Utility : "))
       (setq Z1 0)
	   (setq Z2 0)
	   (setq Z3 0)
	   (setq UT1 "UU")
	   (setq UT2 "UU")
	   (setq UT3 0)
	   (while (< Z1 1)
	   (if (= flag 1) (progn
	   (setq UT1 (getstring "\nEnter B for Bend/Manhole, E for Existing, T for Temporary, R for Relocated, P if this crossing is the same as the previous one: " ))
	   (IF (OR (= UT1 "E") (= UT1 "T") (= UT1 "R") (= UT1 "e") (= UT1 "t") (= UT1 "r") (= UT1 "b") (= UT1 "B")) (setq Z1 2))
	   (if (OR (= UT1 "P") (= UT1 "p")) (PROGN (setq pflag 1)(setq UT1 UTP1) (setq UT2 UTP2) (setq UT3 UTP3) (setq r rp) (setq OD ODP) (setq Z1 2) (setq Z2 2) (setq Z3 2)))
	   ))
	   (if (= flag 0) (progn
	   (setq UT1 (getstring "\nEnter B for Bend/Manhole, E for Existing, T for Temporary, R for Relocated: " ))
	   (IF (OR (= UT1 "E") (= UT1 "T") (= UT1 "R") (= UT1 "e") (= UT1 "t") (= UT1 "r") (= UT1 "b") (= UT1 "B")) (setq Z1 2))
	   ))
	   )(IF (= UT1 "e") (setq UT1 "E")) (IF (= UT1 "t") (setq UT1 "T")) (IF (= UT1 "r") (setq UT1 "R")) (IF (or (= UT1 "b") (= UT1 "B")) (progn (setq Z2 2) (setq Z3 2)))
	   (while (< Z2 1)
	   (if (= UT1 "E") (setq UT2 (getstring "\nEnter Utility: W, IRR, SW, SE, 13.8KV, 33KV, 132KV, STC, ITCMLY, MODA, DISUSED-CABLE: " ))
	                   (setq UT2 (getstring "\nEnter Utility: W, IRR, SW, SE, 13.8KV, 33KV, 132KV, STC, ITCMLY, MODA: " )))
	   (if (= UT1 "E")			   
	   (IF (OR (= UT2 "W") (= UT2 "SW") (= UT2 "SE") (= UT2 "w") (= UT2 "se") (= UT2 "sw") (= UT2 "IRR") (= UT2 "irr") (= UT2 "13.8KV") (= UT2 "13.8kv") (= UT2 "33KV") (= UT2 "33kv") (= UT2 "132KV") (= UT2 "132kv") (= UT2 "STC") (= UT2 "stc") (= UT2 "ITCMLY") (= UT2 "itcmly") (= UT2 "MODA") (= UT2 "moda")(= UT2 "DISUSED-CABLE") (= UT2 "disused-cable") )(setq Z2 2)))
	   (if (/= UT1 "E")			   
	   (IF (OR (= UT2 "W") (= UT2 "SW") (= UT2 "SE") (= UT2 "w") (= UT2 "se") (= UT2 "sw") (= UT2 "IRR") (= UT2 "irr") (= UT2 "13.8KV") (= UT2 "13.8kv") (= UT2 "33KV") (= UT2 "33kv") (= UT2 "132KV") (= UT2 "132kv") (= UT2 "STC") (= UT2 "stc") (= UT2 "ITCMLY") (= UT2 "itcmly") (= UT2 "MODA") (= UT2 "moda") )(setq Z2 2)))
	   ) (IF (= UT2 "w") (setq UT2 "W")) (IF (= UT2 "se") (setq UT2 "SE")) (IF (= UT2 "sw") (setq UT2 "SW")) (IF (= UT2 "irr") (setq UT2 "IRR")) (IF (= UT2 "13.8kv") (setq UT2 "13.8KV")) (IF (= UT2 "33kv") (setq UT2 "33KV")) (IF (= UT2 "132kv") (setq UT2 "132KV")) (IF (= UT2 "stc") (setq UT2 "STC")) (IF (= UT2 "itcmly") (setq UT2 "ITCMLY")) (IF (= UT2 "moda") (setq UT2 "MODA")) (IF (= UT2 "disused-cable") (setq UT2 "DISUSED-CABLE")) 
	   (while (< Z3 1)
	   (if (or (= UT2 "STC") (= UT2 "MODA")) 
	   (progn
	   (setq UT3 (getint "\nEnter number of VC (1,2,4,6,8,12,16,20,24, OR 48): " )) 
       (if (or (= UT3 1) (= UT3 2) (= UT3 4) (= UT3 6) (= UT3 8) (= UT3 12) (= UT3 16) (= UT3 20) (= UT3 24) (= UT3 48)) (setq Z3 2))	   
	   ))
	   (if (= UT2 "ITCMLY") 
	   (progn
	   (setq UT3 (getint "\nEnter number of VC (1,6,12, OR 15): " )) 
       (if (or (= UT3 1) (= UT3 6) (= UT3 12) (= UT3 15)) (setq Z3 2))	   
	   ))
	   (if (= UT2 "33KV") 
	   (progn
	   (setq UT3 (getint "\nEnter number of VC (1,2,4, OR 5): " )) 
       (if (or (= UT3 1) (= UT3 2) (= UT3 4) (= UT3 5)) (setq Z3 2))	   
	   ))
	   (if (or (= UT2 "W") (= UT2 "IRR")(= UT2 "SW") (= UT2 "SE")) 
	   (progn
	   (setq UT3 (getint "\nEnter Utility Nominal Diameter in mm : " ))
	   (setq  RRR (RTOS UT3 2 ))
       (setq r (/ (atof RRR) 1000))
	   (setq odflag 0)
	   (while (< odflag 1)
	   (setq ODD1 (getint "\nEnter Utility Outer Diameter in mm : " ))
	   (if (>= ODD1 UT3) (setq odflag 2)))
	   (setq  ODD2 (rtos ODD1 2 2))
       (setq OD (/ (ATOF ODD2) 1000))
	   (setq Z3 2)	   
	   ))
	   (if (or (= UT2 "13.8KV") (= UT2 "132KV") (= UT2 "DISUSED-CABLE")) (setq Z3 2))
	   )
	   
	   



;(cond ((or (= UT1 "B") (= UT1 "b")) 
(if (or (= UT1 "B") (= UT1 "b"))
     (progn
          (COMMAND "LAYER" "SET" "C-PRFL-CROSS-BEND" "")
          (setq d (distance (list (car j) (cadr j)) (list (car c) (cadr c))))
          (setq xp4 (+ (car q) (* d XF)))
          (setq yp4 (cadr qq) )
          (setq p (list xp4 yp4 0))
          (command "_xline" "v" p  "")
          (setq L (getstring "\nEnter bend name :"))
		  (COMMAND "LAYER" "SET" "C-PRFL-CROSS-TEXT" "")
          (command "TEXT" "L" p "0" L)   
      ) );_ end of the first then condition

	 ; ((or (= UT2 "W") (= UT2 "IRR") (= UT2 "SW") (= UT2 "SE"))
(if (or (= UT2 "W") (= UT2 "IRR") (= UT2 "SW") (= UT2 "SE"))
    (progn	 
	   (SETVAR "OSMODE"    0)
	   (COMMAND "LAYER" "SET" "C-PRFL-CROSS-WET" "")
       (setq d (distance (list (car j) (cadr j)) (list (car c) (cadr c))))
       (setq EPODI (MAPCAR '+ q (LIST (* d XF) 0)) )
	   (setq EPOD1 (MAPCAR '- EPODI (LIST 0 (* OD VF))))
	   (setq EPOD2 (MAPCAR '- EPODI (LIST (/ (* OD XF) 2) (/ (* OD VF) 2))))
	   (setq PTH (ABS(/ (- OD r) 2)))
	   (setq EPI (MAPCAR '- EPODI (LIST 0 (* PTH VF))))
	   (setq EP1 (MAPCAR '- EPI (LIST 0 (* r VF))))
	   (setq EP2 (MAPCAR '- EPI (LIST (/ (* r XF) 2) (/ (* r VF) 2) )) )
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (setq i 0)
	    (while (<= i (- n 4))
		   
	       (if (and (<= (nth i coords) (car EPODI)) (>= (nth (+ i 2) coords) (car EPODI)))
		    (PROGN 
			(princ coords)
		      (setq xp1 (nth i coords))
              (setq yp1 (nth (+ i 1) coords))
              (setq xp2 (nth (+ i 2) coords))
              (setq yp2 (nth (+ i 3) coords))
              (setq p1 (list xp1 yp1))
              (setq p2 (list xp2 yp2))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq yIP1 (+ (* slp (car EPODI)) b))
			  (princ b)
			  (setq din (getreal "\nEnter crossing Utility Cover from grade level or Invert Level: "))
			   (if (<= din 10) (setq yIP (- yIP1 (* din VF))))
			   (if (> din 10) 
			   (progn
			    (if (<= datum din) (setq yIP1 (+ (cadr datumpt) (* (- din datum) VF))))
			    (if (> datum din) (setq yIP1 (- (cadr datumpt) (* (- datum din) VF))))
			    (setq yIP (+ yIP1 (* (+ PTH r) VF)))
			   ))
			   (setq xEPODI (car EPODI))
			   (setq EPODI (list xEPODI yIP))
		     )
		  )		  
			  (setq i (+ i 2))
	     )
		(setq EPOD1 (MAPCAR '- EPODI (LIST 0 (* OD VF))))
	    (setq EPOD2 (MAPCAR '- EPODI (LIST (/ (* OD XF) 2) (/ (* OD VF) 2))))
	    (setq PTH (ABS(/ (- OD r) 2)))
	    (setq EPI (MAPCAR '- EPODI (LIST 0 (* PTH VF))))
	    (setq EP1 (MAPCAR '- EPI (LIST 0 (* r VF))))
	    (setq EP2 (MAPCAR '- EPI (LIST (/ (* r XF) 2) (/ (* r VF) 2) )) )
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   (command "_ellipse" EPODI EPOD1 OD  "")
	   (COMMAND "COLOR" "8")
	   (COMMAND "HATCH" "SOLID" "LAST" "")
	   (COMMAND "COLOR" "BYLAYER")
	   (command "_ellipse" EPI EP1 r  "")
	   (if (AND (= UT2 "W") (= UT1 "E"))(COMMAND "COLOR" "4"))
	   (if (AND (= UT2 "W") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "W") (= UT1 "R"))(COMMAND "COLOR" "221"))
	   (if (AND (= UT2 "IRR") (= UT1 "E"))(COMMAND "COLOR" "49"))
	   (if (AND (= UT2 "IRR") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "IRR") (= UT1 "R"))(COMMAND "COLOR" "221"))
	   (if (AND (= UT2 "SE") (= UT1 "E"))(COMMAND "COLOR" "30"))
	   (if (AND (= UT2 "SE") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "SE") (= UT1 "R"))(COMMAND "COLOR" "191"))
	   (if (AND (= UT2 "SW") (= UT1 "E"))(COMMAND "COLOR" "180"))
	   (if (AND (= UT2 "SW") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "SW") (= UT1 "R"))(COMMAND "COLOR" "200"))
	   (COMMAND "HATCH"  "SOLID" "LAST" "")
	   (COMMAND "COLOR" "BYLAYER")
	   (COMMAND "LAYER" "SET" "C-PRFL-CROSS-MLDR" "")
	   (if (or (= pflag 0) (>= d 4)) (progn
	   (setq LD1 (MAPCAR '+ EPODI (LIST -5 10)))
	   (setq LD2 (MAPCAR '+ LD1 (LIST -20.5 0))) ))
	   
	   
	   (if (or (= pflag 0) (>= d 4))
	   (COMMAND "PLINE" EPODI LD1 LD2 ""))
	   (if (and (= pflag 1) (<= d 4))
	   (COMMAND "PLINE" EPODI LDP1 ""))
	   
	   
	   
	   (COMMAND "COLOR" "BYLAYER")
	   (setq DIAM (RTOS (* r 1000) 2 0))
	   (setq TX (STRCAT UT1 "-" UT2 "-" DIAM))
	
	   (COMMAND "LAYER" "SET" "C-PRFL-CROSS-TEXT" "")
	   (if (AND (= UT2 "W") (= UT1 "E"))(COMMAND "COLOR" "4"))
	   (if (AND (= UT2 "W") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "W") (= UT1 "R"))(COMMAND "COLOR" "221"))
	   (if (AND (= UT2 "IRR") (= UT1 "E"))(COMMAND "COLOR" "49"))
	   (if (AND (= UT2 "IRR") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "IRR") (= UT1 "R"))(COMMAND "COLOR" "221"))
	   (if (AND (= UT2 "SE") (= UT1 "E"))(COMMAND "COLOR" "30"))
	   (if (AND (= UT2 "SE") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "SE") (= UT1 "R"))(COMMAND "COLOR" "191"))
	   (if (AND (= UT2 "SW") (= UT1 "E"))(COMMAND "COLOR" "180"))
	   (if (AND (= UT2 "SW") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "SW") (= UT1 "R"))(COMMAND "COLOR" "200"))
	   (if (or (= pflag 0) (>= d 4)) 
       (command "TEXT" "BL" LD2 "0" TX))
       (COMMAND "COLOR" "BYLAYER")
	    (setq p EPODI )
      )) ;_ end of optional else condition

    ; ((or (= UT2 "13.8KV") (= UT2 "33KV") (= UT2 "132KV") (= UT2 "STC") (= UT2 "MODA")(= UT2 "ITCMLY"))
(if (or (= UT2 "13.8KV") (= UT2 "33KV") (= UT2 "132KV") (= UT2 "STC") (= UT2 "MODA")(= UT2 "ITCMLY") (= UT2 "DISUSED-CABLE"))
    (progn 
          (SETVAR "OSMODE"    0)	
	      (COMMAND "LAYER" "SET" "C-PRFL-CROSS-DRY" "")
          (setq d (distance (list (car j) (cadr j)) (list (car c) (cadr c))))
          (setq xp4 (+ (car q) (* d XF)))
          (setq yp4 (cadr q) )
          (setq p (list xp4 yp4 0))
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  (setq i 0)
	      (while (<= i (- n 4))
	       (if (and (<= (nth i coords) (car p)) (>= (nth (+ i 2) coords) (car p)))
		    (PROGN 
		      (setq xp1 (nth i coords))
              (setq yp1 (nth (+ i 1) coords))
              (setq xp2 (nth (+ i 2) coords))
              (setq yp2 (nth (+ i 3) coords))
              (setq p1 (list xp1 yp1))
              (setq p2 (list xp2 yp2))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq yIP1 (+ (* slp (car p)) b))
			  (setq xP (car p))
			  (setq p (list xP yIP1))
			    )
		  )		  
			  (setq i (+ i 2))
	     )
		  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;(setq L (getstring "\nEnter block name :"))
		  
		  (if (= UT2 "DISUSED-CABLE") (setq LTEXT (STRCAT UT1 "-" (substr UT2 01 07))) )
		  (if (/= UT2 "DISUSED-CABLE") (setq LTEXT (STRCAT UT1 "-" UT2)) )
		  
		   (setq UT34 (RTOS UT3 2 0))
		  
		  (if (/= UT34 "0")(setq L (STRCAT UT1 "-" UT2 "-" UT34 "VC")) (setq L (STRCAT UT1 "-" UT2)) )
		  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		  (COMMAND "COLOR" "BYLAYER")
	   (COMMAND "LAYER" "SET" "C-PRFL-CROSS-MLDR" "")
	   (if (= UT2 "13.8KV") (setq lip (mapcar '- p (list 0 (* 0.6 VF))) ))
	   (if (= UT2 "DISUSED-CABLE") (setq lip (mapcar '- p (list 0 (* 0.6 VF))) ))
	   (if (= UT2 "33KV") (setq lip (mapcar '- p (list 0 (* 0.75 VF))) ))
	   (if (= UT2 "132KV") (setq lip (mapcar '- p (list 0 (* 1.35 VF))) ))
	   (if (= UT2 "STC") (setq lip (mapcar '- p (list 0 (* 0.45 VF))) ))
	   (if (= UT2 "MODA") (setq lip (mapcar '- p (list 0 (* 0.45 VF))) ))
	   (if (= UT2 "ITCMLY") (setq lip (mapcar '- p (list 0 (* 0.35 VF))) ))
	   (if (or (= pflag 0) (>= d 4)) (progn
	   (setq LD1 (MAPCAR '+ lip (LIST -5 10)))
	   (setq LD2 (MAPCAR '+ LD1 (LIST -20.5 0)))))
	   
	   (if (or (= pflag 0) (>= d 4))
	   (COMMAND "PLINE" lip LD1 LD2 ""))
	   (if (and (= pflag 1) (<= d 4))
	   (COMMAND "PLINE" lip LDP1 ""))
	   
	   (COMMAND "COLOR" "BYLAYER")
	   
	   (COMMAND "LAYER" "SET" "C-PRFL-CROSS-TEXT" "")
	   (if (AND (= UT2 "DISUSED-CABLE") (= UT1 "E"))(COMMAND "COLOR" "10"))
	   (if (AND (= UT2 "13.8KV") (= UT1 "E"))(COMMAND "COLOR" "10"))
	   (if (AND (= UT2 "13.8KV") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "13.8KV") (= UT1 "R"))(COMMAND "COLOR" "140"))
	   (if (AND (= UT2 "33KV") (= UT1 "E"))(COMMAND "COLOR" "10"))
	   (if (AND (= UT2 "33KV") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "33KV") (= UT1 "R"))(COMMAND "COLOR" "140"))
	   (if (AND (= UT2 "132KV") (= UT1 "E"))(COMMAND "COLOR" "10"))
	   (if (AND (= UT2 "132KV") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "132KV") (= UT1 "R"))(COMMAND "COLOR" "140"))
	   (if (AND (= UT2 "STC") (= UT1 "E"))(COMMAND "COLOR" "80"))
	   (if (AND (= UT2 "STC") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "STC") (= UT1 "R"))(COMMAND "COLOR" "123"))
	   (if (AND (= UT2 "ITCMLY") (= UT1 "E"))(COMMAND "COLOR" "80"))
	   (if (AND (= UT2 "ITCMLY") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "ITCMLY") (= UT1 "R"))(COMMAND "COLOR" "123"))
	   (if (AND (= UT2 "MODA") (= UT1 "E"))(COMMAND "COLOR" "80"))
	   (if (AND (= UT2 "MODA") (= UT1 "T"))(COMMAND "COLOR" "6"))
	   (if (AND (= UT2 "MODA") (= UT1 "R"))(COMMAND "COLOR" "123"))
       
       
		  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (command "_insert" L p "" "" "")
          (COMMAND "LAYER" "SET" "C-PRFL-CROSS-TEXT" "")
		  (if (or (= pflag 0) (>= d 4)) (progn
          (command "TEXT" "BL" LD2 "0" LTEXT)
		  (if (= UT2 "DISUSED-CABLE") (progn  (setq LD2 (MAPCAR '- LD2 (LIST 0 0.75)))(command "TEXT" "TL" LD2 "0" "CABLE") ))))
		  (COMMAND "COLOR" "BYLAYER")
      ) ;_ end of the first then condition
) ;_ end of cond statement


(setq xp3 (car c))
(setq yp3 (cadr c))
(setq j (list xp3 yp3 0))

(setq xp5 (car p))
(setq yp5 (cadr p))
(setq q (list xp5 yp5 0))

(setq flag 1)
(setq UTP1 UT1)
(setq UTP2 UT2)
(if (/= UT3 "0")(setq UTP3 UT3) (setq UTP3 0))
(if (/= r nil)(setq rp r))
(if (/= OD nil)(setq ODP OD))
(setq LDP1 LD1)
(setq LDP2 LD2)
(setq pflag 0)


 (princ)
) ;end while
(setvar "LTSCALE" 1)
(SETVAR "OSMODE" 16383)
)

