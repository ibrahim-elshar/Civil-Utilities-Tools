;;--------------=={ Profile Exaggerated Offset }==------------;;
;;                                                            ;;
;;  Make an exaggerated offset respecting the vertical and    ;;
;;  horizontal scale provided for a LWPolyline specified by   ;;
;;  the user;                                                 ;;
;;------------------------------------------------------------;;
;;  Author: Ibrahim Elsharr, Copyright © 2016                 ;;
;;------------------------------------------------------------;;
;;  Email:  ibrahim.elshar@khatibalami.com                    ;;
;;------------------------------------------------------------;;
;;  Version 1.0    -    08-05-2016                            ;;
;;                                                            ;;
;;  First release.                                            ;;
;;------------------------------------------------------------;;
( defun c:PEXO()
 (vl-load-com)
 (SETVAR "CMDECHO" 0) 
; (setq VSS (getint  "\nEnter Profile Vertical Scale (Default 1:100) 1:" ))
; (setq HSS (getint  "\nEnter Profile Horizontal Scale (Default 1:500) 1:" ))
; (if (= VSS nil) (setq VSS 100))
; (if (= HSS nil) (setq HSS 500))
 (setq VSS
    (cond
      (
        (getint
          (strcat "\nEnter Profile Vertical Scale 1:<"
            (itoa
              (setq VSS
                (cond ( VSS ) ( 100 ))
              )
            )
            ">: "
          )
        )
      )
      ( VSS )
    )
  )
  
  (setq HSS
    (cond
      (
        (getint
          (strcat "\nEnter Profile Horizontal Scale 1:<"
            (itoa
              (setq HSS
                (cond ( HSS ) ( 500 ))
              )
            )
            ">: "
          )
        )
      )
      ( HSS )
    )
  )
 (if (= VSS nil) (setq VSS 100))
 (if (= HSS nil) (setq HSS 500))
 (SETQ    HF (/ 1000.0 HSS))
 (SETQ    VF (/ 1000.0 VSS))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;match properties function
 (defun mm (  e ss2 )
 
  (setq  
       shigh (getvar "highlight") 
       sblip (getvar "blipmode") 
        
  )
  (setvar "blipmode" 0)
    (setvar "highlight" 0)
  (setq en (entget e)
        col (cdr (assoc 62 en))
		ss3 (entget ss2)
  )
  (setq lay (cdr (assoc 8 en))
        lt (cdr (assoc 6 en))
		gw (cdr (assoc 43 en))
  )
  (setq lts (cdr (assoc 48 en)))
  (if (not col)
    (setq col "bylayer")
  )
  (if (not lt)
    (setq lt "bylayer")
  )
  (if (not lts) 
  (progn
    (setq ss3 (subst (cons '43 gw) (assoc 43 ss3) ss3) );subst Searches a list for an old item and returns a copy of the list with a new item substituted in place of every occurrence of the old item (subst newitem olditem lst)
	                                                    ;cons Adds an element to the beginning of a list, or constructs a dotted list (cons new-first-element list-or-atom)
                                                        ;assoc Searches an association list for an element and returns that association list entry (assoc element alist)
	(entmod ss3)
    (command "change" ss2 "" "p" "c" col "la" lay "lt" lt "S" "1" "")	
	)
  (progn
    (setq ss3 (subst (cons '43 gw) (assoc 43 ss3) ss3))
    (entmod ss3)	
	(command "change" ss2 "" "p" "c" col "la" lay "lt" lt "s" lts "")
  )
  )
  (setvar "blipmode" sblip)
  (setvar "highlight" shigh)
  (command "undo" "e")
  (prin1)
)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;error msg function
 (defun *error* (msg)
  ;(princ "error: ")
  (princ msg)
 (princ)
)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; auto get polyline coordinates
 (defun autogetpts () 
(SETVAR "PICKBOX"   pb1)
  (cond
 (*activeDoc*)
 ((setq *activeDoc* (vla-get-activedocument (vlax-get-acad-object)))))
 
 (prompt "\n >> Select object to do an exaggerated offset: ")
 (if (setq ss (ssget ":S:E" '((0 . "*POLYLINE")))) 
 (progn
  
 (setq fg (sslength ss)) ;get the number of objects in the selection set
  ;(setq ss (car (entsel "\n Select :"))) ; (gets entity name) Prompts the user to select a single object (entity) by specifying a point. Return Values Type: List or nil. A list whose first element is the entity name of the chosen object and whose second element is the coordinates (in terms of the current UCS) of the point used to pick the object.
(setq ss1 (ssname ss 0)) ; (gets entity name of first entity in the selection set (the first entity has an index = 0))
(setq e (entget ss1)); get the entity list
(setq pipegw (cdr (assoc 41 e))) ; get the global width
(setq layername (cdr (assoc 8 e))) ;retrieve the layer name
 (vlax-for x (setq ss (vla-get-activeselectionset *activeDoc*)) 
 (setq pipecoords
 (vlax-safearray->list
 (vlax-variant-value
 (vla-get-coordinates x)))))
 (vla-delete ss)) (progn (setq msg nil)(quit)))
 ;(princ pipecoords)
 (setq n 0)  ;set up counter
 ;while not at the end of the list do...
 (while (not (equal (setq item (nth n pipecoords)) nil))
 ;(setq n (1+ n) ) 
 (setq n (+ n 1))    ;add 1 t counter                             
   );end WHILE
  ;(command "erase" "previous" "")
   ;(princ n)
   ;(princ pipecoords)
  (setq i 0) 
  (setq pdt0 nil)
  (setq CD0 0) 
  (setq cdlist0 nil)
  (while (<= i (- (length pipecoords) 4))
  (setq xp1 (/ (nth i pipecoords) HF))
  (setq yp1 (/ (nth (+ i 1) pipecoords) VF))
  (setq xp2 (/ (nth (+ i 2) pipecoords) HF))
  (setq yp2 (/ (nth (+ i 3) pipecoords) VF))
  (setq p1 (list xp1 yp1))
  (setq p2 (list xp2 yp1))
  (setq d0 (distance  p1  p2 ) )
  (setq d10 (list d0))
  (setq pdt0 (append pdt0 d10))
  (setq CD0 (+ CD0 d0))
  (setq CD1pi0 (list CD0))
  (setq cdlist0 (append cdlist0 CD1pi0))
  (setq i (+ i 2))
  )
 ;(princ cdlist0)
 ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (setq ofsd nil)
; (while (or (= ofsd nil) (= ofsd 0) (< ofsd 0))
; (if (or (= ofsd nil) (= ofsd 0) (< ofsd 0)) (prompt "\n Must be a positive number:"))
; (setq ofsd (getreal "\nSpecify the distance that will be exaggerated: " ))
; )
(initget 6 )
(if (or (= ofsd 0) (= ofsd nil)) (while (= ofsd nil) (initget 6 ) (setq ofsd (getreal "\nSpecify the distance that will be exaggerated: " ))) 
(progn ;(princ ofsd) 
;(setq ofsd (getreal (acet-str-format "\nSpecify the distance that will be exaggerated <%1>: " ofsd)))
(initget 6 )
(setq ofsd
    (cond
      (
        (getreal
          (strcat "\nSpecify the distance that will be exaggerated <"
            (rtos ofsd 2 4 )
            ">: "
          )
        )
      )
      ( ofsd )
    )
  )
  )
)
(terpri)
;(princ ofsd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(while (= 1 1)
(SETVAR "CMDECHO"   0)
(setq dz1 (getvar "DIMZIN"))
 (setq pb1 (getvar "PICKBOX")) 
 (setvar "DIMZIN" 0)
 (command "undo" "g") 
(autogetpts)
(setq ptside (getpoint "\nSpecify point on side to do an exaggerated offset: "))
  (setq i 0)
  (setq k 0)
  (setq slpl nil)
  (setq angs nil)
  (while (<= i (- (length pipecoords) 4))
  (setq y1 (/ (nth (+ i 1) pipecoords) VF))
  (setq y2 (/ (nth (+ i 3) pipecoords) VF))
  (setq dist (nth k pdt0))
  (setq slp (/ (- y2 y1) dist))
  (setq ang (atan slp))
  (setq slp (list slp))
  (setq slpl (append slpl slp))
  (setq ang (list ang))
  (setq angs (append angs ang))
  (setq i (+ i 2))
  (setq k (+ k 1))
  )
  ;(princ angs)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;checks the offset direction
  (defun positioning () ;checks if the side point is above or below the line
  (setq i 0)
  (setq flag 0)
  (while (<= i (- (length pipecoords) 4)) 
  	   
		  (if (and (<= (nth i pipecoords)  (car ptside)) (>= (nth (+ i 2) pipecoords) (car ptside))) 
		    (progn
			;(princ (nth i pipecoords))
			  (setq xp1 (nth i pipecoords))
              (setq yp1 (nth (+ i 1) pipecoords))
              (setq xp2 (nth (+ i 2) pipecoords))
              (setq yp2 (nth (+ i 3) pipecoords))
              (setq p1 (list xp1 yp1))
              (setq p2 (list xp2 yp2))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq y (+ (* slp  (car ptside)) b))
			  ;(princ y)
			(if  (< y (cadr ptside)) (setq flag 1) ;the side point is above the line
			  (setq i (+ i 2)))	
		    (if  (= y (cadr ptside)) (progn (while (= y (cadr ptside)) (setq ptside (getpoint "\nSpecify point on side to do an exaggerated offset: "));the side point is on the line, i.e, the offset distance is zero
			  (setq i (+ i 2))) (positioning)))
		    (if  (> y (cadr ptside)) (progn (setq flag -1) (setq i (+ i 2)))) ;the side point is below the line
		     )
			 )
			 	   (setq i (+ i 2))
		   
   )
			 
			 (if  (> (car pipecoords)  (car ptside))
			 (progn
			  ;(princ (nth 0 pipecoords))
			  (setq xp1 (nth 0 pipecoords))
              (setq yp1 (nth (+ 0 1) pipecoords))
              (setq xp2 (nth (+ 0 2) pipecoords))
              (setq yp2 (nth (+ 0 3) pipecoords))
              (setq p1 (list xp1 yp1))
              (setq p2 (list xp2 yp2))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq y (+ (* slp  (car ptside)) b))
			  ;(princ y)
			 (if  (< y (cadr ptside)) (setq flag 1)) ;the side point is above the line
		     (if  (= y (cadr ptside)) (progn (while (= y (cadr ptside)) (setq ptside (getpoint "\nSpecify point on side to do an exaggerated offset: "));the side point is on the line, i.e, the offset distance is zero
			  ) (positioning)))
		    (if  (> y (cadr ptside)) (progn (setq flag -1) )) ;the side point is below the line
		     )
			 )
			 
			 (if  (< (nth (- (length pipecoords) 2) pipecoords)  (car ptside))
			 (progn
			  ;(princ (nth (- (length pipecoords) 4) pipecoords))
			  (setq xp1 (nth (- (length pipecoords) 4) pipecoords))
              (setq yp1 (nth (+ (- (length pipecoords) 4) 1) pipecoords))
              (setq xp2 (nth (+ (- (length pipecoords) 4) 2) pipecoords))
              (setq yp2 (nth (+ (- (length pipecoords) 4) 3) pipecoords))
              (setq p1 (list xp1 yp1))
              (setq p2 (list xp2 yp2))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq y (+ (* slp  (car ptside)) b))
			  ;(princ y)
			 (if  (< y (cadr ptside)) (setq flag 1) ;the side point is above the line
			  )	
		    (if  (= y (cadr ptside)) (progn (while (= y (cadr ptside)) (setq ptside (getpoint "\nSpecify point on side to do an exaggerated offset: "));the side point is on the line, i.e, the offset distance is zero
			  ) (positioning)))
		    (if  (> y (cadr ptside)) (progn (setq flag -1) )) ;the side point is below the line
		     )
			 )
   )
   (positioning)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;offset above line
(defun  exg-offset-above() 
 (setq newpipecoords nil)
  (setq x1 (* ofsd (sin (car angs))))
  (setq y1 (* ofsd (cos (car angs))))
  (setq newpt1 (mapcar '+ (list (car pipecoords) (cadr pipecoords)) (list  (* ( * x1 -1) HF) (* y1 VF))))
  (setq newpt1 (list newpt1))
  (setq newpipecoords (append newpipecoords newpt1))
  (setq i 0)
  (setq k 2)
  (while (<= i (- (length angs) 2))
  (setq ang1 (nth i angs))
  (setq ang2 (nth (+ i 1) angs))
  (setq hyp (/ ofsd (cos (/ (- ang1 ang2) 2))))
  (setq xi (* hyp (sin (/ (+ ang1 ang2) 2))))
  (setq yi (* hyp (cos (/ (+ ang1 ang2) 2))))
  (setq newpti (mapcar '+ (list (nth (+ k 0) pipecoords) (nth (+ k 1) pipecoords)) (list (* ( * xi -1) HF) (* yi VF))))
  (setq newpti (list newpti))
  (setq newpipecoords (append newpipecoords newpti))
  (setq i (+ i 1))
  (setq k (+ k 2))
  )  
  (setq xn (* ofsd (sin (last angs))))
  (setq yn (* ofsd (cos (last angs))))
  (setq newptn (mapcar '+ (list (nth (- (length pipecoords) 2) pipecoords) (nth (- (length pipecoords) 1) pipecoords)) (list (* ( * xn -1) HF)  (* yn VF))))
  (setq newptn (list newptn))
  (setq newpipecoords (append newpipecoords newptn))
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;offset below line
  (defun  exg-offset-below() 
 (setq newpipecoords nil)
  (setq x1 (* ofsd (sin (car angs))))
  (setq y1 (* ofsd (cos (car angs))))
  (setq newpt1 (mapcar '+ (list (car pipecoords) (cadr pipecoords)) (list  (* ( * x1 1) HF) (* (* y1 VF) -1))))
  (setq newpt1 (list newpt1))
  (setq newpipecoords (append newpipecoords newpt1))
  (setq i 0)
  (setq k 2)
  (while (<= i (- (length angs) 2))
  (setq ang1 (nth i angs))
  (setq ang2 (nth (+ i 1) angs))
  (setq hyp (/ ofsd (cos (/ (- ang1 ang2) 2))))
  (setq xi (* hyp (sin (/ (+ ang1 ang2) 2))))
  (setq yi (* hyp (cos (/ (+ ang1 ang2) 2))))
  (setq newpti (mapcar '+ (list (nth (+ k 0) pipecoords) (nth (+ k 1) pipecoords)) (list (* ( * xi 1) HF) (* (* yi VF) -1))))
  (setq newpti (list newpti))
  (setq newpipecoords (append newpipecoords newpti))
  (setq i (+ i 1))
  (setq k (+ k 2))
  )  
  (setq xn (* ofsd (sin (last angs))))
  (setq yn (* ofsd (cos (last angs))))
  (setq newptn (mapcar '+ (list (nth (- (length pipecoords) 2) pipecoords) (nth (- (length pipecoords) 1) pipecoords)) (list (* ( * xn 1) HF)  (* (* yn VF) -1))))
  (setq newptn (list newptn))
  (setq newpipecoords (append newpipecoords newptn))
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (= flag 1) (exg-offset-above) (exg-offset-below))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;drawing the new polyline
  (entmakex
                (append
                    (list
                       '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                        (cons 90 (length newpipecoords))
                       '(70 . 0)
                    )
                    (mapcar '(lambda ( x ) (cons 10 x)) newpipecoords)
                )
     )
	 (mm ss1 (entlast))
  ; (setq i 0) 
  ; (SETVAR "OSMODE"    0)
  ; (setvar "ORTHOMODE" 0)
  ; (SETVAR "PICKBOX"   0)
  ; (while (<= i (- (length newpipecoords) 1))
   ; (command "PLINE"  (nth i newpipecoords) (nth (+ i 1) newpipecoords) "" )
   ; (setq i (+ i 1))
   ; )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;zoom
  ; (setq pk1 (car newpipecoords))
  ; (setq pk2 (last newpipecoords))
  ; (setq zmp1 (mapcar '+ pk1 '(-50 50 0)))
  ; (setq zmp2 (mapcar '+ pk2 '( 50 -50 0)))
  ; (COMMAND "ZOOM" "W" zmp1 zmp2)
  ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;join
  ; (setq i 0) 
  ; (while (<= i (- (length newpipecoords) 1))
   ; (command "_JOIN"  (nth i newpipecoords) (nth (+ i 1) newpipecoords) "" )
   ; (setq i (+ i 1))
   ; )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (SETVAR "PICKBOX"   4)
 (SETVAR "OSMODE" 16383)
 (command "UNDO" "e")
 (SETVAR "CMDECHO"   1 )
 )
)
 (terpri)
 (prompt "\nPEXO (VER. 1.0 - Date: 08-05-2016)" )
 (prompt "\nA Lisp routine to make an exaggerated offset in accordance with a specified scale." )
 (prompt "\nAuthor: Ibrahim El Sharr." )
 (prompt "\n>Type PEXO to start." )
;;------------------------------------------------------------;;
;;                        End of File                         ;;
;;------------------------------------------------------------;;    
   

  
  