;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WP PRERELEASE A LISP ROUTINE TO  DRAW WATER PROFILES BY IBRAHIM EL SHARR ------- Ver. 1.0 DATE: 27-12-2016 ;
;                                                                                                             ; 
;                                                                                                             ;
;                                                                                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Time Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun timeini ()
  (setq s (getvar "DATE"))
  (setq seconds (* 86400.0 (- s (fix s))))
)

(defun timeend ()
  (setq s1 (getvar "DATE"))
  (setq seconds1 (* 86400.0 (- s1 (fix s1))))
  (setq seconds2 (fix (- seconds1 seconds)))
  (princ
    (strcat "\nTime : "
	    (itoa seconds2)
	    " seconds"
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Polyline angles Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w_ang? ( p1 px p2 / l_pt l_d p ang)
(setq
l_pt (mapcar '(lambda (x) (list (car x) (cadr x))) (list px p1 p2))
l_d (mapcar 'distance l_pt (append (cdr l_pt) (list (car l_pt))))
p (/ (apply '+ l_d) 2.0)
)
(if (zerop (* p (- p (cadr l_d))))
(setq ang pi)
(setq ang (* (atan (sqrt (abs (/ (* (- p (car l_d)) (- p (caddr l_d))) (* p (- p (cadr l_d))))))) 2.0))
)
(angtos (- pi ang) (getvar "AUNITS") (getvar "AUPREC"))
)
(defun id_pline_ang ( ent / js a_base a_dir ent dxf_ent pt_lst l_ang)
;(while (not (setq js (ssget "_+.:E:S" '((0 . "LWPOLYLINE"))))))
(setq
a_base (getvar "ANGBASE")
a_dir (getvar "ANGDIR")
;ent (ssname js 0)
dxf_ent (entget ent)
pt_lst (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) dxf_ent))
)
(setvar "ANGBASE" 0)
(setvar "ANGDIR" 0)
(while (and (car pt_lst) (cadr pt_lst) (caddr pt_lst))
(setq
l_ang (cons (w_ang? (car pt_lst) (cadr pt_lst) (caddr pt_lst)) l_ang)
pt_lst (cdr pt_lst)
)
)
(setvar "ANGBASE" a_base)
(setvar "ANGDIR" a_dir)
(reverse l_ang)
(setq ang_list (reverse l_ang))
(setq i 0)
(setq bend_angles nil)
;(setq angtol 5) ;angle deviation tolerance
(while (<= i (- (length ang_list) 1))
(cond
((and (>= (atof (nth i ang_list)) 0) (<= (atof (nth i ang_list)) angtol)) (setq bend_angles (append bend_angles (list  nil))))
((and (>= (atof (nth i ang_list)) (- 11.25 angtol)) (<= (atof (nth i ang_list)) (+ 11.25 angtol))) (setq bend_angles (append bend_angles (list "11.25%%dH.BEND"))))
((and (>= (atof (nth i ang_list)) (- 22.5 angtol)) (<= (atof (nth i ang_list)) (+ 22.5 angtol))) (setq bend_angles (append bend_angles (list "22.5%%dH.BEND"))))
((and (>= (atof (nth i ang_list)) (- 45 angtol)) (<= (atof (nth i ang_list)) (+ 45 angtol))) (setq bend_angles (append bend_angles (list "45%%dH.BEND"))))
((and (>= (atof (nth i ang_list)) (- 90 angtol)) (<= (atof (nth i ang_list)) (+ 90 angtol))) (setq bend_angles (append bend_angles (list "90%%dH.BEND"))))
(t  (setq bend_angles (append bend_angles (list "NON-STANDARD-BEND"))))
)
(setq i (+ i 1))
)
(setq bend_angles (cons nil bend_angles))
(setq bend_angles (append bend_angles (list nil)))
;(setq bend_angles (mapcar '(lambda ( x ) (cond ((>= (atoi x) 6) (strcat x "%%dH.BEND")))) (reverse l_ang)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setvar Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun inivar ()
  (setq	cmd_ini	(getvar "cmdecho")
	fla_ini	(getvar "flatland")
	osm_ini	(getvar "osmode")
	ort_ini	(getvar "orthomode")
	plt_ini	(getvar "plinetype")
	aup_ini	(getvar "auprec")
	uni_ini	(getvar "unitmode")
	lun_ini	(getvar "lunits")
	diz_ini	(getvar "dimzin")
	edg_ini	(getvar "edgemode")
  )
  (setvar "CMDECHO" 0)
  (setvar "FLATLAND" 0)
  (setvar "OSMODE" 0)
  (setvar "ORTHOMODE" 0)
  (setvar "PLINETYPE" 2)
  (setvar "AUPREC" 0)
  (setvar "UNITMODE" 1)
  (setvar "LUNITS" 2)
  (setvar "DIMZIN" 8) ; zero
  (setvar "EDGEMODE" 1)
)

(defun recvar ()
  (setvar "CMDECHO" cmd_ini)
  (setvar "FLATLAND" fla_ini)
  (setvar "OSMODE" osm_ini)
  (setvar "ORTHOMODE" ort_ini)
  (setvar "PLINETYPE" plt_ini)
  (setvar "AUPREC" aup_ini)
  (setvar "UNITMODE" uni_ini)
  (setvar "LUNITS" lun_ini)
  (setvar "DIMZIN" diz_ini)
  (setvar "EDGEMODE" edg_ini)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chainage Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CHAINAGE_ERRORMSG, CHAINAGE_ATSTART and CHAINAGE_ATEND
; ======================================================
; Description:
; ============
; Handle error messages, set-up and restore point to error message handler, and being/end UNDO loop

; Global Variables:
; =================
; chainage_olderr = pointer to old error message handler

; Internal Variables:
; ===================
; ce_t1 = error message passed to error message handler
; ce_o1 = old value of CMDECHO in chainage_errormsg
; cs_o1 = old value of CMDECHO in chainage_atstart
; ca_o1 = old value of CMDECHO in chainage_atend
(defun chainage_errormsg (ce_t1 / ce_o1)

; restore pointer to old error message handler and display error message
 (setq *error* chainage_olderr)
 (setq chainage_olderr nil)
 (princ (strcat "\nCommand stopped due to error: " ce_t1))

; end UNDO group command
 (setq ce_o1 (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (command "UNDO" "E")
 (setvar "CMDECHO" ce_o1)
 (princ)
)
; =======================================================================================================
(defun chainage_atstart ( / cs_o1)

; set error message handler pointer to new error message handler
 (setq chainage_olderr *error*)
 (setq *error* chainage_errormsg)

; begin UNDO group command
 (setq cs_o1 (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (command "UNDO" "BE")
 (setvar "CMDECHO" cs_o1)
)
; =======================================================================================================
(defun chainage_atend ( / ca_o1)

; end UNDO group command
 (setq ca_o1 (getvar "CMDECHO"))
 (setvar "CMDECHO" 0)
 (command "UNDO" "E")
 (setvar "CMDECHO" ca_o1)

; restore pointer to old error message handler
 (setq *error* chainage_olderr)
 (setq chainage_olderr nil)
)
; =======================================================================================================
; CHAINAGE_SELECTPLINE
; ====================
; Description:
; ============
; Gets user to select a lightweight polyline
; Output: polyline parse data (using parse_polyline) or nil if nothing selected

; Internal Variables:
; ===================
; cp_l1 = output polyline parsed list
; cp_e1 = selected polyline entity
; cp_d1 = selected polyline data list

(defun chainage_selectpline (  l1)

; set default output to nil (nothing selected yet) 
 (setq cp_l1 nil)

; user selects lightweight polyline - error if nothing selected
 ; (if (= (setq cp_e1 (entsel "Select a lightweight polyline")) nil)
  ; (princ "\nNo object selected. Command terminating")

; ; or error if object selected is not correct type
  ; (if (/= (cdr (assoc 0 (setq cp_d1 (entget (car cp_e1))))) "LWPOLYLINE")
   ; (princ "\nObject is not a lightweight polyline. Command terminating")

; otherwise parse source polyline
   (setq cp_d1 (entget l1))
   (setq cp_l1 (parse_polyline cp_d1))
  
; set output list to itself, so that this is the output from this function
 (setq cp_l1 cp_l1)
)
; =======================================================================================================
; PARSE_POLYLINE
; ==============
; Description:
; ============
; Creates an easier-to-use list of lines and arc elements in a lightweight polyline
; Input: data list for lightweight polyline (e.g. from entget)
; Output: list of line and arc segments in format
;         LINE (start point) (end point) distance
;         ARC (start point) (end point) circumference (centre point) radius start-angle end-angle

; Scope for improvement:
; ======================
; Current version of this command doesn't check for closed polylines

; Internal Variables:
; ===================
; pp_d1 = lightweight polyline data list passed to function
; pp_c1 = count through polyline data list
; pp_p1 = start point for line or arc segment
; pp_l2 = output list of line and arc data - output from function
; pp_z1 = z coordinate for lightweight polyline
; pp_n1 = code value of item pp_i1
; pp_i1 = individual item in polyline data list
; pp_b1 = bulge factor for arc segment (= 0.0 for line segment)
; pp_p2 = end point for line or arc segment
; pp_i2 = individual item to be added to output list
; pp_x1, pp_y1 = x and y coordinates of pp_p1
; pp_x2, pp_y2 = x and y coordinates of pp_p2
; pp_x3, pp_y3 = x and y coordinates for mid-point between pp_p1 and pp_p2, then x and y coordinates for centre of arc
; pp_x4, pp_y4 = x and y coordinates for mid-point along arc segment
; pp_f1, pp_f2 = numerators for calculating arc centre-point
; pp_f3 = denominator for calculating arc centre-point
; pp_p3 = arc centre-point
; pp_a1 = start angle (in radians) for arc segment
; pp_a2 = end angle (in radians) for arc segment
; pp_r1 = radius of arc segment

(defun parse_polyline (pp_d1 / pp_c1 pp_p1 pp_l2 pp_z1 pp_n1 pp_i1 pp_b1 pp_p2 pp_i2 pp_x1 pp_y1 pp_x2
                               pp_y2 pp_x3 pp_y3 pp_x4 pp_y4 pp_f1 pp_f2 pp_f3 pp_p3 pp_a1 pp_a2 pp_r1)

; set counter, undefined start point, and empty output list
 (setq pp_c1 0 pp_p1 nil pp_l2 nil)

; get z coordinate for lightweight polyline (or set to 0 if no z coordinate available)
 (if (= (assoc 38 pp_d1) nil)
  (setq pp_z1 0.0)
  (setq pp_z1 (cdr (assoc 38 pp_d1)))
 )

; go through each item in lightweight polyline data list
 (repeat (length pp_d1)

; get code and values for each item in source polyline data list
  (setq pp_n1 (car (setq pp_i1 (nth pp_c1 pp_d1))))

; store bulge factor
  (if (= pp_n1 42)
   (setq pp_b1 (cdr pp_i1))

; point at start/end of segment
   (if (= pp_n1 10)

; store start point, adding z coordinate if only a 2D point
    (if (= pp_p1 nil)
     (progn
      (setq pp_p1 (cdr pp_i1))
      (if (= (length pp_p1) 2)
       (setq pp_p1 (append pp_p1 (list pp_z1)))
      )
     )

; store end point, adding z coordinate if only a 2D point
     (progn
      (setq pp_p2 (cdr pp_i1))
      (if (= (length pp_p2) 2)
       (setq pp_p2 (append pp_p2 (list pp_z1)))
      )

; create new item for adding to output list
      (if (= pp_b1 0.0)
       (setq pp_i2 (list "LINE"))
       (setq pp_i2 (list "ARC"))
      )
      (setq pp_i2 (append pp_i2 (list pp_p1) (list pp_p2)))

; if line segment add length of line to output item
      (if (= pp_b1 0.0)
       (setq pp_i2 (append pp_i2 (list (distance pp_p1 pp_p2))))

; if arc segment...
       (progn
        (setq pp_x1 (car pp_p1) pp_y1 (cadr pp_p1) pp_x2 (car pp_p2) pp_y2 (cadr pp_p2))

; calculate point mid-way between arc segment start and end points (in straight line)
        (setq pp_x3 (/ (+ pp_x1 pp_x2) 2.0) pp_y3 (/ (+ pp_y1 pp_y2) 2.0))

; calculate point mid-way along arc segment (so have three points to calculate arc centre-point)
        (setq pp_x4 (+ pp_x3 (* pp_b1 (- pp_y3 pp_y1))) pp_y4 (+ pp_y3 (* pp_b1 (- pp_x1 pp_x3))))

; calculate numerators and denominators for calculating arc centre-point
        (setq pp_f1 (- (+ (* pp_x1 pp_x1) (* pp_y1 pp_y1)) (* pp_x2 pp_x2) (* pp_y2 pp_y2)))
        (setq pp_f2 (- (+ (* pp_x1 pp_x1) (* pp_y1 pp_y1)) (* pp_x4 pp_x4) (* pp_y4 pp_y4)))
        (setq pp_f3 (- (* (- pp_x1 pp_x2) (- pp_y1 pp_y4)) (* (- pp_x1 pp_x4) (- pp_y1 pp_y2))))

; calculate arc centre-point
        (setq pp_x3 (/ (- (* (- pp_y1 pp_y4) pp_f1) (* (- pp_y1 pp_y2) pp_f2)) (* pp_f3 2.0)))
        (setq pp_y3 (/ (- (* (- pp_x1 pp_x2) pp_f2) (* (- pp_x1 pp_x4) pp_f1)) (* pp_f3 2.0)))
        (setq pp_p3 (list pp_x3 pp_y3 (caddr pp_p1)))

; calculate arc start and end angles (increasing if -ve bulge/anti-clockwise, decreasing if +ve/clockwise) and radius
        (setq pp_a1 (angle pp_p3 pp_p1) pp_a2 (angle pp_p3 pp_p2) pp_r1 (distance pp_p3 pp_p1))
        (if (and (< pp_b1 0.0) (<= pp_a1 pp_a2))
         (setq pp_a1 (+ pp_a1 chainage_twopi))
        )
        (if (and (> pp_b1 0.0) (<= pp_a2 pp_a1))
         (setq pp_a2 (+ pp_a2 chainage_twopi))
        )

; add length of arc segment to output item
        (if (< pp_b1 0.0)
         (setq pp_i2 (append pp_i2 (list (* (- pp_a1 pp_a2) pp_r1))))
         (setq pp_i2 (append pp_i2 (list (* (- pp_a2 pp_a1) pp_r1))))
        )

; add centre-point, radius, start-angle and end-angle to output item
        (setq pp_i2 (append pp_i2 (list pp_p3 pp_r1 pp_a1 pp_a2)))
       )
      )

; only add line or arc segment to list if it has length (ignore it otherwise)
      (if (> (nth 3 pp_i2) 0.0)

; create output list if doesn't exist yet, otherwise add output item to output list
       (if (= pp_l2 nil)
        (setq pp_l2 (list pp_i2))
        (setq pp_l2 (append pp_l2 (list pp_i2)))
       )
      )

; start point of next segment = end point of current segment 
      (setq pp_p1 pp_p2)
     )
    )
   )
  )

; increase counter to look at next item in source lightweight polyline data list
  (setq pp_c1 (1+ pp_c1))
 )

; set output list to itself, so that this is the output from this function
 (setq pp_l2 pp_l2)
)
; =======================================================================================================
; PARSE_POLYLINE_GETLEN
; =====================
; Description:
; ============
; Gets length of polyline list parsed using parse_polyline function
; Input: list of line and arc segments in parse_polyline format
; Output: total length of all segments

; Internal Variables:
; ===================
; pg_l1 = list of line and arc data - outputted from parse_polyline function
; pg_c1 = count through list of data
; pg_n2 = total length value (output)

(defun parse_polyline_getlen (pg_l1 / pg_c1 pg_n2)

; initiate counter and length to zero
 (setq pg_c1 0 pg_n2 0.0)

; go through each element in list
 (repeat (length pg_l1)

; add up length values in each element in list
  (setq pg_n2 (+ pg_n2 (nth 3 (nth pg_c1 pg_l1))))

; increase counter to look at next element in list
  (setq pg_c1 (1+ pg_c1))
 )

; set output length to itself, so that this is the output from this function
 (setq pg_n2 pg_n2)
)
; =======================================================================================================
; CHMARK_DRAW
; ===========

; Description:
; ============
; Draw chainage mark (or section) line and text (not for sections) at given chainage
; Inputs: polyline segments list, start chainage, requested chainage, chainage mark number style
; Output: None

; Internal Variables:
; ===================
; ck_l1 = source polyline parsed list
; ck_n1 = chainage at start of polyline
; ck_n2 = chainage where chainage mark is to be drawn
; ck_b1 = chainage mark number style (nil for CHXSECT and CHXSECTS)
; ck_s1 = chainage mark line length (bigger for CHXSECT and CHXSECTS)
; ck_c1 = count through ck_l1
; ck_n3 = chainage at start of segment
; ck_z1 = initial value of DIMZIN
; ck_h1 = current text height
; ck_i1 = item in ck_l1
; ck_n4 = length of segment
; ck_p1 = mid-point of chainage mark, then start point for text
; ck_a1 = angle along line or arc, then angle for chainage mark
; ck_t1 = text output
; ck_b2 = flag: T if text output is negative, nil if positive

(defun chmark_draw (ck_l1 ck_n1 ck_n2 ck_b1 ck_s1 / ck_c1 ck_n3 ck_z1 ck_h1 ck_i1 ck_n4 ck_p1 ck_a1 ck_t1 ck_b2)

; initiate counter and set chainage at start of segment to chainage at start of polyline
 (setq ck_c1 0 ck_n3 ck_n1)

; remember initial value of DIMZIN and get current text height
 (setq ck_z1 (getvar "DIMZIN") ck_h1 (getvar "TEXTSIZE"))

; keep looking until counter equals number of segments in polyline
 (while (< ck_c1 (length ck_l1))

; look at individual segment, and get its length
  (setq ck_i1 (nth ck_c1 ck_l1))
  (setq ck_n4 (nth 3 ck_i1))

; see if chainage mark chainage is within this segment
  (if (and (>= ck_n2 ck_n3) (<= ck_n2 (+ ck_n3 ck_n4)) (> ck_n4 0.0))
   (progn

; calculate position and angle of chainage mark along a line segment
    (if (= (car ck_i1) "LINE")
     (progn
      (setq ck_p1 (nth 1 ck_i1))
      (setq ck_a1 (angle ck_p1 (nth 2 ck_i1)))
      (setq ck_p1 (polar ck_p1 ck_a1 (- ck_n2 ck_n3)))
      (setq ck_a1 (+ ck_a1 chainage_halfpi))
     )

; calculate position and angle of chainage mark along an arc segment
     (progn
      (setq ck_a1 (+ (nth 6 ck_i1) (* (- (nth 7 ck_i1) (nth 6 ck_i1)) (/ (- ck_n2 ck_n3) ck_n4))))
      (setq ck_p1 (polar (nth 4 ck_i1) ck_a1 (nth 5 ck_i1)))
     )
    )

; draw chainage mark (or section line for CHXSECT and CHXSECTS)
(if (= allowchainage 1)
    (entmake (list (cons 0 "LINE") (cons 100 "AcDbEntity") (cons 100 "AcDbLine")
                   (append (list 10) (polar ck_p1 ck_a1 ck_s1))
                   (append (list 11) (polar ck_p1 (+ ck_a1 chainage_pi) ck_s1))))
)

; if chainage mark number style exists, draw chainage text
    (if (/= ck_b1 nil)
     (progn

; set DIMZIN to zero so that trailing and leading zeros are NOT truncated by 'rtos'
      (setvar "DIMZIN" 0)

; simple n.nnn style
      (if (= ck_b1 1)
       (setq ck_t1 (rtos ck_n2 2 2))

; either n+nn or n+nn.nn styles
       (progn

; create initial text string without the + sign
        (if (= ck_b1 2)
         (setq ck_t1 (rtos ck_n2 2 0))
         (setq ck_t1 (rtos ck_n2 2 2)) 
        )

; set flag if output text string is negative, and strip - sign from start of string
        (if (= (substr ck_t1 1 1) "-")
         (setq ck_b2 T ck_t1 (substr ck_t1 2))
         (setq ck_b2 nil)
        )

; add preceeding zeros so 1 becomes 0001, 23 becomes 0023, 1.23 becomes 0001.23 or 45.67 becomes 0045.67
        (while (or (and (= ck_b1 2) (< (strlen ck_t1) 4)) (and (= ck_b1 3) (< (strlen ck_t1) 7)))
         (setq ck_t1 (strcat "0" ck_t1))
        )

; insert a + sign so 0001 becomes 0+001, or 0001.23 becomes 0+001.23
        (if (= ck_b1 2)
         (setq ck_t1 (strcat (substr ck_t1 1 (- (strlen ck_t1) 3)) "+" (substr ck_t1 (- (strlen ck_t1) 2))))
         (setq ck_t1 (strcat (substr ck_t1 1 (- (strlen ck_t1) 6)) "+" (substr ck_t1 (- (strlen ck_t1) 5))))
        )

; add preceeding - sign if output text is negative number
        (if (= ck_b2 T)
         (setq ck_t1 (strcat "-" ck_t1))
        )
       )
      )

; reset DIMZIN to initial value
      (setvar "DIMZIN" ck_z1)

; rotate text if between 90 and 270 degrees, by 180 degrees so text is always shown left-to-right or down-to-up
      (if (and (> ck_a1 chainage_halfpi) (< ck_a1 chainage_oneandahalfpi))
       (setq ck_a1 (+ ck_a1 chainage_pi))
      )

; move insertion point (1.75 determines the gap between mark and text) ready for drawing the text
      (setq ck_p1 (polar ck_p1 ck_a1 (* ck_s1 1.75)))

; insertion point is in (11 ..) and (10 ..) is bottom left corner of text (rotate 90 degrees and offset by half text height)
(if (= allowchainage 1)
      (entmake (list (cons 0 "TEXT") (cons 100 "AcDbEntity") (cons 100 "AcDbText")
                     (append (list 10) (polar ck_p1 (- ck_a1 chainage_halfpi) (/ ck_h1 2.0)))
                     (cons 72 0) (cons 73 2) (append (list 11) ck_p1)
                     (cons 50 ck_a1) (cons 40 ck_h1) (cons 1 ck_t1)))
)
     (setq chtext1 (list ck_t1))
	 (setq chtext (append chtext chtext1))
	 (setq chn1 (list ck_n2))
	 (setq chn (append chn chn1))
	 )
    )
	
; set counter to equal number of segments in order to exit while lopp and exit this function
    (setq ck_c1 (length ck_l1))
   )

; if segment not found yet, increase counter to next segement and increase chainage to start of next segment
   (setq ck_c1 (1+ ck_c1) ck_n3 (+ ck_n3 ck_n4))
  )
 )
)
; =======================================================================================================
;  OOOO O   O O   O   O   OOOO  O   O  OOOO
; O     O   O OO OO  O O  O   O O  O  O
; O     OOOOO O O O O   O OOOO  OOO    OOO
; O     O   O O   O OOOOO O   O O  O      O
;  OOOO O   O O   O O   O O   O O   O OOOO
; =======================================================================================================
; CHMARKS
; =======
; Description:
; ============
; Draw chainage marks (e.g. "100.000 -") at regular intervals along source polyline

; Global Variables:
; =================
; chainage_chatstart = chainage value at start of polyline
; chainage_reginterval = regular interval between chainage marks
; chainage_markstyle = how to display chainage 1 = n.nnn 2 = n+nn 3 = n+nn.nn

; Internal Variables:
; ===================
; l1 = source polyline parsed list
; n1 = chainage value at start of polyline
; n2 = chainage mark line length
; r1 = regular interval between chainage marks
; s1 = chainage mark number style
; n3 = chainage of initial and then current chainage mark
; n4 = chainage at end of polyline

(defun CHMARKS  (l1 n1 n2 r1 s1 n3 n4)
 (chainage_atstart)

; get user to select lightweight polyline (nil if polyline not selected)
 (if (/= (setq l1 (chainage_selectpline l1)) nil)
  (progn

; get chainage value at start of polyline, setting it to global value (or zero) if no value entered
   ; (setq n1 (chainage_getglobalvalue "\nEnter chainage value at start of polyline"
             ; chainage_chatstart 0.0 nil))
   ; (setq chainage_chatstart n1)
   
(setq chainage_chatstart n1)
; set chainage mark line length
   (setq n2 (getvar "TEXTSIZE"))

; get regular interval between chainage marks (ensure it's not less than 1.0)
   ; (setq r1 (chainage_getglobalvalue "\nEnter interval between chainage marks (min. interval of 1.0)"
             ; chainage_reginterval 10.0 nil))
			 
   (if (< r1 1.0)
    (setq r1 1.0)
   )
   (setq chainage_reginterval r1)

; get chainage mark number style
   ; (setq s1 (chainage_getglobalvalue "\nEnter chainage mark style (1=n.nnn 2=n+nn 3=n+nn.nn)"
             ; chainage_markstyle 1 T))
			 
   (if (or (< s1 1) (> s1 3))
    (setq s1 1)
   )
   (setq chainage_markstyle s1)

; calculate start chainage to next regular interval after start of polyline
   (setq n3 (- n1 (rem n1 r1))) ;rem: Divides the first number by the second, and returns the remainder
   (if (< n3 n1)
    (setq n3 (+ n3 r1))
   )

; get overall length of polyline
   (setq n4 (+ n1 (parse_polyline_getlen l1)))

; keep looking until past end of polyline
   (while (<= n3 n4)

; draw chainage mark
    (chmark_draw l1 n1 n3 s1 n2)

; increase chainage by regular interval
    (setq n3 (+ n3 r1))
   )
   (if (= s1 1) (setq lasts1 1) (setq lasts1 3))
   (chmark_draw l1 n1 n4 lasts1 n2)
   (princ "\nCommand finished")
  )
 )
 (chainage_atend)
 (princ)
)
; =======================================================================================================
; Set PI variables used by various routines 
(setq chainage_pi            3.1415926535897932384626433832795
      chainage_halfpi        1.5707963267948966192313216916398
      chainage_twopi         6.2831853071795864769252867665590
      chainage_oneandahalfpi 4.7123889803846898576939650749193)

; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscelleneous & List Manipulation functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load the help documentation files.
(defun wp_help ()
  (help "wp.chm")
 (princ)
)
; =======================================================================================================
;; Returns a list with the entered element number removed.
(defun RemoveNth ( n l )
    (if (and l (< 0 n))
        (cons (car l) (RemoveNth (1- n) (cdr l)))
        (cdr l)
    )
)
; =======================================================================================================
;; Returns a list with duplicate elements removed.
(defun LM:Unique (l / x r)
  (while l
    (setq x (car l)
	  l (vl-remove x (cdr l))
	  r (cons x r)
    )
  )
  (reverse r)
)
; =======================================================================================================
(defun Roundto ( n p ) (/ (fix (+ (* n (setq p (expt 10. p))) (if (minusp n) -0.5 0.5))) p)) ; example (Roundto 9.58 2) will give 9.6
; =======================================================================================================
(defun draw_polyline (lst)
 (setq i 0)
 (setq pairll nil)
 (while (<= i (- (length lst) 2))
 (setq pairl (list (nth i lst) (nth (+ i 1) lst)))
 (setq pairll (append pairll (list pairl)))
 (setq i (+ i 2))
 )
(setq ll1 (entmakex
                (append
                    (list
                       '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                        (cons 90 (length pairll))
                       '(70 . 0)
                    )
                    (mapcar '(lambda ( x ) (cons 10 x)) pairll)
                )
     ))
  )
 ; =======================================================================================================
 ; =======================================================================================================
(defun draw_poly (lst)
 (setq ll1 (entmakex
                (append
                    (list
                       '(0 . "LWPOLYLINE")
                       '(100 . "AcDbEntity")
                       '(100 . "AcDbPolyline")
                        (cons 90 (length lst))
                       '(70 . 0)
                    )
                    (mapcar '(lambda ( x ) (cons 10 x)) lst)
                )
     ))
  )
 ; =======================================================================================================
 ;; Sublst 
;; The list analog of the substr function
;; lst - [lst] List from which sublist is to be returned
;; idx - [int] Zero-based index at which to start the sublist
;; len - [int] Length of the sublist or nil to return all items following idx
(defun LM:sublst ( lst idx len / rtn )
    (setq len (if len (min len (- (length lst) idx)) (- (length lst) idx))
          idx (+  idx len)
    )
	(setq len (- len 1))
    (repeat len (setq rtn (cons (nth (setq idx (1- idx)) lst) rtn)))
)
 ; =======================================================================================================
;; pSublst 
;; lst - [lst] List from which sublist is to be returned
;; idx - [int] Zero-based index at which to end the sublist
(defun psublst ( lst idx  / len rtn )
    (setq len -1)
	(setq rtn nil)
    (repeat idx (setq rtn (cons (nth (setq len (1+ len)) lst) rtn)))
)
 ; =======================================================================================================
 (defun  calc_il_skip (ptsip cdtp / i fpnne fnnne idif invert pos1 pos2 sumpdt slp d ptssi)
 (setq i 1)
 (setq ptssi (list (car ptsip)))
 (while (< i (- (length ptsip) 1))
 (if (/= (nth i ptsip) nil)
 (setq ptssi (append ptssi (list (nth i ptsip))))
 (progn
 (setq fpnne (car (vl-remove nil (psublst ptsip i)))) ;first previous non nil element in a list before index i
 (setq fnnne (car (vl-remove nil (LM:sublst ptsip i nil)))) ;first next non nil element in a list after index i
 ;(princ fpnne)
 ;(terpri)
 ;(princ fnnne)
 ;(terpri)
 (setq idif (- fnnne fpnne))
 (setq pos1 (vl-position fpnne ptsip))
 (setq pos2 (vl-position fnnne ptsip))
 (setq sumpdt (- (nth pos2 cdtp) (nth pos1 cdtp)) )
 ;(princ (nth pos2 cdtp))
 (terpri)
 ;(princ (nth pos1 cdtp))
 ;(terpri)
;(princ sumpdt)
 ;(terpri)
 (if (= idif 0) (setq slp 0) (setq slp (/ idif sumpdt)))
 ;(princ slp)
 ;(terpri)
 (setq d (- (nth i cdtp) (nth (- i 1) cdtp)))
 ;(princ d)
 ;(terpri)
 (setq invert (+ (nth (- i 1) ptssi) (* slp d)))
 ;(princ invert)
 (setq ptssi (append ptssi (list invert)))
 ;(terpri)
 ;(princ ptssi)
 )
 )
 (setq i(+ i 1))
 )
(setq ptssi (append  ptssi (list (last ptsip))))
 )
 ; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Excel functions used in Import/Export modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------------------------------------------------------------------------------
; GetExcel - Stores the values from an Excel spreadsheet into *ExcelData@ list
; Arguments: 3
;   ExcelFile$ = Path and filename
;   SheetName$ = Sheet name or nil for not specified
;   MaxRange$ = Maximum cell ID range to include or nil to get the current region from cell A1
; Syntax examples:
; (GetExcel "C:\\Temp\\Temp.xls" "Sheet1" "E19") = Open C:\Temp\Temp.xls on Sheet1 and read up to cell E19
; (GetExcel "C:\\Temp\\Temp.xls" nil "XYZ123") = Open C:\Temp\Temp.xls on current sheet and read up to cell XYZ123
;-------------------------------------------------------------------------------
(defun GetExcel (ExcelFile$ SheetName$ MaxRange$ / Column# ColumnRow@ Data@ ExcelRange^
  ExcelValue ExcelValue ExcelVariant^ MaxColumn# MaxRow# Range$ Row# Worksheet)
  (if (= (type ExcelFile$) 'STR)
    (if (not (findfile ExcelFile$))
      (progn
        (alert (strcat "Excel file " ExcelFile$ " not found."))
        (exit)
      );progn
    );if
    (progn
      (alert "Excel file not specified.")
      (exit)
    );progn
  );if
  (gc)
  (if (setq *ExcelApp% (vlax-get-object "Excel.Application"))
    (progn
      (alert "Close all Excel spreadsheets to continue!")
      (vlax-release-object *ExcelApp%)(gc)
    );progn
  );if
  (setq ExcelFile$ (findfile ExcelFile$))
  (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Open ExcelFile$)
  (if SheetName$
    (vlax-for Worksheet (vlax-get-property *ExcelApp% "Sheets")
      (if (= (vlax-get-property Worksheet "Name") SheetName$)
        (vlax-invoke-method Worksheet "Activate")
      );if
    );vlax-for
  );if
  (if MaxRange$
    (progn
      (setq ColumnRow@ (ColumnRow MaxRange$))
      (setq MaxColumn# (nth 0 ColumnRow@))
      (setq MaxRow# (nth 1 ColumnRow@))
    );progn
    (progn
      (setq CurRegion (vlax-get-property (vlax-get-property
        (vlax-get-property *ExcelApp% "ActiveSheet") "Range" "A1") "CurrentRegion")
      );setq
      (setq MaxRow# (vlax-get-property (vlax-get-property CurRegion "Rows") "Count"))
      (setq MaxColumn# (vlax-get-property (vlax-get-property CurRegion "Columns") "Count"))
    );progn
  );if
  (setq *ExcelData@ nil)
  (setq Row# 1)
  (repeat MaxRow#
    (setq Data@ nil)
    (setq Column# 1)
    (repeat MaxColumn#
      (setq Range$ (strcat (Number2Alpha Column#)(itoa Row#)))
      (setq ExcelRange^ (vlax-get-property *ExcelApp% "Range" Range$))
      (setq ExcelVariant^ (vlax-get-property ExcelRange^ 'Value))
      (setq ExcelValue (vlax-variant-value ExcelVariant^))
      (setq ExcelValue
        (cond
          ((= (type ExcelValue) 'INT) (itoa ExcelValue))
          ((= (type ExcelValue) 'REAL) (rtosr ExcelValue))
          ((= (type ExcelValue) 'STR) (vl-string-trim " " ExcelValue))
          ((/= (type ExcelValue) 'STR) "")
        );cond
      );setq
      (setq Data@ (append Data@ (list ExcelValue)))
      (setq Column# (1+ Column#))
    );repeat
    (setq *ExcelData@ (append *ExcelData@ (list Data@)))
    (setq Row# (1+ Row#))
  );repeat
  (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook") 'Close :vlax-False)
  (vlax-invoke-method *ExcelApp% 'Quit)
  (vlax-release-object *ExcelApp%)(gc)
  (setq *ExcelApp% nil)
  *ExcelData@
);defun GetExcel
;-------------------------------------------------------------------------------
; GetCell - Returns the cell value from the *ExcelData@ list
; Arguments: 1
;   Cell$ = Cell ID
; Syntax example: (GetCell "E19") = value of cell E19
;-------------------------------------------------------------------------------
(defun GetCell (Cell$ / Column# ColumnRow@ Return Row#)
  (setq ColumnRow@ (ColumnRow Cell$))
  (setq Column# (1- (nth 0 ColumnRow@)))
  (setq Row# (1- (nth 1 ColumnRow@)))
  (setq Return "")
  (if *ExcelData@
    (if (and (>= (length *ExcelData@) Row#)(>= (length (nth 0 *ExcelData@)) Column#))
      (setq Return (nth Column# (nth Row# *ExcelData@)))
    );if
  );if
  Return
);defun GetCell
;-------------------------------------------------------------------------------
; OpenExcel - Opens an Excel spreadsheet
; Arguments: 3
;   ExcelFile$ = Excel filename or nil for new spreadsheet
;   SheetName$ = Sheet name or nil for not specified
;   Visible = t for visible or nil for hidden
; Syntax examples:
; (OpenExcel "C:\\Temp\\Temp.xls" "Sheet2" t) = Opens C:\Temp\Temp.xls on Sheet2 as visible session
; (OpenExcel "C:\\Temp\\Temp.xls" nil nil) = Opens C:\Temp\Temp.xls on current sheet as hidden session
; (OpenExcel nil "Parts List" nil) =  Opens a new spreadsheet and creates a Part List sheet as hidden session
;-------------------------------------------------------------------------------
(defun OpenExcel (ExcelFile$ SheetName$ Visible / Sheet$ Sheets@ Worksheet)
  (if (= (type ExcelFile$) 'STR)
    (if (findfile ExcelFile$)
      (setq *ExcelFile$ ExcelFile$)
      (progn
        (alert (strcat "Excel file " ExcelFile$ " not found."))
        (exit)
      );progn
    );if
    (setq *ExcelFile$ "")
  );if
  (gc)
  (if (setq *ExcelApp% (vlax-get-object "Excel.Application"))
    (progn
      (alert "Close all Excel spreadsheets to continue!")
      (vlax-release-object *ExcelApp%)(gc)
    );progn
  );if
  (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  (if ExcelFile$
    (if (findfile ExcelFile$)
      (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Open ExcelFile$)
      (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Add)
    );if
    (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Add)
  );if
  (if Visible
    (vla-put-visible *ExcelApp% :vlax-true)
  );if
  (if (= (type SheetName$) 'STR)
    (progn
      (vlax-for Sheet$ (vlax-get-property *ExcelApp% "Sheets")
        (setq Sheets@ (append Sheets@ (list (vlax-get-property Sheet$ "Name"))))
      );vlax-for
      (if (member SheetName$ Sheets@)
        (vlax-for Worksheet (vlax-get-property *ExcelApp% "Sheets")
          (if (= (vlax-get-property Worksheet "Name") SheetName$)
            (vlax-invoke-method Worksheet "Activate")
          );if
        );vlax-for
        (vlax-put-property (vlax-invoke-method (vlax-get-property *ExcelApp% "Sheets") "Add") "Name" SheetName$)
      );if
    );progn
  );if
  (princ)
);defun OpenExcel
;-------------------------------------------------------------------------------
; PutCell - Put values into Excel cells
; Arguments: 2
;   StartCell$ = Starting Cell ID
;   Data@ = Value or list of values
; Syntax examples:
; (PutCell "A1" "PART NUMBER") = Puts PART NUMBER in cell A1
; (PutCell "B3" '("Dim" 7.5 "9.75")) = Starting with cell B3 put Dim, 7.5, and 9.75 across
;-------------------------------------------------------------------------------
(defun PutCell (StartCell$ Data@ / Cell$ Column# ExcelRange Row#)
  (if (= (type Data@) 'STR)
    (setq Data@ (list Data@))
  )
  (setq ExcelRange (vlax-get-property *ExcelApp% "Cells"))
  (if (Cell-p StartCell$)
    (setq Column# (car (ColumnRow StartCell$))
          Row# (cadr (ColumnRow StartCell$))
    );setq
    (if (vl-catch-all-error-p
          (setq Cell$ (vl-catch-all-apply 'vlax-get-property
            (list (vlax-get-property *ExcelApp% "ActiveSheet") "Range" StartCell$))
          );setq
        );vl-catch-all-error-p
        (alert (strcat "The cell ID \"" StartCell$ "\" is invalid."))
        (setq Column# (vlax-get-property Cell$ "Column")
              Row# (vlax-get-property Cell$ "Row")
        );setq
    );if
  );if
  (if (and Column# Row#)
    (foreach Item Data@
      (vlax-put-property ExcelRange "Item" Row# Column# (vl-princ-to-string Item))
      (setq Column# (1+ Column#))
    );foreach
  );if
  (princ)
);defun PutCell
;-------------------------------------------------------------------------------
; CloseExcel - Closes Excel spreadsheet
; Arguments: 1
;   ExcelFile$ = Excel saveas filename or nil to close without saving
; Syntax examples:
; (CloseExcel "C:\\Temp\\Temp.xls") = Saveas C:\Temp\Temp.xls and close
; (CloseExcel nil) = Close without saving
;-------------------------------------------------------------------------------
(defun CloseExcel (ExcelFile$ / Saveas)
  (if ExcelFile$
    (if (= (strcase ExcelFile$) (strcase *ExcelFile$))
      (if (findfile ExcelFile$)
        (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook") "Save")
        (setq Saveas t)
      );if
      (if (findfile ExcelFile$)
        (progn
          (vl-file-delete (findfile ExcelFile$))
          (setq Saveas t)
        );progn
        (setq Saveas t)
      );if
    );if
  );if
  (if Saveas
    (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook")
      "SaveAs" ExcelFile$ -4143 "" "" :vlax-false :vlax-false nil
    );vlax-invoke-method
  );if
  (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook") 'Close :vlax-False)
  (vlax-invoke-method *ExcelApp% 'Quit)
  (vlax-release-object *ExcelApp%)(gc)
  (setq *ExcelApp% nil *ExcelFile$ nil)
  (princ)
);defun CloseExcel
;-------------------------------------------------------------------------------
; ColumnRow - Returns a list of the Column and Row number
 
; Arguments: 1
;   Cell$ = Cell ID
; Syntax example: (ColumnRow "ABC987") = '(731 987)
;-------------------------------------------------------------------------------
(defun ColumnRow (Cell$ / Column$ Char$ Row#)
  (setq Column$ "")
  (while (< 64 (ascii (setq Char$ (strcase (substr Cell$ 1 1)))) 91)
    (setq Column$ (strcat Column$ Char$)
          Cell$ (substr Cell$ 2)
    );setq
  );while
  (if (and (/= Column$ "") (numberp (setq Row# (read Cell$))))
    (list (Alpha2Number Column$) Row#)
    '(1 1);default to "A1" if there's a problem
  );if
);defun ColumnRow
;-------------------------------------------------------------------------------
; Alpha2Number - Converts Alpha string into Number
 
; Arguments: 1
;   Str$ = String to convert
; Syntax example: (Alpha2Number "ABC") = 731 
;-------------------------------------------------------------------------------
(defun Alpha2Number (Str$ / Num#)
  (if (= 0 (setq Num# (strlen Str$)))
    0
    (+ (* (- (ascii (strcase (substr Str$ 1 1))) 64) (expt 26 (1- Num#)))
       (Alpha2Number (substr Str$ 2))
    );+
  );if
);defun Alpha2Number
;-------------------------------------------------------------------------------
; Number2Alpha - Converts Number into Alpha string
 
; Arguments: 1
;   Num# = Number to convert
; Syntax example: (Number2Alpha 731) = "ABC"
;-------------------------------------------------------------------------------
(defun Number2Alpha (Num# / Val#)
  (if (< Num# 27)
    (chr (+ 64 Num#))
    (if (= 0 (setq Val# (rem Num# 26)))
      (strcat (Number2Alpha (1- (/ Num# 26))) "Z")
      (strcat (Number2Alpha (/ Num# 26)) (chr (+ 64 Val#)))
    );if
  );if
);defun Number2Alpha
;-------------------------------------------------------------------------------
; Cell-p - Evaluates if the argument Cell$ is a valid cell ID
 
; Arguments: 1
;   Cell$ = String of the cell ID to evaluate
; Syntax examples: (Cell-p "B12") = t, (Cell-p "BT") = nil
;-------------------------------------------------------------------------------
(defun Cell-p (Cell$)
  (and (= (type Cell$) 'STR)
    (or (= (strcase Cell$) "A1")
      (not (equal (ColumnRow Cell$) '(1 1)))
    );or
  );and
);defun Cell-p
;-------------------------------------------------------------------------------
; Row+n - Returns the cell ID located a number of rows from cell
 
; Arguments: 2
;   Cell$ = Starting cell ID
;   Num# = Number of rows from cell
; Syntax examples: (Row+n "B12" 3) = "B15", (Row+n "B12" -3) = "B9"
;-------------------------------------------------------------------------------
(defun Row+n (Cell$ Num#)
  (setq Cell$ (ColumnRow Cell$))
  (strcat (Number2Alpha (car Cell$)) (itoa (max 1 (+ (cadr Cell$) Num#))))
);defun Row+n
;-------------------------------------------------------------------------------
; Column+n - Returns the cell ID located a number of columns from cell
 
; Arguments: 2
;   Cell$ = Starting cell ID
;   Num# = Number of columns from cell
; Syntax examples: (Column+n "B12" 3) = "E12", (Column+n "B12" -1) = "A12"
;-------------------------------------------------------------------------------
(defun Column+n (Cell$ Num#)
  (setq Cell$ (ColumnRow Cell$))
  (strcat (Number2Alpha (max 1 (+ (car Cell$) Num#))) (itoa (cadr Cell$)))
);defun Column+n
;-------------------------------------------------------------------------------
; rtosr - Used to change a real number into a short real number string
; stripping off all trailing 0's.
; Arguments: 1
;   RealNum~ = Real number to convert to a short string real number
; Returns: ShortReal$ the short string real number value of the real number.
;-------------------------------------------------------------------------------
(defun rtosr (RealNum~ / DimZin# ShortReal$)
  (setq DimZin# (getvar "DIMZIN"))
  (setvar "DIMZIN" 8)
  (setq ShortReal$ (rtos RealNum~ 2 8))
  (setvar "DIMZIN" DimZin#)
  ShortReal$
);defun rtosr
;-------------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dialag box functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (defun saveVars()
    (setq ModeC(atoi(get_tile "ModeC")))
	(setq ModeA(atoi(get_tile "ModeA")))
    (setq ModeM(atoi(get_tile "ModeM")))
	(setq ModeS(atoi(get_tile "ModeS")))
	(setq ModeI(atoi(get_tile "ModeI")))
	(setq vscale(distof(get_tile "vscale")))
	(setq hscale(distof(get_tile "hscale")))
	;(setq existingpipec(atoi(get_tile "existingpipec")))
	;(setq temporarypipec(atoi(get_tile "temporarypipec")))
	;(setq relocatedpipec(atoi(get_tile "relocatedpipec")))
	(setq diameter(atoi(get_tile "diameter")))
	(setq sstyle1 (atoi(get_tile "sstyle1")))
	(setq sstyle2 (atoi(get_tile "sstyle2")))
	(setq sstyle3 (atoi(get_tile "sstyle3")))
	(setq allowchainage (atoi(get_tile "allowchainage"))) 
	(setq allowchainageprofile (atoi(get_tile "allowchainageprofile")))
	(setq startchainage (distof(get_tile "startchainage")))
	(setq chainageinterval (distof(get_tile "chainageinterval")))
	(setq chtextsize (distof(get_tile "chtextsize")))
	(setq chstyle1 (atoi(get_tile "chstyle1")))
	(setq chstyle2 (atoi(get_tile "chstyle2")))
	(setq chstyle3 (atoi(get_tile "chstyle3")))
	(setq auto_anno_b (atoi(get_tile "auto_anno_b")))
	(setq angtol (distof(get_tile "angtol")))
	(setq copycros (atoi(get_tile "copycros"))) 
	(setq selpanno (atoi(get_tile "selpanno"))) 
	(setq npanno (atoi(get_tile "npanno"))) 
	(setq exportdata(atoi(get_tile "exportdata")))
  )
 ; =======================================================================================================
;; Popup  -  Lee Mac
;; A wrapper for the WSH popup method to display a message box prompting the user.
;; ttl - [str] Text to be displayed in the pop-up title bar
;; msg - [str] Text content of the message box
;; bit - [int] Bit-coded integer indicating icon & button appearance
;; Returns: [int] Integer indicating the button pressed to exit

(defun LM:popup ( ttl msg bit / wsh rtn )
    (if (setq wsh (vlax-create-object "wscript.shell"))
        (progn
            (setq rtn (vl-catch-all-apply 'vlax-invoke-method (list wsh 'popup msg 0 ttl bit)))
            (vlax-release-object wsh)
            (if (not (vl-catch-all-error-p rtn)) rtn)
        )
    )
)
 ; =======================================================================================================
 (defun aboutfunc()
  ;(alert "WP (Pre-Release VER. 1.22 Date: 05-02-2017) \nAutoLisp to create Pressurized Lines Profiles \n\nDesigned and Created by Ibrahim El Sharr \nContact: ibrahim.elshar@gmail.com \n\nCopyright \U+00A9 2017. All rights reserved.")
  (LM:popup "About" "WP (Pre-Release VER. 1.0 Date: 05-02-2017) \nAutoLisp to create Pressurized Lines Profiles \n\nDesigned and Created by Ibrahim El Shar \nContact: ibrahim.elshar@gmail.com \Website: ibrahim-elshar.com \n\nCopyright © 2017. All rights reserved." (+ 0 ))

 )
 ; =======================================================================================================
(defun changemode()
    (if(= (get_tile "ModeI") "1")
	(progn
      (mode_tile "diameter" 1)
      (mode_tile "exportdata" 1) 
      (mode_tile "auto_anno_b" 1) 
      (mode_tile "angtol" 1) 
	  (mode_tile "copycros" 1) 
	  (mode_tile "selpanno" 1)
	  (mode_tile "npanno" 1)
   
    )
	(progn
      (mode_tile "diameter" 0)
      (mode_tile "exportdata" 0) 
      (mode_tile "auto_anno_b" 0) 
      (mode_tile "angtol" 0) 	  
    )
	)
	(if(= (get_tile "ModeS") "0") (progn (mode_tile "copycros" 1) (mode_tile "selpanno" 1) (mode_tile "npanno" 1))  (progn (mode_tile "copycros" 0) (mode_tile "selpanno" 0) (mode_tile "npanno" 0)))
	(if (and (= (get_tile "ModeA") "0") (= (get_tile "ModeC") "0") (= (get_tile "ModeM") "0") )
	(progn 
	  (mode_tile "allowchainage" 1)
	  (mode_tile "allowchainageprofile" 1)
      (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 1)
	  (mode_tile "chtextsize" 1)
      (mode_tile "chainagestyle" 1)
	  (mode_tile "auto_anno_b" 1)
	  (mode_tile "angtol" 1)  
	) 
	  (progn
	 (if (= (get_tile "ModeM") "0")
	 (progn
	 (mode_tile "auto_anno_b" 0)
	  (if (and (= (get_tile "allowchainage") "1")  (= (get_tile "allowchainageprofile") "1"))
	  (progn  
       (mode_tile "allowchainage" 0)
	   (mode_tile "allowchainageprofile" 0)
	   (mode_tile "startchainage" 0)
      (mode_tile "chainageinterval" 0)
      (mode_tile "chainagestyle" 0)
	  (mode_tile "chtextsize" 0)
	  (set_tile "chainagestyle" "1")
	  )
	  )
	  (if (and (= (get_tile "allowchainage") "1")  (= (get_tile "allowchainageprofile") "0"))
	  (progn  
       (mode_tile "allowchainage" 0)
	   (mode_tile "allowchainageprofile" 0)
	   (mode_tile "startchainage" 0)
      (mode_tile "chainageinterval" 0)
      (mode_tile "chainagestyle" 0)
	  (mode_tile "chtextsize" 0)
	  (set_tile "chainagestyle" "1")
	  )
	  )
	  (if (and (= (get_tile "allowchainage") "0")  (= (get_tile "allowchainageprofile") "1"))
	  (progn  
       (mode_tile "allowchainage" 0)
	   (mode_tile "allowchainageprofile" 0)
	   (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 0)
      (mode_tile "chainagestyle" 0)
	  (mode_tile "chtextsize" 1)
	  (set_tile "chainagestyle" "1")
	  )
	  )
	  (if (and (= (get_tile "allowchainage") "0")  (= (get_tile "allowchainageprofile") "0"))
	  (progn  
       (mode_tile "allowchainage" 0)
	   (mode_tile "allowchainageprofile" 0)
	   (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 1)
      (mode_tile "chainagestyle" 1)
	  (mode_tile "chtextsize" 1)
	  ;(set_tile "chainagestyle" "1")
	  )
	  )
	  )
	  (progn 
	  (mode_tile "auto_anno_b" 1)
	  (mode_tile "angtol" 1)
	  (if (= (get_tile "allowchainageprofile") "1")
	  (progn
	  ;(set_tile "allowchainage" "0")
	   (mode_tile "allowchainage" 1)
	   (mode_tile "allowchainageprofile" 0)
	   (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 0)
      (mode_tile "chainagestyle" 0)
	  (mode_tile "chtextsize" 1)
	  (set_tile "chainagestyle" "1")
	  )
	  (progn 
	  ;(set_tile "allowchainage" "0")
	  (mode_tile "allowchainage" 1)
	   (mode_tile "allowchainageprofile" 0)
	   (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 1)
      (mode_tile "chainagestyle" 1)
	  (mode_tile "chtextsize" 1)
	  (set_tile "chainagestyle" "1")
	  )
	  )
	  )
     )
)
)	 
	 
	 
  )
; =======================================================================================================	
	(defun changechainagemode()
	(if (or (= (get_tile "ModeA") "1") (= (get_tile "ModeC") "1") (= (get_tile "ModeM") "1") )
    (if (= (get_tile "ModeM") "0")
	(progn
	(cond
	( (and  (= (get_tile "allowchainage") "0") (= (get_tile "allowchainageprofile") "0") ) 
	  (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 1)
	  (mode_tile "chtextsize" 1)
      (mode_tile "chainagestyle" 1)
    )
	((and  (= (get_tile "allowchainage") "1") (= (get_tile "allowchainageprofile") "0") )
	  (mode_tile "startchainage" 0)
      (mode_tile "chainageinterval" 0)
	  (mode_tile "chtextsize" 0)
      (mode_tile "chainagestyle" 0)
	  (set_tile "chstyle1" (cond ((= (get_tile "chstyle1") "1") "1") (t "0")))
	  (set_tile "chstyle2" (cond ((= (get_tile "chstyle2") "1") "1") (t "0")))
	  (set_tile "chstyle3" (cond ((= (get_tile "chstyle3") "1") "1") (t "0")))
	  (if (and (= (get_tile "chstyle1") "0") (= (get_tile "chstyle2") "0") (= (get_tile "chstyle3") "0"))(set_tile "chstyle2" "1"))
   	)
	((and  (= (get_tile "allowchainage") "0") (= (get_tile "allowchainageprofile") "1") )
	  (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 0)
	  (mode_tile "chtextsize" 1)
      (mode_tile "chainagestyle" 0)
	  (set_tile "chstyle1" (cond ((= (get_tile "chstyle1") "1") "1") (t "0")))
	  (set_tile "chstyle2" (cond ((= (get_tile "chstyle2") "1") "1") (t "0")))
	  (set_tile "chstyle3" (cond ((= (get_tile "chstyle3") "1") "1") (t "0")))
	  (if (and (= (get_tile "chstyle1") "0") (= (get_tile "chstyle2") "0") (= (get_tile "chstyle3") "0"))(set_tile "chstyle2" "1"))
    )
	((and  (= (get_tile "allowchainage") "1") (= (get_tile "allowchainageprofile") "1") )
	  (mode_tile "startchainage" 0)
      (mode_tile "chainageinterval" 0)
	  (mode_tile "chtextsize" 0)
      (mode_tile "chainagestyle" 0)
	  (set_tile "chstyle1" (cond ((= (get_tile "chstyle1") "1") "1") (t "0")))
	  (set_tile "chstyle2" (cond ((= (get_tile "chstyle2") "1") "1") (t "0")))
	  (set_tile "chstyle3" (cond ((= (get_tile "chstyle3") "1") "1") (t "0")))
	  (if (and (= (get_tile "chstyle1") "0") (= (get_tile "chstyle2") "0") (= (get_tile "chstyle3") "0"))(set_tile "chstyle2" "1"))
    )
	)
	)
	(progn 
	; (set_tile "auto_anno_b" "0")
	; (mode_tile "auto_anno_b" 1)
	; (mode_tile "angtol" 1)
	(if (= (get_tile "allowchainageprofile") "1")
	(progn
	  (set_tile "allowchainage" "0")
	  (mode_tile "allowchainage" 1)
	  (mode_tile "allowchainageprofile" 0)
	  (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 0)
      (mode_tile "chainagestyle" 0)
	  (mode_tile "chtextsize" 1)
	  (set_tile "chstyle1" (cond ((= (get_tile "chstyle1") "1") "1") (t "0")))
	  (set_tile "chstyle2" (cond ((= (get_tile "chstyle2") "1") "1") (t "0")))
	  (set_tile "chstyle3" (cond ((= (get_tile "chstyle3") "1") "1") (t "0")))
	  (if (and (= (get_tile "chstyle1") "0") (= (get_tile "chstyle2") "0") (= (get_tile "chstyle3") "0"))(set_tile "chstyle2" "1"))
	  )
	  (progn 
	  (set_tile "allowchainage" "0")
	   (mode_tile "allowchainage" 1)
	  (mode_tile "allowchainageprofile" 0)
	  (mode_tile "startchainage" 1)
      (mode_tile "chainageinterval" 1)
      (mode_tile "chainagestyle" 1)
	  (mode_tile "chtextsize" 1)
	  )
	  )
	)
    )
	)
	)
; =======================================================================================================	
(defun changeautoanno()
(if (or (= (get_tile "ModeA") "1") (= (get_tile "ModeC") "1") )
	(progn
	 (mode_tile "auto_anno_b" 0)
	 (if (= (get_tile "auto_anno_b") "1") (mode_tile "angtol" 0) (mode_tile "angtol" 1) )
	 (cond
	 ((and (>= (atoi (get_tile "diameter")) 1) (<= (atoi (get_tile "diameter")) 160))   (set_tile "angtol" "5"))
	 ((and (>= (atoi (get_tile "diameter")) 200) (<= (atoi (get_tile "diameter")) 300))   (set_tile "angtol" "4"))
	 ((and (>= (atoi (get_tile "diameter")) 350) (<= (atoi (get_tile "diameter")) 600))   (set_tile "angtol" "3"))
	 ((and (>= (atoi (get_tile "diameter")) 700) (<= (atoi (get_tile "diameter")) 800))   (set_tile "angtol" "2"))
	 ((and (>= (atoi (get_tile "diameter")) 900) (<= (atoi (get_tile "diameter")) 1100))   (set_tile "angtol" "1.5"))
	 ((and (>= (atoi (get_tile "diameter")) 1200) (<= (atoi (get_tile "diameter")) 1800))   (set_tile "angtol" "1.5"))
	 (t (set_tile "angtol" "1.5"))
	 )
	)
	(progn
	(mode_tile "auto_anno_b" 1)
	 (mode_tile "angtol" 1) 
	 (cond
	 ((and (>= (atoi (get_tile "diameter")) 1) (<= (atoi (get_tile "diameter")) 160))   (set_tile "angtol" "5"))
	 ((and (>= (atoi (get_tile "diameter")) 200) (<= (atoi (get_tile "diameter")) 300))   (set_tile "angtol" "4"))
	 ((and (>= (atoi (get_tile "diameter")) 350) (<= (atoi (get_tile "diameter")) 600))   (set_tile "angtol" "3"))
	 ((and (>= (atoi (get_tile "diameter")) 700) (<= (atoi (get_tile "diameter")) 800))   (set_tile "angtol" "2"))
	 ((and (>= (atoi (get_tile "diameter")) 900) (<= (atoi (get_tile "diameter")) 1100))   (set_tile "angtol" "1.5"))
	 ((and (>= (atoi (get_tile "diameter")) 1200) (<= (atoi (get_tile "diameter")) 1800))   (set_tile "angtol" "1.5"))
	 (t (set_tile "angtol" "1.5"))
	 )
	)
)
)
; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Style functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esttexto	()
  (vl-cmdf "._style" "WP-TEXT" "romans" chtextsize 0.80 0 "n" "n" "n") ;chtextsize
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input modes functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Survey Contour mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getlayname (/ selset lyrset n ent)
  (setq contourstest nil)
  (while (= contourstest nil)
    (prompt "\nSelect one or more entities to get Contours Layer(s) : ")
    (setq selset (ssget "_:L" '((0 . "*LINE")))) ;(setq selset (ssget "_X" (list (cons 0 "*LINE"))))
    (setq lyrset (list))
    (repeat (setq n (sslength selset))
      (setq ent (ssname selset (setq n (1- n))))
      (setq lyrset (append lyrset (list (vla-get-layer (vlax-ename->vla-object ent)))))
    )
    (setq lyrset (LM:Unique lyrset))
	;(princ lyrset)
    (setq contourstest
	   (ssget "_X"
		  (append '((0 . "*LINE") (-4 . "<OR")) (mapcar '(lambda (n) (cons 8 n)) lyrset) '((-4 . "OR>")))
	   )
    )
  )
)
; =======================================================================================================
(defun activexsupport ()
  (vl-load-com)
  (setq	*modelspace*
	 (vla-get-modelspace
	   (vla-get-activedocument (vlax-get-acad-object))
	 )
  )
)
; =======================================================================================================
;get the pipe polyline
(defun getha ()
  ;; this entity must be a lwpolyline
  (activexsupport)
  (setq
    ha (entsel "\nSelect the Pipe Polyline: ")
  )
  (while (= ha nil)
    (progn
      (princ "\nNothing selected...")
      (setq ha
	     (entsel "\nSelect the Pipe Polyline: ")
      )
    )
  )
  (setq ha-type (cdr (assoc 0 (entget (car ha)))))
  (if (not (equal ha-type "LWPOLYLINE"))
    (progn
      (setq ha nil)
      (princ "\n***Pipe Polyline: must be a LWPolyline***")
    )
  )
  (while (= ha nil)
    (progn
      (princ "\nNothing selected...")
      (setq ha
	     (entsel "\nSelect the Pipe Polyline: ")
      )
      (setq ha-type (cdr (assoc 0 (entget (car ha)))))
      (if (not (equal ha-type "LWPOLYLINE"))
	(progn
	  (setq ha nil)
	  (princ "\n***Select the Pipe Polyline:***")
	)
      )
    )
  )
  (setq ha-ename (entget (car ha)))
  (setq ha-ename (cdr (assoc -1 ha-ename)))
  (setq ha-object (vlax-ename->vla-object ha-ename))

  ; (vl-cmdf "._text"
	   ; (vlax-curve-getstartpoint ha-object)
	   ; "0"
	   ; "A"
  ; )
  ; (vl-cmdf "._text"
	   ; (vlax-curve-getendpoint ha-object)
	   ; "0"
	   ; "B"
  ; )
  (if (= auto_anno_b 1) (id_pline_ang ha-ename))
  ;(setq ang_list (id_pline_ang ha-ename))
  ;(princ ang_list)
;;;;;DRAW CHAINAGE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;(setq ss (car (entsel "\n Select :"))) ; (gets entity name) Prompts the user to select a single 
 ; object (entity) by specifying a point. Return Values Type: List or nil. A list whose first
 ; element is the entity name of the chosen object and whose second element is the coordinates
 ; (in terms of the current UCS) of the point used to pick the object.
 (if (or (= allowchainage 1) (= allowchainageprofile 1))
(progn
(setq l1 (car ha))
(COMMAND "LAYER" "SET" "C-ANNO-CHNG" "")
(if (= chstyle1 1) (setq chst 1))
(if (= chstyle2 1) (setq chst 2))
(if (= chstyle3 1) (setq chst 3))
(esttexto)
(CHMARKS l1 startchainage (getvar "TEXTSIZE") chainageinterval chst n3 n4 )
)
)
)
; =======================================================================================================
(defun listptintersect ()
  (setq ve 1) ;Vertical Exaggeration
  (setq listaxy nil)
  (setq hazvalue (caddr (vlax-curve-getStartPoint ha-object)))
  (setq curvas contourstest)
  (setq ncurvas (sslength curvas))
  (setq listaxy nil)
  (setq counter 0)
  (while (< counter ncurvas)
    (progn
      (setq cnivel-ename (ssname curvas counter)) ;(ssname ss index) ssname: Returns the object (entity) name of the indexed element of a selection set   (setq ent1 (ssname ss 0)) <Entity name: 1d62d68>
      (setq cnivel-object (vlax-ename->vla-object cnivel-ename)); vlax-ename->vla-object Transforms an entity to a VLA-object  (setq e (car (entsel)))  <Entity name: 27e0540> (vlax-ename->vla-object e)  #<VLA-OBJECT IAcadLWPolyline 03f713a0>
      ;get the z of an object
      (setq cnivelzvalue
	     (caddr (vlax-curve-getStartPoint cnivel-object)) ; vlax-curve-getStartPoint Returns the start point (in WCS) of the curve
      )

      (setq ha-ENTITY
	     (subst (cons 38 cnivelzvalue)  ;subst Searches a list for an old item and returns a copy of the list with a new item substituted in place of every occurrence of the old item (subst newitem olditem lst)
                                        ;cons Adds an element to the beginning of a list, or constructs a dotted list  (cons new-first-element list-or-atom)
		    (assoc 38 (entget (car ha))) ;assoc Searches an association list for an element and returns that association list entry (assoc element alist)
		    (entget (car ha))            ;Retrieves an object's (entity's) definition data (entget ename [applist])
	     )
      )
      (entmod ha-ENTITY) ;entmod Modifies the definition data of an object (entity) (entmod elist)

      (setq intersectpt
	     (vlax-variant-value    ;Returns the value of a variant (vlax-variant-value var)
	       (vlax-invoke-method   ;vlax-invoke-method  Calls the specified ActiveX method (vlax-invoke-method obj method arg [arg...])
		 ha-object
		 "IntersectWith"
		 cnivel-object
		 acExtendNone
	       )
	     )
      )

      (setq test nil)
      (setq
	test (vl-catch-all-apply
	       'vlax-safearray->list
	       (list intersectpt)
	     )
      )
      (setq errorrwpc (vl-catch-all-error-p test))

      (if (= errorrwpc nil) ;(/= error t)
	(progn
	  (setq intersectpt (vlax-safearray->list intersectpt))
	  (setq interlength (length intersectpt))

	  (if (> interlength 3)
	    (progn
	      (setq dividelength (/ interlength 3))
	      (setq count 0)
	      (while (< count interlength)
		(progn
		  (setq	newpt (list (nth count intersectpt)
				    (nth (+ count 1) intersectpt)
				    (nth (+ count 2) intersectpt)
			      )
		  )
        
		   (setq x (vlax-curve-getdistatPoint ha-ename newpt))
		   (setq z (caddr intersectpt))
		   (setq xy (list x (* z ve)))
		   (setq
		    listaxy (append listaxy (list xy))
		   )

		  (setq count (+ count 3))
		)
	      )
	    )
	    (progn
	      (setq x (vlax-curve-getdistatPoint ha-ename intersectpt))
	      (setq z (caddr intersectpt))
	      (setq xy (list x (* z ve)))
	      (setq
		listaxy	(append listaxy (list xy))
	      )
	    )
	  )

	  (setq	ha-ENTITY
		 (subst	(cons 38 hazvalue)
			(assoc 38 (entget (car ha)))
			(entget (car ha))
		 )
	  )
	  (entmod ha-ENTITY)
	)
      )
      (setq counter (1+ counter))
    )
  )

  (setq	listaxy
	 (vl-sort listaxy
		  (function (lambda (e1 e2)
			      (< (car e1) (car e2))
			    )
		  )
	 )
  )
  (if (<= (length listaxy) 1) (progn (alert "Error: Not enough contour lines.\nMake sure enough contour polylines are selected to get all contour layers.") (quit)))
  (setq	startdist (vlax-curve-getdistatPoint
		    ha-ename
		    (vlax-curve-getstartpoint ha-ename)
		  )
	enddist	  (vlax-curve-getdistatPoint
		    ha-ename
		    (vlax-curve-getendpoint ha-ename)
		  )
  )

  (setq	pt1 (car (car listaxy))
	pt2 (car (last listaxy))
  )

  (if (/= startdist pt1)
    (progn
      (setq x startdist)
      (setq y (+ (* (/ (- (cadr (car listaxy)) (cadr (cadr listaxy)));;;;;error here
		       (- (car (cadr listaxy)) (car (car listaxy)))
		    )
		    (- (car (car listaxy)) startdist)
		 )
		 (cadr (car listaxy))
	      )
      )
      (setq xy (list x y))
      (setq
	listaxy	(append listaxy (list xy))
      )
      (setq listaxy
	     (vl-sort listaxy
		      (function	(lambda	(e1 e2)
				  (< (car e1) (car e2))
				)
		      )
	     )
      )

    )
  )

  (if (/= enddist pt1)
    (progn
      (setq pos (1- (length listaxy)))
      (setq x enddist)
      (setq y
	     (+
	       (*
		 (/ (- (cadr (nth pos listaxy))
		       (cadr (nth (1- pos) listaxy))
		    )
		    (- (car (nth pos listaxy)) (car (nth (1- pos) listaxy)))
		 )
		 (- enddist (car (nth pos listaxy)))
	       )
	       (cadr (nth pos listaxy))
	     )
      )
      (setq xy (list x y))
      (setq
	listaxy	(append listaxy (list xy))
      )
      (setq listaxy
	     (vl-sort listaxy
		      (function	(lambda	(e1 e2)
				  (< (car e1) (car e2))
				)
		      )
	     )
      )

    )
  )
)
; =======================================================================================================
(defun contourmode () 
  ;(esttexto)
  (getha)
  (setq e (entget ha-ename)); get the entity list
  (setq tt 1)
  (if ( assoc 62 e) ;if color not bylayer
  (progn
  (setq tt 0)
  (setq pipecolor (cdr (assoc 62 e)))))  ; get the color
  (if  (= tt 1)  ; if bylayer
  (progn
  (setq layer (cdr (assoc 8 e))) ;retrieve the layer name
  (setq layerinf (tblsearch "LAYER" layer)) ;get the layer data
  (setq pipecolor (cdr (assoc 62 layerinf) )) ;extract the default layer color
   ))
  (getlayname)
  (princ)
  ;(getexaggeration)
  (listptintersect)
  
  ;(princ listaxy)
)
; =======================================================================================================
 ;;;;;;;should be replace if mode_prof_chainage is zero or off
 (defun addchpr ()
 (setq dupflag 0)
 (setq icdlist cdlist)
 (setq i0cdlist(cons '0 cdlist))
 (setq i 0)
 (setq bdnames nil)
 (while (< i  (length i0cdlist) )
 (setq bdname (list (list (nth i bendnames) (nth i i0cdlist))))
 (setq bdnames (append bdnames bdname))
 (setq i (+ i 1))
 )
 ;(princ bdnames)
 (setq cdlist (append cdlist  (RemoveNth 0 chn)))
 (setq cdlist(cons '0 cdlist))
 (setq cdlist (vl-sort cdlist '<))
 (setq cdlist (LM:Unique cdlist))
 (setq i 0)
 (setq cdlistaa cdlist)
 (while (< i (-(length cdlistaa) 2))
 (if (< (- (nth (+ i 1) cdlist) (nth i cdlist)) 0.01) 
 (progn
 (setq cdlista (if (/= (member (nth i cdlist) chn) nil) (RemoveNth i cdlist) (RemoveNth (+ i 1) cdlist)))
 (setq dupflag 1)
 )
 )
 (setq i (+ i 2))
 )
 (if (= dupflag 0) (setq cdlista cdlist))
 (setq cdlist cdlista)
 
 
 ;(princ cdlist)
 (setq i 0)
   ; (terpri)
   ;(princ i0cdlist)
 (setq chnames nil)
 (while (< i (-(length cdlist) 1))
   ;(terpri)
  ; (princ (nth i cdlist))
   ;(terpri)
   ;(princ (member (nth i cdlist) chn))
   ;(terpri)
   ;(princ (member (nth i cdlist) i0cdlist))
   ;(terpri)
  (if (and (/= (member (nth i cdlist) chn) nil) (= (member (nth i cdlist) i0cdlist) nil) ) (progn
  (setq k (vl-position (nth i cdlist) chn))
  (setq chname (nth k chtext))
  ;(terpri)
  ;(princ (strcat "hereeeeeeeeeeeee"))
  ;(princ (nth k chn)  )
  ;(princ (nth i cdlist))
 ; (princ (strcat "hereeeeeeeeeeeee"))
  ;(terpri)
  (if (= (abs (- (nth k chn)  (nth i cdlist))) 0);;;;;;;;;;;;;;;;;;;
  (progn;;;;;;;;;;;;;;;;;
  (setq chname (list (list chname (nth i cdlist))))
  (setq chnames (append chnames chname))
  );;;;;;;;;;
  );;;;;;;;;;;
  ) 
  )
  (setq i (+ i 1))
  )
 ;(princ chnames)
 (setq bendnames (append bdnames chnames))
 (setq bendnamess (vl-sort bendnames (function (lambda (e1 e2) (< (cadr e1) (cadr e2)) ) ) ))
  (setq ci 0)
  (setq bendnames nil)
  (while (<= ci (- (length bendnamess) 1))
  (setq bendname (car (nth ci bendnamess)))
  (setq bendname (list bendname))
   (setq bendnames (append bendnames bendname))
   (setq ci (+ ci 1))
  )
  ;(terpri)
 ;(princ bendnames)
 
 )
; =======================================================================================================
(defun calc_cdlist ()
  (setq i 0) 
  (setq pdt nil)
  (setq CD 0) 
  (setq cdlist nil)
  (while (<= i (- (length pipecoords) 4))
  (setq xp1 (nth i pipecoords))
  (setq yp1 (nth (+ i 1) pipecoords))
  (setq xp2 (nth (+ i 2) pipecoords))
  (setq yp2 (nth (+ i 3) pipecoords))
  (setq p1 (list xp1 yp1))
  (setq p2 (list xp2 yp2))
  (setq d (distance  p1  p2 ))
  (setq d1 (list d))
  (setq pdt (append pdt d1))
  (setq CD (+ CD d))
  (setq CD1pi (list CD))
  (setq cdlist (append cdlist CD1pi))
  (setq i (+ i 2))
  )
 ;(terpri)
 ;(princ pdt)
  ;(terpri)
 ;(princ CD)
  ;(terpri)
 ;(princ cdlist)
  ;(terpri)
  (setq cdlist_for_mcm cdlist)
 (princ)
 )
 ; =======================================================================================================
 (defun calc_ch_gr ()
   (setq ptsgati nil)
  (setq k 0)
  (setq i 0)
  (while (<= k (length cdlist)) 
  (setq i 0)
  (setq distinv (nth k cdlist))
  ;(princ cdlist)
  ;(terpri)
  ;(princ distinv)
  ;(terpri)
 	    (while (<= i (- (length ptsg) 1))
		;(princ (nth i ptsg) ) (princ (nth (+ i 1) ptsg))
		  (if (and (<= (nth i gcdlist)  distinv) (>= (nth (+ i 1) gcdlist) distinv)) 
		    (PROGN 
			  (setq xp1 (nth i gcdlist))
              (setq yp1 (nth i ptsg))
              (setq xp2 (nth (+ i 1) gcdlist))
              (setq yp2 (nth (+ i 1) ptsg))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq gradelevel (+ (* slp  distinv) b))
			  ;(terpri)
              ;(princ gradelevel)
			  (setq gradelevel (list gradelevel))
              (setq ptsgati (append ptsgati gradelevel))
			  (setq i (length ptsg))
              ;(princ ptsgati)			  
		     )
		  )		  
			  (setq i (+ i 1))
	     )
	(setq k (+ k 1))	 
  )
  ;(terpri)
  ;(princ ptsgati)
  ;(terpri)
  (setq gcdlist (RemoveNth 0 gcdlist))
  )
; =======================================================================================================
 (defun ptinv ()
 ;(setq ss (vlax-ename->vla-object ha-ename)) 
 ;use this to change from entity name to ObjectID to be used in visual lisp
 (setq pipecoords
 (vlax-safearray->list
 (vlax-variant-value
 (vla-get-coordinates ha-object))))
 (setq n 0)                                               ;set up counter
 ;while not at the end of the list do...
 (while (not (equal (setq item (nth n pipecoords)) nil))
 ;(setq n (1+ n) ) 
 (setq n (+ n 1))    ;add 1 t counter                             
   );end WHILE
   ;(princ n)
   ;(princ pipecoords)
   (setq ptsg nil)
   (setq gcdlist nil)
  (setq ci 0)
  (setq nlistaxy (length listaxy))
  (while (<= ci (- nlistaxy 1))
  (setq gradelevel (cadr (nth ci listaxy)))
  (setq ptg (list gradelevel))
   (setq ptsg (append ptsg ptg))
   (setq gdist (car (nth ci listaxy)))
  (setq gcdl (list gdist))
   (setq gcdlist (append gcdlist gcdl))
   (setq ci (+ ci 1))
  )
 ; (princ ptsg)
  ;(princ gcdlist)
  ;pdtg
  (setq pdtg nil)
  (setq ci 0)
  (while (<= ci (- nlistaxy 2))
  (setq gdist (- (car (nth (+ ci 1) listaxy)) (car (nth ci listaxy)) ))
  (setq gdist (list gdist))
  (setq pdtg (append pdtg gdist))
  (setq ci (+ ci 1))
  )
  ;(terpri)
 ;(princ pdtg)
 ;(terpri)
 (setq i 0)
 (setq ptsi nil)
 (setq bendnames nil)
 (setq flagii 0)
 (setq flagiii 0)
 (setq z11 0)
 (setq z22 0)
 (COMMAND "LAYER" "SET" "C-PRFL-TEXT" "")
 (setq kk 0)
 (setq bkk nil)
 (setq kkk 0)
 (if (= auto_anno_b 0)
 (while (<= i (- n 1))
   (if (= i 2) (progn (setq flagii 0) (setq flagiii 0)))
   (setq xp1 (nth i pipecoords))
   (setq yp1 (nth (+ i 1) pipecoords))
   (setq p1 (list xp1 yp1))
   (command "circle" p1 1 )
   (setq zp1 (mapcar '+ p1 '( -20 20 0)))
   (setq zp2 (mapcar '+ p1 '( 20 -20 0)))
   (COMMAND "ZOOM" "W" zp1 zp2)
   (if (= i 0)(setq bendname (getstring T "\nSpecify Point Name [BGV/BFV/ARV/WASHOUT]: ")) (setq bendname (getstring T "\nSpecify Point Name [Undo/BGV/BFV/ARV/WASHOUT]: ")))
   (if (and (OR(= bendname "u") (= bendname "U") (= bendname "u ") (= bendname "U ")) (/= i 0))
   (progn 
   (command "erase" "last" "") 
   ;(princ (- (/ i 2) 1)) 
   (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
   (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))  
   ;(princ bendnames) (princ ptsg)   (princ ptsi)
   (setq i (- i 2)) )     
   (progn 
   (setq z00 0)
   (if (and (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (/= i 0))
   (while (= z00 0)
	(setq bendname (getstring T "\nSpecify Point Name [Undo/BGV/BFV/ARV/WASHOUT]: "))
  	(if (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (setq z00 1) ) 
    ))
        
   (setq z11 1)
   (setq z22 0)
        
   (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor))  (progn
    (if (or (= i 0) (= i (- n 2))) (progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : "))) 
	(progn
	(if (and (= flagii 0) (= flagiii 0)) 
	(progn 
	(initget "k K a A")
	(setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	(if (= invertlevel nil)(setq flagii 1) 
	(setq flagiii 1))) 
    (if (= flagiii 1) 
	(progn 
	(while (= z22 0)
	(progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: "))  (if  (= invertlevel nil) (setq invertlevel "k")))
  	(if (/= invertlevel nil) (setq z22 1) )
	)
    )
	)
	)
   (if (= flagii 1) (setq invertlevel nil))
	)))
   (progn
     (if (or (= i 0) (= i (- n 2)))(progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : ")))  
	 (progn
	  (if (and (= flagii 0) (= flagiii 0)) 
	  (progn 
	(initget "k K a A")
	  (setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	(if (= invertlevel nil)(setq flagii 1) 
	(setq flagiii 1))) 
	  (if (= flagiii 1) (progn 
	  (while (= z22 0)
	  (progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: "))  (if  (= invertlevel nil) (setq invertlevel "k")))
  	  (if (/= invertlevel nil) (setq z22 1) )
	  )
      )
    	)
		)
	 (if (= flagii 1) 
	 (setq invertlevel nil))
	 )))
	 ; (progn
	 ; (setq z22 0)
	 ; (while (= z22 0)
     ; (setq invertlevel (getreal "\nSpecify Invert Level : "))
  	 ; (if (/= invertlevel nil) (setq z22 1) )
	 ; )
	 ; )
									
    )
   (command "erase" "last" "")
   (setq bendname (list bendname))
   (setq bendnames (append bendnames bendname))
   (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel nil))
   (setq pti (list invertlevel))
   (setq ptsi (append ptsi pti))
   ;(princ bendnames)
   ;(princ ptsg)
   ;(princ ptsi) 
   (setq i (+ i 2))))
   (if (>= i (- n 1)) (progn (setq zz1 0) 
   (while (= zz1 0) 
   (initget "g G")
   (setq confirm (getstring "\nPress ENTER to confirm input or [GoBack]: "))
   (if (or (= confirm "") (= confirm " ") (= confirm "g")  (= confirm "G")) (setq zz1 1) ))  
   (if (or (= confirm "g") (= confirm "G")) 
   (progn 
   (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
   (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))(setq i (- i 2))  ))  ))   
   )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (while (<= i (- n 1))
   (if (= i 2) (progn (setq flagii 0) (setq flagiii 0)))
   (setq xp1 (nth i pipecoords))
   (setq yp1 (nth (+ i 1) pipecoords))
   (setq p1 (list xp1 yp1))
   (command "circle" p1 1 )
   (setq zp1 (mapcar '+ p1 '( -20 20 0)))
   (setq zp2 (mapcar '+ p1 '( 20 -20 0)))
   (COMMAND "ZOOM" "W" zp1 zp2)
   (if (= i 0)(progn (setq bkk nil) (setq kk 0) (setq kkk 0)(setq bendname (getstring T "\nSpecify Point Name [BGV/BFV/ARV/WASHOUT]: ")))) 
   (if (and (> i 0) (< i (- n 2))) 
   (if (= (nth (/ i 2) bend_angles) nil)
   (progn
  (setq bkk (append bkk (list kk)))
  (setq kk 0)
  (setq kkk (+ kkk 1))
   (setq bendname (getstring T "\nSpecify Point Name [Undo/BGV/BFV/ARV/WASHOUT]: ")) 
   ) 
   (progn 
   (setq bendname (nth (/ i 2) bend_angles))
   )
   ) )
   (if (= i (- n 2)) 
   (progn 
  (setq bkk (append bkk (list kk)))
  (setq kkk (+ kkk 1))
   (setq bendname (getstring T "\nSpecify Point Name [Undo/BGV/BFV/ARV/WASHOUT]: "))
   )
   )
   (if (and (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (/= i 0))
   (progn 
    (command "erase" "last" "") 
    (repeat (nth (- kkk 1) bkk)
    (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
    (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))  
    (setq i (- i 2)) )
	(setq kkk (- kkk 1))
	(setq bkk (RemoveNth kkk bkk ))  
   (setq kkk (- kkk 1))
    )   
   (progn 
   (setq kk (+ kk 1))
   (setq z00 0)
   (if (and (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (/= i 0))
   (while (= z00 0)
	(setq bendname (getstring T "\nSpecify Point Name [BGV/BFV/ARV/WASHOUT]: "))
  	(if (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (setq z00 1) ) 
    ))
        
   (setq z11 1)
   (setq z22 0)
        
   (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor))  (progn
    (if (or (= i 0) (= i (- n 2)))(progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : "))) 
	(progn
	(if (and (= flagii 0) (= flagiii 0)) 
	(progn 
	(initget "k K a A")
	  (setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	(if (= invertlevel nil)(setq flagii 1) 
	(setq flagiii 1))) 
    (if (= flagiii 1) 
	(progn 
	(while (= z22 0)
	(progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: "))  (if  (= invertlevel nil) (setq invertlevel "k")))
  	(if (/= invertlevel nil) (setq z22 1) )
	)
    )
	)
	)
   (if (= flagii 1) (setq invertlevel nil))
	)))
   (progn
     (if (or (= i 0) (= i (- n 2)))(progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : ")))  
	 (progn
	  (if (and (= flagii 0) (= flagiii 0)) 
	  (progn 
	(initget "k K a A")
	  (setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	(if (= invertlevel nil)(setq flagii 1) 
	(setq flagiii 1))) 
	  (if (= flagiii 1) (progn 
	  (while (= z22 0)
	  (progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: "))  (if  (= invertlevel nil) (setq invertlevel "k")))
  	  (if (/= invertlevel nil) (setq z22 1) )
	  )
      )
    	)
		)
	 (if (= flagii 1) 
	 (setq invertlevel nil))
	 )))
									
    )
   (command "erase" "last" "")
   (setq bendname (list bendname))
   (setq bendnames (append bendnames bendname))
   (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel nil))
   (setq pti (list invertlevel))
   (setq ptsi (append ptsi pti))
   ; (princ bendnames)
   ; (princ ptsg)
   ; (princ ptsi) 
   (setq i (+ i 2))))
   (if (>= i (- n 1)) (progn (setq zz1 0) 
   (while (= zz1 0) 
   (initget "g G")
   (setq confirm (getstring "\nPress ENTER to confirm input or [GoBack]: "))
   (if (or (= confirm "") (= confirm " ") (= confirm "g")  (= confirm "G")) (setq zz1 1) ))  
   (if (or (= confirm "g") (= confirm "G"))  
   (progn 
   (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
   (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))
   (setq i (- i 2)) 
   (setq kkk (- kkk 1))
	(setq bkk (RemoveNth kkk bkk ))  
   (setq kk (- kk 1))
   ))  ))   
   )
   )
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (calc_cdlist)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (if (= allowchainageprofile 1)
 (addchpr)
 (setq cdlist (cons '0 cdlist))
 )
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (calc_ch_gr)
 ;(princ cdlist)
  )
 ; =======================================================================================================
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Automatic mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun autogetpts () 
  ; (cond
 ; (*activeDoc*)
 ; ((setq *activeDoc* (vla-get-activedocument (vlax-get-acad-object)))))
 ; (prompt "\n >> Select Pipe Polyline: ")
 ; (if (setq ss (ssget ":S:E" '((0 . "*POLYLINE")))) 
 ; (progn
  
 ; (setq fg (sslength ss)) ;get the number of objects in the selection set
  ; ;(setq ss (car (entsel "\n Select :"))) ; (gets entity name) Prompts the user to select a single object (entity) by specifying a point. Return Values Type: List or nil. A list whose first element is the entity name of the chosen object and whose second element is the coordinates (in terms of the current UCS) of the point used to pick the object.
; (setq ss1 (ssname ss 0)) ; (gets entity name of first entity in the selection set (the first entity has an index = 0))
; (setq e (entget ss1)); get the entity list
; (setq tt 1)
; (if ( assoc 62 e) ;if color not bylayer
; (progn
; (setq tt 0)
; (setq pipecolor (cdr (assoc 62 e)))))  ; get the color

; (if  (= tt 1)  ; if bylayer
; (progn
; (setq layer (cdr (assoc 8 e))) ;retrieve the layer name
; (setq layerinf (tblsearch "LAYER" layer)) ;get the layer data
; (setq pipecolor (cdr (assoc 62 layerinf) )) ;extract the default layer color
 ; ))
; ;(COMMAND "COLOR" pipecolor)
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ;;;;;DRAW CHAINAGE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (if (or (= allowchainage 1) (= allowchainageprofile 1))
; (progn
; (vl-cmdf "._style" "WP-TEXT" "romans" chtextsize 0.80 0 "n" "n" "n") ;chtextsize
; (setq l1 ss1)
; (COMMAND "LAYER" "SET" "C-ANNO-CHNG" "")
; (if (= chstyle1 1) (setq chst 1))
; (if (= chstyle2 1) (setq chst 2))
; (if (= chstyle3 1) (setq chst 3))
; (esttexto)
; (CHMARKS l1 startchainage (getvar "TEXTSIZE") chainageinterval chst n3 n4 )
; )
; )
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; (vlax-for x (setq ss (vla-get-activeselectionset *activeDoc*)) 
 ; (setq pipecoords
 ; (vlax-safearray->list
 ; (vlax-variant-value
 ; (vla-get-coordinates x)))))
 ; (vla-delete ss)))
 (getha)
 ;;;;;;
  ; (setq fg (sslength ss)) ;get the number of objects in the selection set
  ; ;(setq ss (car (entsel "\n Select :"))) ; (gets entity name) Prompts the user to select a single object (entity) by specifying a point. Return Values Type: List or nil. A list whose first element is the entity name of the chosen object and whose second element is the coordinates (in terms of the current UCS) of the point used to pick the object.
;(setq ss1 (ssname ss 0)) ; (gets entity name of first entity in the selection set (the first entity has an index = 0))
(setq e (entget ha-ename)); get the entity list
(setq tt 1)
(if ( assoc 62 e) ;if color not bylayer
(progn
(setq tt 0)
(setq pipecolor (cdr (assoc 62 e)))))  ; get the color

(if  (= tt 1)  ; if bylayer
(progn
(setq layer (cdr (assoc 8 e))) ;retrieve the layer name
(setq layerinf (tblsearch "LAYER" layer)) ;get the layer data
(setq pipecolor (cdr (assoc 62 layerinf) )) ;extract the default layer color
 ))
 ;;;;;;
 (setq pipecoords
 (vlax-safearray->list
 (vlax-variant-value
 (vla-get-coordinates ha-object))))
 (setq n 0)                                               ;set up counter
 ;while not at the end of the list do...
 (while (not (equal (setq item (nth n pipecoords)) nil))
 ;(setq n (1+ n) ) 
 (setq n (+ n 1))    ;add 1 t counter                             
   );end WHILE
   ;(princ n)
) 
; =======================================================================================================
;ptgrinv function that gets the grade and invert level at each point of pipecoords
 (defun ptgrinv ()
 (setq i 0)
 (setq ptsg nil)
 (setq ptsi nil)
 (setq bendnames nil)
 (setq flagii 0)
 (setq flagiii 0)
 (setq z11 0)
 (setq z22 0)
 (COMMAND "LAYER" "SET" "C-PRFL-TEXT" "")
 (setq kk 0)
 (setq bkk nil)
 (setq kkk 0)
 (if (= auto_anno_b 0)
 (while (<= i (- n 1))
   (if (= i 2) (progn (setq flagii 0) (setq flagiii 0)))
   (setq xp1 (nth i pipecoords))
   (setq yp1 (nth (+ i 1) pipecoords))
   (setq p1 (list xp1 yp1))
   (command "circle" p1 1 )
   (setq zp1 (mapcar '+ p1 '( -20 20 0)))
   (setq zp2 (mapcar '+ p1 '( 20 -20 0)))
   (COMMAND "ZOOM" "W" zp1 zp2)
   (if (= i 0)(setq bendname (getstring T "\nSpecify Point Name [BGV/BFV/ARV/WASHOUT]: ")) (setq bendname (getstring T "\nSpecify Point Name [Undo/BGV/BFV/ARV/WASHOUT]: ")))
   (if (and (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (/= i 0))
   (progn 
   (command "erase" "last" "") 
   ;(princ (- (/ i 2) 1)) 
   (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
   (setq ptsg (RemoveNth (- (/ i 2) 1 ) ptsg )) 
   (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))  
   ;(princ bendnames) (princ ptsg)   (princ ptsi)
   (setq i (- i 2)) )     
   (progn 
   (setq z00 0)
   (if (and (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (/= i 0))
   (while (= z00 0)
	(setq bendname (getstring T "\nSpecify Point Name [BGV/BFV/ARV/WASHOUT]: "))
  	(if (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (setq z00 1) ) 
    ))
        
   (setq z11 0)
   (setq z22 0)
   (while (= z11 0)
   (initget 1)
	(setq gradelevel (getreal "\nSpecify Grade Level : "))
  	(if (/= gradelevel nil) (setq z11 1) )
    )
     
   (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor))  (progn
    (if (or (= i 0) (= i (- n 2)))(progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : "))) 
	(progn
	(if (and (= flagii 0) (= flagiii 0)) 
	(progn 
	(initget "k K a A")
	  (setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	(if (= invertlevel nil)(setq flagii 1) 
	(setq flagiii 1))) 
	(if (= flagiii 1) (progn (while (= z22 0)
	;(if (/= flagii "k") (progn (initget 1) (setq invertlevel (getreal "\nSpecify Invert Level : ")))
	(progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: "))  (if  (= invertlevel nil) (setq invertlevel "k"))) ;(if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel nil)))
	;)
  	(if (/= invertlevel nil) (setq z22 1) )
	)
	)
	)
	)
	(if (= flagii 1) (setq invertlevel nil))
	)))
	(progn
    (if (or (= i 0) (= i (- n 2)))(progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : "))) 
	(progn
	(if (and (= flagii 0) (= flagiii 0)) 
	(progn 
	  (initget "k K a A")
	  (setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	  (if (= invertlevel nil)(setq flagii 1) 
	  (setq flagiii 1))) 
	(if (= flagiii 1) (progn (while (= z22 0)
	;(if (/= flagii "k") (progn (initget 1) (setq invertlevel (getreal "\nSpecify Invert Level : ")))
	(progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: ")) (if  (= invertlevel nil) (setq invertlevel "k")) ) ;(if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel nil)))
	;)
  	(if (/= invertlevel nil) (setq z22 1) )
	)
	)
	)
	)
	(if (= flagii 1) (setq invertlevel nil))
	)))
	; (progn
	; (setq z22 0)
	; (while (= z22 0)
	; (setq invertlevel (getreal "\nSpecify Invert Level : "))
  	; (if (/= invertlevel nil) (setq z22 1) )
	; )
	; )
	)
   (command "erase" "last" "")
   (setq bendname (list bendname))
   (setq bendnames (append bendnames bendname))
   (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel nil))
   (setq ptg (list gradelevel))
   (setq ptsg (append ptsg ptg))
   (setq pti (list invertlevel))
   (setq ptsi (append ptsi pti))
   ;(princ bendnames)
   ;(princ ptsg)
   ;(princ ptsi) 
   (setq i (+ i 2))))
   (if (>= i (- n 1)) (progn (setq zz1 0) 
   (while (= zz1 0) 
   (initget "g G")
   (setq confirm (getstring "\nPress ENTER to confirm input or [GoBack]: "))
   (if (or (= confirm "") (= confirm " ") (= confirm "g")  (= confirm "G")) (setq zz1 1) ))  
   (if (or (= confirm "g") (= confirm "G"))
   (progn (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
   (setq ptsg (RemoveNth (- (/ i 2) 1 ) ptsg )) 
   (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))(setq i (- i 2))  ))  ))   
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (while (<= i (- n 1))
   (if (= i 2) (progn (setq flagii 0) (setq flagiii 0)))
   (setq xp1 (nth i pipecoords))
   (setq yp1 (nth (+ i 1) pipecoords))
   (setq p1 (list xp1 yp1))
   (command "circle" p1 1 )
   (setq zp1 (mapcar '+ p1 '( -20 20 0)))
   (setq zp2 (mapcar '+ p1 '( 20 -20 0)))
   (COMMAND "ZOOM" "W" zp1 zp2)
   (if (= i 0)(progn (setq bkk nil) (setq kk 0) (setq kkk 0)(setq bendname (getstring T "\nSpecify Point Name[BGV/BFV/ARV/WASHOUT]: ")))) 
   (if (and (> i 0) (< i (- n 2))) 
   (if (= (nth (/ i 2) bend_angles) nil)
   (progn
   (setq bkk (append bkk (list kk)))
   (setq kk 0)
   (setq kkk (+ kkk 1))
   (setq bendname (getstring T "\nSpecify Point Name[Undo/BGV/BFV/ARV/WASHOUT]: ")) 
   ) 
   (progn 
   (setq bendname (nth (/ i 2) bend_angles))
   )
   ) )
   (if (= i (- n 2)) 
   (progn 
  (setq bkk (append bkk (list kk)))
  (setq kkk (+ kkk 1))
   (setq bendname (getstring T "\nSpecify Point Name[Undo/BGV/BFV/ARV/WASHOUT]: "))
   )
   )
   (if (and (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (/= i 0))
   (progn 
    (command "erase" "last" "") 
    (repeat (nth (- kkk 1) bkk)
    (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
	(setq ptsg (RemoveNth (- (/ i 2) 1 ) ptsg ))
    (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))  
    (setq i (- i 2)) )
	(setq kkk (- kkk 1))
	(setq bkk (RemoveNth kkk bkk ))  
   (setq kkk (- kkk 1))
    )   
   (progn 
   (setq kk (+ kk 1))
   (setq z00 0)
   (if (and (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (/= i 0))
   (while (= z00 0)
	(setq bendname (getstring T "\nSpecify Point Name[BGV/BFV/ARV/WASHOUT]: "))
  	(if (OR(= bendname "u") (= bendname "u ") (= bendname "U") (= bendname "U ")) (setq z00 1) ) 
    ))
   (setq z11 0)
   (setq z22 0)
   (while (= z11 0)
   (initget 1)
	(setq gradelevel (getreal "\nSpecify Grade Level : "))
  	(if (/= gradelevel nil) (setq z11 1) )
    )    
   (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor))  (progn
    (if (or (= i 0) (= i (- n 2)))(progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : "))) 
	(progn
	(if (and (= flagii 0) (= flagiii 0)) 
	(progn 
	(initget "k K a A")
	  (setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	(if (= invertlevel nil)(setq flagii 1) 
	(setq flagiii 1))) 
    (if (= flagiii 1) 
	(progn 
	(while (= z22 0)
	;(if (/= flagii "k") (progn (initget 1) (setq invertlevel (getreal "\nSpecify Invert Level : ")))
	(progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: "))  (if  (= invertlevel nil) (setq invertlevel "k"))) ;(if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel nil)))
	;)
	  	(if (/= invertlevel nil) (setq z22 1) )
	)
    )
	)
	)
   (if (or (= flagii 1) ) (setq invertlevel nil));;;;;;;;;;;
	)))
   (progn
     (if (or (= i 0) (= i (- n 2)))(progn (initget 1)(setq invertlevel (getreal "\nSpecify Invert Level : ")))  
	 (progn
	  (if (and (= flagii 0) (= flagiii 0)) 
	  (progn 
	  (initget "k K a A")
	  (setq invertlevel (getreal "\nSpecify Invert Level or [sKip/skipAll] <A>: ")) (if (or (= invertlevel "a") (= invertlevel "A")) (setq invertlevel nil)) (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel "k")) (if (= invertlevel "k")(setq flagii "k"))
	  (if (= invertlevel nil)(setq flagii 1) 
	  (setq flagiii 1))) 
	  (if (= flagiii 1) (progn 
	  (while (= z22 0)
	 ;(if (/= flagii "k") (progn (initget 1) (setq invertlevel (getreal "\nSpecify Invert Level : ")))
	(progn (initget "k K") (setq invertlevel (getreal "\nSpecify Invert Level or [sKip] <K>: ")) (if  (= invertlevel nil) (setq invertlevel "k")) ) 
	;)
  	  (if (/= invertlevel nil) (setq z22 1) )
	  )
      )
    	)
		)
	 (if (= flagii 1) 
	 (setq invertlevel nil))
	 )))
									
    )
   (command "erase" "last" "")
   (setq bendname (list bendname))
   (setq bendnames (append bendnames bendname))
   (if (or (= invertlevel "k") (= invertlevel "K")) (setq invertlevel nil))
   (setq pti (list invertlevel))
   (setq ptg (list gradelevel))
   (setq ptsg (append ptsg ptg))
   (setq ptsi (append ptsi pti))
   ; (princ bendnames)
   ; (princ ptsg)
   ; (princ ptsi) 
   (setq i (+ i 2))))
   (if (>= i (- n 1)) (progn (setq zz1 0) 
   (while (= zz1 0) 
   (initget "g G")
   (setq confirm (getstring "\nPress ENTER to confirm input or [GoBack]: "))
   (if (or (= confirm "") (= confirm " ") (= confirm "g")  (= confirm "G")) (setq zz1 1) ))  
   (if (or (= confirm "g") (= confirm "G"))  
   (progn 
   (setq bendnames (RemoveNth (- (/ i 2) 1) bendnames)) 
   (setq ptsg (RemoveNth (- (/ i 2) 1 ) ptsg ))
   (setq ptsi (RemoveNth (- (/ i 2) 1) ptsi ))
   (setq i (- i 2)) 
   (setq kkk (- kkk 1))
	(setq bkk (RemoveNth kkk bkk ))  
   (setq kk (- kk 1))
   ))  ))   
   )
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (calc_cdlist)
  (setq gcdlist (cons '0 cdlist));;;;;;;;;;;;;;;;;;;;;;;;;;check error
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (if (= allowchainageprofile 1)
 (addchpr)
 (setq cdlist (cons '0 cdlist))
 )
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (calc_ch_gr)
 (setq ptsg ptsgati)
  )
; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Manual mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;mangetpts function gets manually the the coord of  start, bend, sections and end of the water line 
;and puts in a list called pipecoords
(defun mangetpts ()  
   (setq pipecoords nil)
   (setq manpipepoint '(E E))
   (setq bendnames nil)
   (setq ptsg nil)
   (setq ptsi nil)
    (while (/= (car manpipepoint) nil)
	(setq manpipepoint (getpoint "\nPick next point on the pipe (press Enter to end): ") ) 
    (if  (/= (car manpipepoint) nil) 
	(progn 
	(setq manpipepoint (list (car manpipepoint) (cadr manpipepoint)))
	(setq pipecoords (append pipecoords manpipepoint))
	(setq bendname (getstring T "\nSpecify Point Name: ")) 
	
	(setq z11 0)
    (while (= z11 0)
	(setq gradelevel (getreal "\nSpecify Grade Level: "))
  	(if (/= gradelevel nil) (setq z11 1) )
     )
	(setq invertlevel (getreal "\nSpecify Invert Level: ")) 
	(setq bendname (list bendname))
    (setq bendnames (append bendnames bendname))
	(setq ptg (list gradelevel))
    (setq ptsg (append ptsg ptg))
	(setq pti (list invertlevel))
    (setq ptsi (append ptsi pti))
	;(princ pipecoords)
	;(terpri)
	;(princ bendnames)
	;(terpri)
	;(princ ptsg)
	;(terpri)
	;(princ ptsi)
	;(terpri)
		))
    )
	(setq n 0)                                               ;set up counter
 ;while not at the end of the list do...
 (while (not (equal (setq item (nth n pipecoords)) nil))
  (setq n (+ n 1))    ;add 1 t counter                             
   );end WHILE
   ;(princ n) 
	
	;(terpri)
 ;(princ pipecoords) 
 ;(terpri)
 ;(princ pipecoords) 
 ;(terpri)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;(setq ptsg (mapcar  '(lambda (x)            (atof x)           )           ptsg ))

 (draw_polyline pipecoords)

(if (= allowchainageprofile 1)
(progn 
(esttexto)
(setq startchainage 0)
(CHMARKS ll1 startchainage (getvar "TEXTSIZE") chainageinterval chst n3 n4 )
)
)
(command "erase" "last" "") 
	 
  (calc_cdlist)
  (setq gcdlist (cons '0 cdlist));;;;;;;;;;;;;;;;;;;;;;;;;;check error
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (if (= allowchainageprofile 1)
 (addchpr)
 (setq cdlist (cons '0 cdlist))
 )
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (calc_ch_gr)
 (setq ptsg ptsgati)
 )
; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Import mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun import-data ()
  (setq filename1
      (getfiled "Choose file"
         (strcat (getvar 'dwgprefix) ); (vl-filename-base (getvar 'dwgname)) )
          "xls"
         8
      )
  )
  (setq filename1 (findfile filename1))
;(terpri)
; ;(princ filename1)
; ;(OpenExcel filename1 nil nil)
 ; (setq i 1)
 ; (setq cell "A7")
; (while ( /= (GetCell cell) "")
 ; (setq i (+ i 1))
 ; (setq  cell (strcat (Number2Alpha i)"7"))
; ; (princ i)
 ; (terpri)
 ; ;(princ cell)
 ; )
 ; ;(CloseExcel filename1)
 ; (terpri)
 ; ;(princ i)
 ; (terpri)
 ; (setq  cell (strcat (Number2Alpha (+ i 0))"7"))
; ;(princ cell)
 ; (terpri)
; (princ cell)
(setq datalist (GetExcel filename1 nil nil))
;(princ datalist)

(setq TITLExls (nth 0 datalist))
;(setq TITLExls (append TITLExls (LIST "TEST")))
;(setq TITLExls (cdr TITLExls))
(setq TITLExls (car TITLExls))
(setq TITLEinitial (substr TITLExls 1 8)) 

(if (or  (= TITLEinitial "EXISTING") (= TITLEinitial "Existing") 
 (= TITLEinitial "TEMPORAR") (= TITLEinitial "Temporar") 
 (= TITLEinitial "RELOCATE") (= TITLEinitial "Relocate") ) 
 (progn
(if (OR (= TITLEinitial "EXISTING") (= TITLEinitial "Existing")) (setq pipecolor existpipecolor))
(if (OR (= TITLEinitial "TEMPORAR") (= TITLEinitial "Temporar")) (setq pipecolor temppipecolor))
(if (OR (= TITLEinitial "RELOCATE") (= TITLEinitial "Relocate")) (setq pipecolor relocpipecolor))
) (setq pipecolor relocpipecolor))
(setq dia (nth 1 datalist))
(setq dia (cdr dia))
(setq dia (car dia))
(setq dia (atoi dia))
(setq rdia (rtos dia 2 0))
;(terpri)
;(princ dia)

(setq bendnames (nth 2 datalist))
(setq bendnames (cdr bendnames))
;(princ (length bendnames))
;(terpri)
;(princ bendnames)
(setq ptsg (nth 3 datalist))
(setq ptsg (cdr ptsg))
(setq ptsg (vl-remove "" ptsg))
(setq ptsg (mapcar  '(lambda (x)            (atof x)           )           ptsg ))
;(terpri)
;(princ ptsg)
(setq ptsi (nth 4 datalist))
(setq ptsi (cdr ptsi))
(setq ptsi (vl-remove "" ptsi))
(setq ptsi (mapcar  '(lambda (x)            (atof x)           )           ptsi ))
;(terpri)
;(princ ptsi)
(setq slpl (nth 5 datalist))
(setq slpl (cdr slpl))
(setq slpl (vl-remove "" slpl))
(setq slpl (mapcar  '(lambda (x)            (atof x)           )           slpl ))
(setq slpl (mapcar  '(lambda (x)            (/ x 100)           )           slpl ))
;(terpri)
;(princ slpl)
(setq pdt (nth 6 datalist))
(setq pdt (cdr pdt))
;(setq pdt (apply 'append (mapcar '(lambda (x) (if x (list x))) pdt)))
(setq pdt (vl-remove "" pdt))
(setq pdt (mapcar  '(lambda (x)            (atof x)           )           pdt ))
;(terpri)
;(princ pdt)
(setq i 0)
(setq CD 0)
(setq cdlist nil)
(while (<= i (- (length pdt) 1))
(setq CD (+ CD (nth i pdt)))
(setq CD1 CD)
(setq CD1 (list CD1))
(setq cdlist (append cdlist CD1))
(setq i (+ i 1))
)
;(terpri)
;(princ CD)
;(terpri)
;(princ cdlist)
;(terpri)
(setq n (* (length ptsi) 2))
;(princ n)
)
; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Export Data functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun export-data ( / emsg )
(setq bendnamesxls  (list (strcat "Point Name:")))
(setq bendnamesxls (append bendnamesxls bendnames))
(setq bendnamesxls(subst "" nil bendnamesxls))
(if (and (/= MODE "C") (/= MODE "S")) (progn 
(setq ptsgxls (list (strcat "Grade level:")))
(setq ptsgxls (append ptsgxls ptsg))
) (progn (setq ptsgxls (list (strcat "Grade level:")))  (setq ptsgxls (append ptsgxls ptsgati))))
(setq ptsixls (list (strcat "Invert level:") (car ptsi)))
;(princ ptsixls)
;(setq ptsixls (append ptsixls (car ptsi)))
(setq i 2)
;(princ i)
(while (< i (+(length ptsi) 1))
(setq ilet (Number2Alpha i))
(setq ilett (list (strcat "=" ilet "5+(" ilet "6/100)*" ilet "7")))
(setq ptsixls (append ptsixls  ilett))
(setq i (+ i 1))
)



(setq slplxls (list (strcat "Slope(%):")))
(setq slpl (mapcar  '(lambda (x)            (* x 100)           )           slpl ))
(setq slplxls (append slplxls slpl))
(setq pdtxls (list (strcat "Partial Distance:")))
(setq pdtxls (append pdtxls pdt))
(setq diaxls (list (strcat "Pipe Diameter:")))
(setq diaxls (append diaxls (list dia)))


	 (setq fname
       (getfiled "Save as"
          (strcat (getvar 'dwgprefix) (vl-filename-base (getvar 'dwgname)) )
           "xls"
          1
       )
   )
  
 ; (setq    f (open  fname "a"))
 ; (close f)

 ;;(defun Put-DATA ()
   ;;(OpenExcel "C:\\WP\\WP.xls" "Sheet1" nil);<-- Edit Filename.xls
   ;(OpenExcel fname "Sheet1" nil)
   (if (/= fname nil) (progn
   (setq emsg (strcat "\nSaving... " fname ))  
   (princ emsg)
   (OpenExcel nil nil nil)
   (PutCell "A1" TITLE)
   (PutCell "A2" diaxls)
   (repeat (length bendnamesxls) (PutCell "A3" bendnamesxls));Repeat as required
   (repeat (length ptsgxls)(PutCell "A4" ptsgxls));Repeat as required
   (repeat (length ptsixls)(PutCell "A5" ptsixls));Repeat as required
   (repeat (length slplxls)(PutCell "A6" slplxls));Repeat as required
   (repeat (length pdtxls)(PutCell "A7" pdtxls));Repeat as required
    
	; (setq fname
      ; (getfiled "Save as"
         ; (strcat (getvar 'dwgprefix) (vl-filename-base (getvar 'dwgname)) )
          ; "xls"
         ; 1
      ; )
  ; )
;(terpri)
;(princ fname)
;(terpri)
   
   ;(CloseExcel "C:\\WP\\WP.xls");<-- Edit Filename.xls
   (CloseExcel fname)
  )
  )
   
   (princ)
; );defun
  
 ;(Put-DATA)
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Second run mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique with Fuzz  -  Lee Mac
;; Returns a list with all elements considered duplicate to
;; a given tolerance removed.

(defun LM:UniqueFuzz ( l f / x r )
    (while l
        (setq x (car l)
              l (vl-remove-if (function (lambda ( y ) (equal x y f))) (cdr l))
              r (cons x r)
        )
    )
    (reverse r)
)
; =======================================================================================================
(defun getinter () ;pilpl_clin_pts 
(setq clinset (ssget "_X"  '((0 . "LINE")(8 . "C-PRFL-CLIN"))))
(setq ve 1) ;Vertical Exaggeration
  (setq listxy nil)
  ;(setq hazvalue (caddr (vlax-curve-getStartPoint hai-object)))
  (setq curvas clinset)
  (if (/= curvas nil) (setq ncurvas (sslength curvas))  (setq ncurvas 0))
  (setq listxy nil)
  (setq counter 0)
  (while (< counter ncurvas)
    (progn
      (setq cnivel-ename (ssname curvas counter)) ;(ssname ss index) ssname: Returns the object (entity) name of the indexed element of a selection set   (setq ent1 (ssname ss 0)) <Entity name: 1d62d68>
      (setq cnivel-object (vlax-ename->vla-object cnivel-ename)); vlax-ename->vla-object Transforms an entity to a VLA-object  (setq e (car (entsel)))  <Entity name: 27e0540> (vlax-ename->vla-object e)  #<VLA-OBJECT IAcadLWPolyline 03f713a0>
      ; ;get the z of an object
      ; (setq cnivelzvalue
	     ; (caddr (vlax-curve-getStartPoint cnivel-object)) ; vlax-curve-getStartPoint Returns the start point (in WCS) of the curve
      ; )

      ; (setq hai-ENTITY
	     ; (subst (cons 38 cnivelzvalue)  ;subst Searches a list for an old item and returns a copy of the list with a new item substituted in place of every occurrence of the old item (subst newitem olditem lst)
                                        ; ;cons Adds an element to the beginning of a list, or constructs a dotted list  (cons new-first-element list-or-atom)
		    ; (assoc 38 (entget (car hai))) ;assoc Searches an association list for an element and returns that association list entry (assoc element alist)
		    ; (entget (car hai))            ;Retrieves an object's (entity's) definition data (entget ename [applist])
	     ; )
      ; )
      ; (entmod hai-ENTITY) ;entmod Modifies the definition data of an object (entity) (entmod elist)

      (setq intersectpt
	     (vlax-variant-value    ;Returns the value of a variant (vlax-variant-value var)
	       (vlax-invoke-method   ;vlax-invoke-method  Calls the specified ActiveX method (vlax-invoke-method obj method arg [arg...])
		 hai-object
		 "IntersectWith"
		 cnivel-object
		 acExtendNone
	       )
	     )
      )

      (setq test nil)
      (setq
	test (vl-catch-all-apply
	       'vlax-safearray->list
	       (list intersectpt)
	     )
      )
      (setq errorrwpc (vl-catch-all-error-p test))

      (if (= errorrwpc nil) ;(/= error t)
	(progn
	  (setq intersectpt (vlax-safearray->list intersectpt))
	  (setq interlength (length intersectpt))

	  (if (> interlength 3)
	    (progn
	      (setq dividelength (/ interlength 3))
	      (setq count 0)
	      (while (< count interlength)
		(progn
		  (setq	newpt (list (nth count intersectpt)
				    (nth (+ count 1) intersectpt)
				    (nth (+ count 2) intersectpt)
			      )
		  )

		  (setq x (car intersectpt))
		  (setq y (cadr intersectpt))
		  (setq xy (list x (* y ve)))
		  (setq
		    listxy (append listxy (list xy))
		  )

		  (setq count (+ count 3))
		)
	      )
	    )
	    (progn
	       (setq x (car intersectpt))
		  (setq y (cadr intersectpt))
	      (setq xy (list x (* y ve)))
	      (setq
		listxy	(append listxy (list xy))
	      )
	    )
	  )

	  ; (setq	hai-ENTITY
		 ; (subst	(cons 38 hazvalue)
			; (assoc 38 (entget (car hai)))
			; (entget (car hai))
		 ; )
	  ; )
	  ; (entmod hai-ENTITY)
	)
      )
      (setq counter (1+ counter))
    )
  )

  (setq	listxy
	 (vl-sort listxy
		  (function (lambda (e1 e2)
			      (< (car e1) (car e2))
			    )
		  )
	 )
  )

 ; (princ listxy)
(setq srpinvertcoords
 (vlax-safearray->list
 (vlax-variant-value
 (vla-get-coordinates hai-object))))
  (setq i 0)
 (setq pairll nil)
 (while (<= i (- (length srpinvertcoords) 2))
 (setq pairl (list (nth i srpinvertcoords) (nth (+ i 1) srpinvertcoords)))
 (setq pairll (append pairll (list pairl)))
 (setq i (+ i 2))
 )
 (setq srpinvertcoords pairll) 
 (setq srpinvertcoords (append srpinvertcoords  listxy))
 (setq srpinvertcoords (vl-sort srpinvertcoords (function (lambda (e1 e2)  (< (car e1) (car e2)) ) ))) ;(vl-sort '((1 3) (2 2) (3 1))              (function (lambda (e1 e2)                          (< (cadr e1) (cadr e2)) ) ) )
 (setq srpinvertcoords (LM:Unique srpinvertcoords))
 (setq srpinvertcoords (LM:UniqueFuzz srpinvertcoords 0.009))
 (setq dlsrpinvertcoords srpinvertcoords)
(setq min_srpinvertcoords(apply 'min (mapcar 'cadr srpinvertcoords)))
  
  (setq ci 0)
  (setq xy nil)
  (setq nsrpinvertcoords (length srpinvertcoords))
  (while (<= ci (- nsrpinvertcoords 1))
  (setq x (car (nth ci srpinvertcoords)))
  (setq y (cadr (nth ci srpinvertcoords)))
  (setq xx (list x))
  (setq yy (list y))
  (setq xy (append xy xx))
  (setq xy (append xy yy))
  (setq ci (+ ci 1))
  )
  (setq srpinvertcoords xy)
  ;(terpri)
  ;(princ srpinvertcoords)
 ;(terpri)
  ; (setq	startdist (vlax-curve-getdistatPoint
		    ; hai-ename
		    ; (vlax-curve-getstartpoint hai-ename)
		  ; )
	; enddist	  (vlax-curve-getdistatPoint
		    ; hai-ename
		    ; (vlax-curve-getendpoint hai-ename)
		  ; )
  ; )

  ; (setq	pt1 (car (car listxy))
	; pt2 (car (last listxy))
  ; )

  ; (if (/= startdist pt1)
    ; (progn
      ; (setq x startdist)
      ; (setq y (+ (* (/ (- (cadr (car listxy)) (cadr (cadr listxy)))
		       ; (- (car (cadr listxy)) (car (car listxy)))
		    ; )
		    ; (- (car (car listxy)) startdist)
		 ; )
		 ; (cadr (car listxy))
	      ; )
      ; )
      ; (setq xy (list x y))
      ; (setq
	; listxy	(append listxy (list xy))
      ; )
      ; (setq listxy
	     ; (vl-sort listxy
		      ; (function	(lambda	(e1 e2)
				  ; (< (car e1) (car e2))
				; )
		      ; )
	     ; )
      ; )

    ; )
  ; )

  ; (if (/= enddist pt1)
    ; (progn
      ; (setq pos (1- (length listxy)))
      ; (setq x enddist)
      ; (setq y
	     ; (+
	       ; (*
		 ; (/ (- (cadr (nth pos listxy))
		       ; (cadr (nth (1- pos) listxy))
		    ; )
		    ; (- (car (nth pos listxy)) (car (nth (1- pos) listxy)))
		 ; )
		 ; (- enddist (car (nth pos listxy)))
	       ; )
	       ; (cadr (nth pos listxy))
	     ; )
      ; )
      ; (setq xy (list x y))
      ; (setq
	; listxy	(append listxy (list xy))
      ; )
      ; (setq listxy
	     ; (vl-sort listxy
		      ; (function	(lambda	(e1 e2)
				  ; (< (car e1) (car e2))
				; )
		      ; )
	     ; )
      ; )

    ; )
  ; )
  )
; =======================================================================================================
;get the profiel pipe Invert polyline
(defun getpilpl ()
  ;; this entity must be a lwpolyline
  (activexsupport)
  (setq
    hai (entsel "\nSelect Profile Pipe Invert Polyline: ")
  )
  (while (= hai nil)
    (progn
      (princ "\nNothing selected...")
      (setq hai
	     (entsel "\nSelect the Profile Pipe Invert Polyline: ")
      )
    )
  )
  (setq hai-type (cdr (assoc 0 (entget (car hai)))))
  (if (not (equal hai-type "LWPOLYLINE"))
    (progn
      (setq hai nil)
      (princ "\n***Profile Pipe Invert Polyline: must be a LWPolyline***")
    )
  )
  (while (= hai nil)
    (progn
      (princ "\nNothing selected...")
      (setq hai
	     (entsel "\nSelect the Profile Pipe Invert Polyline: ")
      )
      (setq hai-type (cdr (assoc 0 (entget (car hai)))))
      (if (not (equal hai-type "LWPOLYLINE"))
	(progn
	  (setq hai nil)
	  (princ "\n***Select the Profile Pipe Invert Polyline:***")
	)
      )
    )
  )
  (setq hai-ename (entget (car hai)))
  (setq hai-ename (cdr (assoc -1 hai-ename)))
  (setq hai-object (vlax-ename->vla-object hai-ename))
)
; =======================================================================================================
;get the profile grade polyline 
(defun getpgrpl ()
  ;; this entity must be a lwpolyline
  (activexsupport)
  (setq
    hag (entsel "\nSelect Profile Grade Polyline: ")
  )
  (while (= hag nil)
    (progn
      (princ "\nNothing selected...")
      (setq hag
	     (entsel "\nSelect the Profile Grade Polyline: ")
      )
    )
  )
  (setq hag-type (cdr (assoc 0 (entget (car hag)))))
  (if (not (equal hag-type "LWPOLYLINE"))
    (progn
      (setq hag nil)
      (princ "\n***Profile Grade Polyline: must be a LWPolyline***")
    )
  )
  (while (= hag nil)
    (progn
      (princ "\nNothing selected...")
      (setq hag
	     (entsel "\nSelect the Profile Grade Polyline: ")
      )
      (setq hag-type (cdr (assoc 0 (entget (car hag)))))
      (if (not (equal hag-type "LWPOLYLINE"))
	(progn
	  (setq hag nil)
	  (princ "\n***Select the Profile Grade Polyline:***")
	)
      )
    )
  )
  (setq hag-ename (entget (car hag)))
  (setq hag-ename (cdr (assoc -1 hag-ename)))
  (setq hag-object (vlax-ename->vla-object hag-ename))
)
; =======================================================================================================
;usage:(combss ss1 ss2) puts the elements in selection set 2 into selection set 1

(defun combss (ss ss1 / cnt)
(if (/= ss1 nil) (progn
  (setq cnt 0)
  (repeat (sslength ss1)
    (ssadd (ssname ss1 cnt) ss)  ;take each item in ss1 and add it to ss
    (setq cnt (1+ cnt))
  )
  )
  )
  ss
)
;========================================================================================================
;get the profile crossings 
(defun getpcr ()
 (setq sss nil)
 (activexsupport)
 (setq pt1 (list  (- (car srpgradecoords) 5) (+ (fix max_srpg) (* 3.5 VF))))
 (setq pt2a (list (+ (nth (- (length srpgradecoords) 2) srpgradecoords) 5) (- (fix min_srpinvertcoords) (* 1 VF))))
 (setq pt2b (list  (- (car srpgradecoords) 5) (- (fix min_srpinvertcoords) (* 1 VF))))
 (setq pt2 (list (+ (nth (- (length srpgradecoords) 2) srpgradecoords) 5) (- (fix min_srpinvertcoords) (* 3 VF))))
 (COMMAND "ZOOM" "W" pt1 pt2)
 (setq sss1 (ssget "_W" pt1 pt2a '((-4 . "<NOT")(-4 . "<OR") (8 . "C-PRFL-GRID")  (8 . "C-PRFL-CLIN") (8 . "C-PRFL-GRDE") (8 . "C-PRFL-PIPE") (8 . "C-PRFL-BORD")  (-4 . "OR>")  (-4 . "NOT>")) ))
 (setq sss2 (ssget "_W" pt2b pt2 '((-4 . "<NOT")(-4 . "<OR") (8 . "C-PRFL-GRID")  (8 . "C-PRFL-CLIN") (8 . "C-PRFL-GRDE") (8 . "C-PRFL-PIPE") (8 . "C-PRFL-BORD") (8 . "C-PRFL-TEXT")(-4 . "OR>")  (-4 . "NOT>")) ));(8 . srp_layname)
 (setq sss (combss sss1 sss2))
 (command "zoom" "_P")
 ;(princ pt1)
 ;(princ pt2)
 ;(princ sss)
 (if (/= sss nil) (progn
 (command "._copybase"  (car dlsrpinvertcoords)  sss "")
 (setq pt3 (list (car BASE) (* (car ptsi ) VF)))
 (command "._pasteclip" pt3 )
 ;(command "_.copy" sss "" (car dlsrpinvertcoords) pt3)
 )
 )
 (princ)
)
 ; =======================================================================================================
(defun secondrun () 
(setq datumpt (getpoint "\nPick a point on the profile datum level: "))
(initget 1)
(setq datum (getreal "\nSpecify Datum Level: "))

 (getpgrpl)
 (setq srpgradecoords nil)
 (setq srpgradecoords
 (vlax-safearray->list
 (vlax-variant-value
 (vla-get-coordinates hag-object))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq i 0)
 (setq pairll nil)
 (while (<= i (- (length srpgradecoords) 2))
 (setq pairl (list (nth i srpgradecoords) (nth (+ i 1) srpgradecoords)))
 (setq pairll (append pairll (list pairl)))
 (setq i (+ i 2))
 )
 (setq srpgradecoords pairll) 
 (setq srpgradecoords (vl-sort srpgradecoords (function (lambda (e1 e2)  (< (car e1) (car e2)) ) ))) 
 (setq srpgradecoords (LM:Unique srpgradecoords))
 (setq srpgradecoords (LM:UniqueFuzz srpgradecoords 0.009))
 ;(setq dlsrpinvertcoords srpgradecoords)
 ;(setq min_srpinvertcoords(apply 'min (mapcar 'cadr srpgradecoords)))
  
  (setq ci 0)
  (setq xy nil)
  (setq nsrpgradecoords (length srpgradecoords))
  (while (<= ci (- nsrpgradecoords 1))
  (setq x (car (nth ci srpgradecoords)))
  (setq y (cadr (nth ci srpgradecoords)))
  (setq xx (list x))
  (setq yy (list y))
  (setq xy (append xy xx))
  (setq xy (append xy yy))
  (setq ci (+ ci 1))
  )
  (setq srpgradecoords xy)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq ptsg nil)
 (setq i 1)
 (while (<= i (- (length srpgradecoords) 1))
 (setq gradelevel (+ datum (/ (- (nth i srpgradecoords) (cadr datumpt)) VF))) 
 (setq gradelevel (list gradelevel))
 (setq ptsg (append ptsg gradelevel))
 (setq i (+ i 2))
 )

;(terpri)
 ;(princ ptsg)
 
 (setq i 1)
 (setq max_srpg (car srpgradecoords))
 (while (<= i (- (length srpgradecoords) 1))
 (if (>= (nth i srpgradecoords) max_srpg) (setq max_srpg (nth i srpgradecoords)))
 (setq i (+ i 2))
 )
 
 
 
  (setq i 0) 
  (setq pdtg nil)
  (while (<= i (- (length srpgradecoords) 4))
  (setq xp1 (nth i srpgradecoords))
  (setq yp1 (nth (+ i 1) srpgradecoords))
  (setq xp2 (nth (+ i 2) srpgradecoords))
  (setq yp2 (nth (+ i 3) srpgradecoords))
  (setq p1 (list xp1 yp1))
  (setq p2 (list xp2 yp1))
  (setq d (/ (distance  p1  p2 ) HF))
  (setq d1 (list d))
  (setq pdtg (append pdtg d1))
  (setq i (+ i 2))
  )
  
 (getpilpl)
 (setq e (entget hai-ename)); get the entity list
 (setq tt 1)
 (setq srp_layname  (strcat (cdr (assoc 8 e))) )
 (if ( assoc 62 e) ;if color not bylayer
 (progn
 (setq tt 0)
 (setq pipecolor (cdr (assoc 62 e)))))  ; get the color

 (if  (= tt 1)  ; if bylayer
 (progn
 (setq layer (cdr (assoc 8 e))) ;retrieve the layer name
 (setq layerinf (tblsearch "LAYER" layer)) ;get the layer data
 (setq pipecolor (cdr (assoc 62 layerinf) )) ;extract the default layer color
 ))

 
 ;(terpri)
 (getinter)
 ;(terpri)
 ;(princ srpinvertcoords)
 (setq ptsi nil)
 (setq i 1)
 (while (<= i (- (length srpinvertcoords) 1))
 (setq invertlevel (+ datum (/ (- (nth i srpinvertcoords)(cadr datumpt)) VF)))
 (setq invertlevel (list invertlevel))
 (setq ptsi (append ptsi invertlevel))
 (setq i (+ i 2))
 )
 ;(terpri)
 ;(princ ptsi)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq i 0) 
  (setq pdt nil)
  (setq CD 0) 
  (setq cdlist nil)
  (while (<= i (- (length srpinvertcoords) 4))
  (setq xp1 (nth i srpinvertcoords))
  (setq yp1 (nth (+ i 1) srpinvertcoords))
  (setq xp2 (nth (+ i 2) srpinvertcoords))
  (setq yp2 (nth (+ i 3) srpinvertcoords))
  (setq p1 (list xp1 yp1))
  (setq p2 (list xp2 yp1))
  (setq d (/ (distance  p1  p2 ) HF))
  (setq d1 (list d))
  (setq pdt (append pdt d1))
  (setq CD (+ CD d))
  (setq CD1pi (list CD))
  (setq cdlist (append cdlist CD1pi))
  (setq i (+ i 2))
  )
 ;(terpri)
 ;(princ pdt)
  ;(terpri)
 ;(princ CD)
  ;(terpri)
 ;(princ cdlist)
  ;(terpri)
 (princ)
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq ptsgati nil)
  (setq k 0)
  (setq i 0)
  (while (<= k (- (length srpinvertcoords) 1)) 
  (setq i 0)
  (setq EPODI (nth k srpinvertcoords))
  ;(princ EPODI)
  ;(terpri)
	    (while (<= i (- (length srpgradecoords) 4))
		;(princ (nth i srpgradecoords) ) (princ (nth (+ i 2) srpgradecoords))
		  (if (and (<= (nth i srpgradecoords)  EPODI) (>= (nth (+ i 2) srpgradecoords) EPODI)) 
		    (PROGN 
			  (setq xp1 (nth i srpgradecoords))
              (setq yp1 (nth (+ i 1) srpgradecoords))
              (setq xp2 (nth (+ i 2) srpgradecoords))
              (setq yp2 (nth (+ i 3) srpgradecoords))
              (setq p1 (list xp1 yp1))
              (setq p2 (list xp2 yp2))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq gradelevel (+ (* slp  EPODI) b))
			  (setq gradelevel (+ datum (/ (- gradelevel (cadr datumpt)) VF))) 
			  (setq gradelevel (list gradelevel))
              (setq ptsgati (append ptsgati gradelevel))
			  (setq i (length srpgradecoords))		   			   
		     )
		  )		  
			  (setq i (+ i 2))
	     )
	(setq k (+ k 2))	 
  )
  
  (if (or (< (nth (- (length srpgradecoords) 2) srpgradecoords) 
  (nth (- (length srpinvertcoords) 2) srpinvertcoords) )   
  (>  (car srpgradecoords)  (car srpinvertcoords) )  ) 
  (progn (alert "Please make sure that the ground polyline extends to or beyond the pipe invert level polyline and try again.")
  (quit)))
  (setq n (* (length ptsi) 2))
 ;(princ ptsg)
 ;(terpri)
 ;(princ ptsgati)
 ;;;;;;;;;;;;;;;;;;;;;
 
 (setq bendnames nil)
 (if (= copycros 0)
 (progn
 (if (= selpanno 1)
 (progn
 (setq i 1)
 (while (<= i  (/ n 2) )
 (setq is (rtos i 2 2))
 (setq btext (strcat "\n Select Point "is " Anno.:"))
 ;(terpri)
 ;(princ btext)
 ;(prompt "\n >> Select Pipe Polyline: ")
 (setq o2 (entsel btext))
 (if (/= o2 nil)
 (progn (setq bendname (cdr (assoc 1(entget (setq o1(car o2)))))) (princ bendname)) (setq bendname nil))
  ;(setq bendname (getstring "\nSpecify Point Name: "))
  ;(if (/= bendname nil) (progn
 (setq bendname (list bendname))
 (setq bendnames (append bendnames bendname))
 ;))
 (setq i (+ i 1))
  )
  )
  (progn
 (setq i 1)
 (while (<= i  (/ n 2) )
  (setq bendname nil)
 (setq bendname (list bendname))
 (setq bendnames (append bendnames bendname))
 ;))
 (setq i (+ i 1))
  )
  )
  ))
  (progn
 (setq i 1)
 (while (<= i  (/ n 2) )
 ;(setq is (rtos i 2 2))
 ;(setq btext (strcat ">> Select Bend"is " name:"))
 ;(terpri)
 ;(princ btext)
 ;(prompt "\n >> Select Pipe Polyline: ")
  (setq bendname nil)
  ;(setq bendname (getstring "\nSpecify Point Name: "))
  ;(if (/= bendname nil) (progn
 (setq bendname (list bendname))
 (setq bendnames (append bendnames bendname))
 ;))
 (setq i (+ i 1))
  )
  )
  )
  
  )
 ; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Profile Drawing Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (DEFUN BRDR2 ()
	(setq minpd (* (apply 'min pdt) HF))
 (setq alpd 15)
 (SETVAR "CMDECHO"   0) ;commands executed during the lisp are not echoed in the command line
 (SETVAR "PICKBOX"   0) ;Sets the object selection target height, in pixels. When PICKBOX is set to 0, selection previewing of objects is not available.
 (SETVAR "OSMODE"    0) ;Sets running object snaps 0 means NONE.
 (SETVAR "DIMASZ"    0) ;Controls the size of dimension line and leader line arrowheads. Also controls the size of hook lines.
 (if (<= minpd alpd)
 (setq DH 35 DV 93 Z 0 EMN 0 MAXCD 0 EB "Y"  MAXR 0  MINR 10 XBASE 0) 
 (setq DH 35 DV 84 Z 0 EMN 0 MAXCD 0 EB "Y"  MAXR 0  MINR 10 XBASE 0) 
 )
(SETQ  DJN1 0 FTJN1 "" FTJN2 ""  TJN2 "" PDS 0  )
(SETQ PPB 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;========================================================================
; (setq ipt (getpoint "\nSpecify insertion point:"))
; (setq xipt (car ipt))
; (setq yipt (cadr ipt))
; (setq XBASE xipt)

   
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;========================================================================
 ;(SETQ   EMN (+ EMN         N))
 (SETQ MAXCD (+ MAXCD      CD))
 (SETQ MAXGE (+ (FIX MAXGE) 2)) ;Returns the conversion of a real number into the nearest smaller integer
 ;(princ MAXGE)
 (SETQ YBASE (- (FIX YBASE) 3))  
  (if (EQ EB "Y") (progn (setq   R (- MAXGE YBASE))
                         (if (> R MAXR)(setq MAXR R  ))  ))
    (SETVAR "REGENMODE" 1) ;Controls automatic regeneration of the drawing. 1 Allows automatic regeneration for certain commands. 0 Prevents automatic regeneration for commands (for example, when thawing layers)
    (IF (EQ EB "Y") (SETQ R (+ MAXR 2))
                    (SETQ R (+ (- MAXGE YBASE) 2)))
                    (IF   (< R MINR) (SETQ R MINR))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;					
 ; (setq YBASE yipt) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
    (SETQ   BHU (*  R  VF))
    (SETQ    BH (+ DV BHU))
    (SETQ    BW (* (- MAXCD XBASE) HF))
    (SETQ  BASE (LIST   XBASE   YBASE))
    (SETQ  BASE (MAPCAR '* BASE (LIST HF VF))) ;BASE POINT (0, DATUM LEVEL)
    (SETQ    C1 (MAPCAR '- BASE (LIST (+ DH 20) (+ DV 10)))) ; C1 (0 - 55, DATUM LEVEL -100)
    (SETQ    C2 (MAPCAR '+ BASE (LIST (+ BW 20) (+ BHU 10)))) ; C2 (0 + CUM DIST +20,DATUM LEVEL + BHU+10)
    (COMMAND "ZOOM" "W" C1 C2)
    (COMMAND "BASE" (MAPCAR '- BASE (LIST (+ DH DH) DV))) ;Sets the insertion base point for the current drawing.
                                                         ; ;DWG BASE POINT =(0-70,DATUM LEVEL -90)
    (COMMAND "LAYER" "SET" "C-PRFL-TEXT" "")
    (COMMAND "STYLE" "RS3" "ROMANS" "3" "" "" "" "" "")
    (SETQ TP (MAPCAR '- BASE   (LIST (+ (/ DH 2) 10) (/ DV 28))))
    (COMMAND "TEXT" "M" TP "0"   "EXISTING")  ;"DIA.(mm) 
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"      "GROUND")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"      "LEVEL")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"      "TOP LEVEL")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
	
(if  (= MODE "M")(COMMAND "TEXT" "M" TP "0"  "OF RELOCATED"))
(if  (or (= MODE "C") (= MODE "A")(= MODE "S") (= MODE "I"))  (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor) (= pipecolor existpipecolor))
                   (progn
                     (if (= pipecolor relocpipecolor) (COMMAND "TEXT" "M" TP "0"  "OF RELOCATED"))  
                     (if (= pipecolor temppipecolor)   (COMMAND "TEXT" "M" TP "0"  "OF TEMPORARY"))  
                     (if (= pipecolor existpipecolor) (COMMAND "TEXT" "M" TP "0"  "OF EXISTING"))
                    ) (COMMAND "TEXT" "M" TP "0"  "OF RELOCATED"))
)
	
	
	
	
    ;(COMMAND "TEXT" "M" TP "0"  "OF RELOCATED")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "PIPE")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "INVERT LEVEL")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
	
(if   (= MODE "M")   (COMMAND "TEXT" "M" TP "0"  "OF RELOCATED"))
(if  (or  (= MODE "C") (= MODE "A") (= MODE "S") (= MODE "I")) (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor) (= pipecolor existpipecolor))
                   (progn
                     (if (= pipecolor relocpipecolor) (COMMAND "TEXT" "M" TP "0"  "OF RELOCATED"))  
                     (if (= pipecolor temppipecolor)   (COMMAND "TEXT" "M" TP "0"  "OF TEMPORARY"))  
                     (if (= pipecolor existpipecolor) (COMMAND "TEXT" "M" TP "0"  "OF EXISTING"))
                    ) (COMMAND "TEXT" "M" TP "0"  "OF RELOCATED"))
)	
	
	
    ;(COMMAND "TEXT" "M" TP "0" "OF RELOCATED")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "PIPE")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "SLOPE")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "DIAMETER(mm)")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "PIPE MATERIAL")
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "PART.DIST.(m)")
	(if (<= minpd alpd)
	(progn
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 8.857142857))))
    (COMMAND "TEXT" "M" TP "0"  "CUM.DIST.(m)")
	)
	(progn
    (SETQ TP (MAPCAR '- TP     (LIST  0 (/ DV 14))))
    (COMMAND "TEXT" "M" TP "0"  "CUM.DIST.(m)")
	)
	)
    (SETQ TP (MAPCAR '- BASE   (LIST (+ (/ DH 2) 10) (/ DV (- (* VF -1) 2)))))
    (COMMAND "TEXT" "M" TP "0"  "DATUM LEVEL")
  
    (SETQ  X (MAPCAR '- BASE  (LIST (+ (/ DH 2) 10) 0)))
    (SETQ TP (MAPCAR '+  X   '(0  1)))
    (SETQ  X 0 )
    (WHILE (< X R)
           (SETQ     X (+  X         1))
           (SETQ   TEL (RTOS YBASE 2 2))
           (COMMAND "TEXT" "C" TP "0" TEL)
           (SETQ    TP (MAPCAR '+ TP (LIST  0 VF)))
           (SETQ YBASE (+  YBASE 1 ))             )

    (COMMAND "LAYER" "SET" "C-PRFL-BORD" "")
    (SETQ C1 (MAPCAR '- BASE (LIST (+ DH 10)  DV )))
    (SETQ C2 (MAPCAR '+   C1 (LIST  0 BH)))
    (SETQ C3 (MAPCAR '+   C1 (LIST (+ (+ DH 20) BW) 0)))
    (SETQ C4 (MAPCAR '+   C2 (LIST (+ (+ DH 20) BW) 0)))
    (SETQ C5 (MAPCAR '+   C1 (LIST DH  0)))
    (SETQ C6 (MAPCAR '+   C2 (LIST DH  0)))
	(setq bpts (list C1 C2 C4 C3 C1))
	(draw_poly bpts)
    ;(COMMAND "PLINE" C1 C2 C4 C3 C1 "")
    (COMMAND "LINE" C5 C6 "")
    (SETQ C7 (MAPCAR '+   C1 (LIST 0 (+ DV VF))))
    (SETQ C8 (MAPCAR '+   C1 (LIST  DH  (+ DV VF))))
    (COMMAND "LINE" C7 C8 "")
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ C9 (MAPCAR '-   BASE (LIST  (+ DH 10) (* VF -1))))
    (SETQ X  1 Y VF)
    (SETQ RR (- R 1))
    (WHILE (< X RR)
           (SETQ X (+ X   1))
           (COMMAND "OFFSET"  Y  C9 C6 "")
           (SETQ Y (+ Y  VF))              )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ C2 (MAPCAR '+   C1 (LIST  0  DV)))
    (SETQ C4 (MAPCAR '+   C3 (LIST  0  DV)))
    (COMMAND "LINE" C2 C4 "")
   
   
    (COMMAND "LAYER" "SET" "C-PRFL-GRID" "")
    (SETQ C2 (MAPCAR '+   C1 (LIST  DH  (+ DV VF))))
    (SETQ C4 (MAPCAR '+   C3 (LIST  0  (+ DV VF))))
    (COMMAND "LINE" C2 C4 "")

    (SETQ C9 (MAPCAR '+   BASE (LIST  0 VF)))
    (SETQ X  1 Y VF)
    (SETQ RR (- R 1))
    (WHILE (< X RR)
           (SETQ X (+ X   1))
           (COMMAND "OFFSET"  Y  C9 C6 "")
           (SETQ Y (+ Y  VF))              )
  
    (COMMAND "OFFSET" (* (/ DV 14)  3) BASE C5 "")
    (COMMAND "OFFSET" (* (/ DV 14)  6) BASE C5 "")
    (COMMAND "OFFSET" (* (/ DV 14)  9) BASE C5 "")
    (COMMAND "OFFSET" (* (/ DV 14) 10) BASE C5 "")
    (COMMAND "OFFSET" (* (/ DV 14) 11) BASE C5 "")
    (COMMAND "OFFSET" (* (/ DV 14) 12) BASE C5 "")
    (COMMAND "OFFSET" (* (/ DV 14) 13) BASE C5 "")
   
    (COMMAND "LAYER" "SET" "C-PRFL-TEXT" "")
    (COMMAND "STYLE" "RD5" "ROMAND"  "5" "" "" "" "" "" )
    (COMMAND "COLOR" "7")
    (SETQ TP (MAPCAR '- C3 (LIST (/ (+ (+ DH 20) BW) 2) 20)))
    (COMMAND "TEXT" "C" TP  "0"   TITLE  )
    (COMMAND "COLOR" "BYLAYER")
    (COMMAND "STYLE" "RD3" "ROMAND"  "3" "" "" "" "" "" )
    (SETQ TP (MAPCAR '- TP '(0 10)))
    (COMMAND "TEXT" "C" TP  "0" SC)
    (SETVAR  "REGENMODE" 0)

    (SETQ C4   (MAPCAR '- C4 (LIST 10 (/ DV 5))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq i 0)
 (COMMAND   "LAYER" "SET" "C-PRFL-CLIN" "")
 (setq pt (mapcar '+ BASE (list 0 BHU)))
 (command "line" BASE pt "")
 (setq pt1 BASE)
 (setq n (* (+ (length cdlist) 1) 2))
 (while (<= i (- (/ n 2) 2))
 (setq pt2 (mapcar '+ pt1 (list (* (nth i pdt) HF) 0) ))
 (command "OFFSET" (* (nth i pdt) HF) pt1 pt2 "")
 (setq pt1 pt2)
 (setq i (+ i 1))
  )
(COMMAND   "LAYER" "SET" "C-PRFL-BORD" "")
(if (<= minpd alpd)
(setq pt (mapcar '- BASE (list 0 (- DV (/ DV 6.2)))))
(setq pt (mapcar '- BASE (list 0 (- DV (/ DV 14)))))
)
(command "line" BASE pt "")
(setq pt1 (mapcar '+ BASE (list (* MAXCD HF) 1)))
(command "OFFSET" (* MAXCD HF) BASE pt1 "")

(setq i 0)
(setq pt1 BASE)
(if (<= minpd alpd)
(setq pt2 (mapcar '- BASE (list 0 (/ DV 1.722222222222222))))
(setq pt2 (mapcar '- BASE (list 0 (/ DV 1.555555555555555))))
)
;(setq pt2 (mapcar '- BASE (list 0 (/ DV 1.4))))
(while (<= i (- (/ n 2) 3))
 (setq pt3 (mapcar '+ pt1 (list (* (nth i pdt) HF) 0 )) )
 (setq pt4 (mapcar '+ pt2 (list (* (nth i pdt) HF) 0 )) )
 (command "line" pt3 pt4 "")
 (setq pt1 pt3)
 (setq pt2 pt4)
 (setq i (+ i 1)) 
 )
 (setq i 0)
 (if (<= minpd alpd)
 (progn
 (setq pt1 (mapcar '- BASE (list 0 (/ DV 1.291666667))))
 (setq pt2 (mapcar '- BASE (list 0 (/ DV 1.192307692))))
  )
 (progn
 (setq pt1 (mapcar '- BASE (list 0 (/ DV 1.16666666666666666))))
 (setq pt2 (mapcar '- BASE (list 0 (/ DV 1.076923077))))
)
)
(while (<= i (- (/ n 2) 3))
 (setq pt3 (mapcar '+ pt1 (list (* (nth i pdt) HF) 0 )) )
 (setq pt4 (mapcar '+ pt2 (list (* (nth i pdt) HF) 0 )) )
 (command "line" pt3 pt4 "")
 (setq pt1 pt3)
 (setq pt2 pt4)
 (setq i (+ i 1)) 
 )
 )
 ; =======================================================================================================
  (DEFUN TEXTT () 
  (setq rtod 0.4) ;right text offset distance
  (setq tod -0.5) ;left text offset distance
 (setq minpd (* (apply 'min pdt) HF))
 (setq alpd 15)
 (setq i 1)
 (COMMAND   "LAYER" "SET" "C-PRFL-TEXT" "")
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2.5" "" "" "" "" "" )
 (setq pt1 (mapcar '+ BASE (list 0 (- BHU 2))))
 (if (< minpd alpd)
 (progn
 (setq pt2 (mapcar '- BASE (list 0 (/ DV 10.3333333333333333))))
 (setq pt3 (mapcar '- BASE (list 0 (/ DV 3.44444444444444444))))
 (setq pt4 (mapcar '- BASE (list 0 (/ DV 2.06666666666666666))))
 (setq pt5 (mapcar '- BASE (list 0 (/ DV 1.083916084))))
 (setq pt11 (mapcar '+ BASE (list (* (/ (car pdt) 2) HF) (/ DV -1.24))))
 )
 (progn
 (setq pt2 (mapcar '- BASE (list 0 (/ DV 9.33333333333333333))))
 (setq pt3 (mapcar '- BASE (list 0 (/ DV 3.11111111111111111))))
 (setq pt4 (mapcar '- BASE (list 0 (/ DV 1.86666666666666666))))
 (setq pt5 (mapcar '- BASE (list 0 (/ DV 1.037037037037))))
 (setq pt11 (mapcar '+ BASE (list (* (/ (car pdt) 2) HF) (/ DV -1.12))))
 )
 )
 (setvar "dimzin" 2)
 (COMMAND   "TEXT" "R" (mapcar '+ pt1 (list tod 0)) "90" (car bendnames)  )
 (if (or  (= MODE "C")(= MODE "S"))
 (progn
 (COMMAND   "TEXT" "C" (mapcar '+ pt2 (list tod 0)) "90" (rtos (car ptsgati) 2 2))
 ) 
 (progn
 (COMMAND   "TEXT" "C" (mapcar '+ pt2 (list tod 0)) "90" (rtos (car ptsg) 2 2))
 )
 )

  (COMMAND   "TEXT" "C" (mapcar '+ pt3 (list tod 0))"90" (rtos (+ (/ (float dia) 1000) (car ptsi)) 2 2))
  (COMMAND   "TEXT" "C" (mapcar '+ pt4 (list tod 0)) "90" (rtos (car ptsi) 2 2))
 ;;;;
 (if (< minpd alpd)
 (COMMAND   "TEXT" "M" pt5 "90" "0.0"  )
 (COMMAND   "TEXT" "M" pt5 "0" "0.0"  )
 )
 ;;;;
  (setvar "dimzin" 8)
 (if (and (< (* (nth 0 pdt) HF) 5.5) (/= (strlen (rtos  (nth 0 pdt) 2 1)) 1));(/= (rtos  (nth i pdt) 2 1) "1") (/= (rtos  (nth i pdt) 2 1) "2"))
 (progn
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2" "" "" "" "" "" )
 (if (< (* (nth 0 pdt) HF) 6) (COMMAND   "STYLE" "RS2-"  "ROMANS" "1.7" "" "" "" "" "" ))
 (COMMAND   "TEXT" "M" pt11 "90" (rtos (car pdt) 2 1)  )
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2.5" "" "" "" "" "" )
 )
 (progn
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2.5" "" "" "" "" "" )
 (COMMAND   "TEXT" "M" pt11 "0" (rtos (car pdt) 2 1)  )
 )
 )

  (setq k 1)
 (while (<= i  (- (/ n 2) 1)) 
 (setvar "dimzin" 2)
 (setq pt6 (mapcar '+ pt1 (list (* (nth (- i 1) pdt) HF) 0 )) )
 (setq pt7 (mapcar '+ pt2 (list (* (nth (- i 1) pdt) HF) 0 )) )
 (setq pt8 (mapcar '+ pt3 (list (* (nth (- i 1) pdt) HF) 0 )) )
 (setq pt9 (mapcar '+ pt4 (list (* (nth (- i 1) pdt) HF) 0 )) )
 (setq pt10 (mapcar '+ pt5 (list (* (nth (- i 1) pdt) HF) 0 )) )
 (if (<= i (- (/ n 2) 2))(setq pt12 (mapcar '+ pt11 (list (* (+ (/ (nth (- i 1) pdt) 2) (/ (nth i pdt) 2)) HF) 0 ))))
  (if (and (< (* (nth (- i 1) pdt) HF) 6) (/= i (- (/ n 2) 2)) ) (COMMAND   "TEXT" "TR" (mapcar '+ pt6 (list rtod 0)) "90" (nth i bendnames)  )(COMMAND   "TEXT" "R" (mapcar '+ pt6 (list tod 0))  "90" (nth i bendnames)  ))
 (if (or  (= MODE "C")(= MODE "S")) (if (and (< (* (nth (- i 1) pdt) HF) 6) (/= i (- (/ n 2) 2)) ) (COMMAND   "TEXT" "TC" (mapcar '+ pt7 (list rtod 0)) "90" (rtos (nth k ptsgati) 2 2))(COMMAND   "TEXT" "C" (mapcar '+ pt7 (list tod 0)) "90" (rtos (nth k ptsgati) 2 2))) 
 (if (and (< (* (nth (- i 1) pdt) HF) 6) (/= i (- (/ n 2) 2)) ) (COMMAND   "TEXT" "TC" (mapcar '+ pt7 (list rtod 0)) "90" (rtos (nth k ptsg) 2 2))(COMMAND   "TEXT" "C" (mapcar '+ pt7 (list tod 0)) "90" (rtos (nth k ptsg) 2 2))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (and (< (* (nth (- i 1) pdt) HF) 6) (/= i (- (/ n 2) 2)) ) (COMMAND   "TEXT" "TC" (mapcar '+ pt9 (list rtod 0)) "90" (rtos (nth k ptsi) 2 2))(COMMAND   "TEXT" "C" (mapcar '+ pt9 (list tod 0))  "90" (rtos (nth k ptsi) 2 2)))
 (if (and (< (* (nth (- i 1) pdt) HF) 6) (/= i (- (/ n 2) 2)) ) (COMMAND   "TEXT" "TC" (mapcar '+ pt8 (list rtod 0)) "90" (rtos (+ (/ (float dia) 1000) (nth k ptsi)) 2 2)) (COMMAND   "TEXT" "C" (mapcar '+ pt8 (list tod 0)) "90" (rtos (+ (/ (float dia) 1000) (nth k ptsi)) 2 2)))
 ;;;;
 (setvar "DIMZIN" 1)
 (if (< minpd alpd)
 (if (and (< (* (nth (- i 1) pdt) HF) 6) (/= i (- (/ n 2) 2)) ) (COMMAND   "TEXT" "TC" pt10 "90" (rtos (nth (- i 1) cdlistft) 2 1))(COMMAND   "TEXT" "M" pt10 "90" (rtos (nth (- i 1) cdlistft) 2 1)))
 (COMMAND   "TEXT" "M" pt10 "0" (rtos (nth (- i 1) cdlistft) 2 1))
 ;;;;(COMMAND   "TEXT" "MR" pt10 "90" (rtos (nth (- i 1) cdlistft) 2 1))
 )
 (setvar "DIMZIN" 8)
 ;;;;
 (if (<= i (- (/ n 2) 2))
 (if (and (< (* (nth i pdt) HF) 5.5) (/= (strlen (rtos  (nth i pdt) 2 1)) 1));(/= (rtos  (nth i pdt) 2 1) "1") (/= (rtos  (nth i pdt) 2 1) "2"))
 (progn
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2" "" "" "" "" "" )
 (if (< (* (nth i pdt) HF) 6) (COMMAND   "STYLE" "RS2-"  "ROMANS" "1.7" "" "" "" "" "" ))
 (COMMAND   "TEXT" "M" pt12 "90" (rtos  (nth i pdt) 2 1)  )
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2.5" "" "" "" "" "" )
 )
 (progn 
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2.5" "" "" "" "" "" )
 (COMMAND   "TEXT" "M" pt12 "0" (rtos  (nth i pdt) 2 1)  )
 )
 )
 )
 (setq pt1 pt6)
 (setq pt2 pt7)
 (setq pt3 pt8)
 (setq pt4 pt9)
 (setq pt5 pt10)
 (setq pt11 pt12)
 (setq i (+ i 1))
 (setq k (+ k 1))
 )
 (setvar "dimzin" 8)
 (if (< minpd alpd)
 (setq pt1 (mapcar '+ BASE (list (/ (* MAXCD HF) 2) (/ DV -1.476190476))))
 (setq pt1 (mapcar '+ BASE (list (/ (* MAXCD HF) 2) (/ DV -1.33333333333333333))))
 )
 (command "TEXT" "M" pt1 "0" rdia)
 (if (< minpd alpd)
 (setq pt1 (mapcar '+ BASE (list (/ (* MAXCD HF) 2) (/ DV -1.347826087))))
 (setq pt1 (mapcar '+ BASE (list (/ (* MAXCD HF) 2) (/ DV -1.217391304))))
 )
 (if  (= MODE "M")  (command "TEXT" "M" pt1 "0" "DUCTILE IRON"))
(if  (or (= MODE "C") (= MODE "A") (= MODE "S") (= MODE "I")) (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor) (= pipecolor existpipecolor))
                   (progn
                     (if (= pipecolor relocpipecolor) (command "TEXT" "M" pt1 "0" "DUCTILE IRON"))  
                     (if (= pipecolor temppipecolor)   (command "TEXT" "M" pt1 "0" "DUCTILE IRON"))  
                     (if (= pipecolor existpipecolor) (command "TEXT" "M" pt1 "0" "ASBESTOS CEMENT"))
                    ) (command "TEXT" "M" pt1 "0" "DUCTILE IRON"))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(COMMAND   "LAYER" "SET" "C-PRFL-TEXT" "")
 (COMMAND   "STYLE" "RS2-"  "ROMANS" "2.5" "" "" "" "" "" )
 (setq i 0)
 (setq flag 1000)
 (setq epds (car pdt))
 (if (< minpd alpd)
 (setq pt1 (mapcar '+ BASE (list 0 (/ DV -1.631578947))))
 (setq pt1 (mapcar '+ BASE (list 0 (/ DV -1.473684211))))
 )
 (while (<= i (- (/ n 2) 3))
 (setq slpp1 (abs (nth i slpl)))
 (setq slpp2 (abs (nth (+ i 1) slpl)))
 ;;;;
 (cond ((= sstyle1 1) (setq slpp1 (rtos slpp1 2 5)) (setq slpp2 (rtos slpp2 2 5)) )
       ((= sstyle2 1) (setq slpp1 (strcat (rtos (* slpp1 100) 2 2)"%")) (setq slpp2  (strcat (rtos (* slpp2 100) 2 2)"%")) )
       ((= sstyle3 1) (setq slpp1 (strcat (rtos (* slpp1 1000) 2 2)"\U+2030")) (setq slpp2 (strcat (rtos (* slpp2 1000) 2 2)"\U+2030")) )
  )
 ;;;;
 ; (setq slpp1 (rtos slpp1 2 5))
 ; (setq slpp2 (rtos slpp2 2 5))
 (if (=  slpp1 slpp2) (progn 

 (if (and (= i (- (/ n 2) 3)) (= flag 1000)) (progn 
 
 (setq epds (+  (* epds 1)  (- CD (nth i cdlist))))
 (setq pt2 (mapcar '+ pt1 (list (* (/  epds 2)  HF) 0)))
 ;(command   "TEXT" "M" pt2 "0" slpp2)
    (entmake
      (list
      (cons 0 "MTEXT") ;; Entity Name
      (cons 100 "AcDbEntity") ;; Subclass Marker
      (cons 410 "Model") ;; Space
      (cons 8 "C-PRFL-TEXT") ;; Layer
      (cons 100 "AcDbMText") ;; Subclass Marker
      (cons 10 pt2) ;; Insertion Point
      (cons 40 2.5) ;; Text Height
      (cons 71 5) ;; Attachment Point (Mid-Cen)
      (cons 1 slpp2) ;; Text Content
      (cons 7 "RS2-")))

  ) (if (/= flag 1000) (progn (setq epds (+ (nth (+ i 1) cdlist) (nth flag cdlist))))(setq epds (nth (+ i 1) cdlist))))
  
  (if (and (= i (- (/ n 2) 3)) (/= flag 1000)) (progn

  (setq pt2 (mapcar '+ pt1 (list (* (/  epds 2)  HF) 0)))
  ;(command   "TEXT" "M" pt2 "0" slpp2)
  (entmake
      (list
      (cons 0 "MTEXT") ;; Entity Name
      (cons 100 "AcDbEntity") ;; Subclass Marker
      (cons 410 "Model") ;; Space
      (cons 8 "C-PRFL-TEXT") ;; Layer
      (cons 100 "AcDbMText") ;; Subclass Marker
      (cons 10 pt2) ;; Insertion Point
      (cons 40 2.5) ;; Text Height
      (cons 71 5) ;; Attachment Point (Mid-Cen)
      (cons 1 slpp2) ;; Text Content
      (cons 7 "RS2-")))
  ))
 
 (setq i (+ i 1))) )

 (if (/= slpp1 slpp2)(progn
 (setq pt2 (mapcar '+ pt1 (list (* (/  epds 2)  HF) 0)))
 ;(command   "TEXT" "M" pt2 "0" slpp1)
   (entmake
      (list
      (cons 0 "MTEXT") ;; Entity Name
      (cons 100 "AcDbEntity") ;; Subclass Marker
      (cons 410 "Model") ;; Space
      (cons 8 "C-PRFL-TEXT") ;; Layer
      (cons 100 "AcDbMText") ;; Subclass Marker
      (cons 10 pt2) ;; Insertion Point
      (cons 40 2.5) ;; Text Height
      (cons 71 5) ;; Attachment Point (Mid-Cen)
      (cons 1 slpp1) ;; Text Content
      (cons 7 "RS2-")))
 
 (if (= flag 1000) (progn
 (if (< minpd alpd)
  (progn
(setq pttt1 (mapcar '+ BASE (list (*  epds  HF) (/ DV -1.7222222222222222))))
(setq pttt2 (mapcar '+ BASE (list (*  epds  HF) (/ DV -1.55))))
)
 (progn
(setq pttt1 (mapcar '+ BASE (list (*  epds  HF) (/ DV -1.555555555555555))))
(setq pttt2 (mapcar '+ BASE (list (*  epds  HF) (/ DV -1.4))))
)
)
(COMMAND   "LAYER" "SET" "C-PRFL-BORD" "")
(COMMAND   "LINE" pttt1 pttt2 "")
(COMMAND   "LAYER" "SET" "C-PRFL-TEXT" "")))

(if (/= flag 1000) (progn
(if (< minpd alpd)
(progn
(setq pttt1 (mapcar '+ BASE (list (*  (- epds (nth flag cdlist) )  HF) (/ DV -1.7222222222222222))))
(setq pttt2 (mapcar '+ BASE (list (*  (- epds (nth flag cdlist) )  HF) (/ DV -1.55))))
)
(progn
(setq pttt1 (mapcar '+ BASE (list (*  (- epds (nth flag cdlist) )  HF) (/ DV -1.555555555555555))))
(setq pttt2 (mapcar '+ BASE (list (*  (- epds (nth flag cdlist) )  HF) (/ DV -1.4))))
)
)
(COMMAND   "LAYER" "SET" "C-PRFL-BORD" "")
(COMMAND   "LINE" pttt1 pttt2 "")
(COMMAND   "LAYER" "SET" "C-PRFL-TEXT" "")))
 
 
 (setq epds (+ (* (nth i cdlist) 2) (nth (+ i 1) pdt)))
 (setq pt2 (mapcar '+ pt1 (list (* (/  epds 2)  HF) 0)))
 (if (= i (- (/ n 2) 3)) ;(command   "TEXT" "M" pt2 "0" slpp2)
   (entmake
      (list
      (cons 0 "MTEXT") ;; Entity Name
      (cons 100 "AcDbEntity") ;; Subclass Marker
      (cons 410 "Model") ;; Space
      (cons 8 "C-PRFL-TEXT") ;; Layer
      (cons 100 "AcDbMText") ;; Subclass Marker
      (cons 10 pt2) ;; Insertion Point
      (cons 40 2.5) ;; Text Height
      (cons 71 5) ;; Attachment Point (Mid-Cen)
      (cons 1 slpp2) ;; Text Content
      (cons 7 "RS2-")))
 )
 (setq flag i)
 (setq i (+ i 1))) )
 
 )
 (if (= (- (/ n 2) 3) -1) (progn
 (setq pt2 (mapcar '+ pt1 (list (* (/  CD 2)  HF) 0)))
 (setq slpp1 (abs (car slpl)))
 ;;;
 (cond ((= sstyle1 1) (setq slpp1 (rtos slpp1 2 5))  )
       ((= sstyle2 1) (setq slpp1 (strcat (rtos (* slpp1 100) 2 2)"%"))  )
       ((= sstyle3 1) (setq slpp1 (strcat (rtos (* slpp1 1000) 2 2)"\U+2030"))  )
  )
  ;;;
 ;(setq slpp1 (rtos slpp1 2 5))
 ;(command   "TEXT" "M" pt2 "0" slpp1)
   (entmake
      (list
      (cons 0 "MTEXT") ;; Entity Name
      (cons 100 "AcDbEntity") ;; Subclass Marker
      (cons 410 "Model") ;; Space
      (cons 8 "C-PRFL-TEXT") ;; Layer
      (cons 100 "AcDbMText") ;; Subclass Marker
      (cons 10 pt2) ;; Insertion Point
      (cons 40 2.5) ;; Text Height
      (cons 71 5) ;; Attachment Point (Mid-Cen)
      (cons 1 slpp1) ;; Text Content
      (cons 7 "RS2-")))
 )) 
 )
 ; =======================================================================================================
 (defun draw_grade_pipe ()  
 (setq pt BASE)
 (setq gptslist nil)
 (setq iptslist nil)
 (setq pdtg (cons 0 pdtg))
 (setq pdt (cons 0 pdt))
 (if (and (/= MODE "S") (/= MODE "C")) (progn
 (setq i 0)
 (setq grpt1 BASE)
 (setq ilpt1 BASE)
 (while (<= i (- (/ n 2) 1))
 (setq grpt1 (list (+ (car grpt1) (* (nth i pdt) HF) ) (* (nth i ptsg ) VF)))
 (setq gptslist (append gptslist (list grpt1)))
 (setq ilpt1 (list (+ (car ilpt1)  (* (nth i pdt) HF) ) (* (nth i ptsi ) VF)))
 (setq iptslist (append iptslist (list ilpt1)))
 (setq i (+ i 1))
  )
 (setq pdt (RemoveNth 0 pdt))
 (COMMAND   "LAYER" "SET" "C-PRFL-GRDE" "")
 (draw_poly gptslist)
 (COMMAND   "LAYER" "SET" "C-PRFL-PIPE" "")
 (COMMAND "COLOR" pipecolor)
 (draw_poly iptslist)
 ))
 (if (or (= MODE "C") (= MODE "S")) (progn
 (setq i 0)
 (setq grpt1 BASE)
 (while (<= i (- (length ptsg) 1))
 (setq grpt1 (list (+ (car grpt1) (* (nth i pdtg) HF) ) (* (nth i ptsg ) VF)))
 (setq gptslist (append gptslist (list grpt1)))
 (setq i (+ i 1))
 )
 (setq pdtg (RemoveNth 0 pdtg))
 (COMMAND   "LAYER" "SET" "C-PRFL-GRDE" "")
 (draw_poly gptslist)
 
 (setq i 0)
  (setq ilpt1 BASE)
  (while (<= i (- (length ptsi) 1))
 (setq ilpt1 (list (+ (car ilpt1)  (* (nth i pdt) HF) ) (* (nth i ptsi ) VF)))
 (setq iptslist (append iptslist (list ilpt1)))
 (setq i (+ i 1))
 )
 (setq pdt (RemoveNth 0 pdt))
 (COMMAND   "LAYER" "SET" "C-PRFL-PIPE" "")
 (COMMAND "COLOR" pipecolor)
 (draw_poly iptslist)

 ))
 
 (COMMAND "COLOR" "BYLAYER")
 (command "OFFSET" (* (/ (float dia) 1000) VF) ilpt1 (list (car ilpt1) (+ (cadr ilpt1) 1)) "")
 )
 ; =======================================================================================================
 (defun exist_change_color ( / clr ccr clrr)
 (setq clr (cdr (assoc 62 (setq ccr(acad_truecolordlg existingpipec)))))
 (cond ((= (length ccr) 3) (setq clrr (cdr (assoc 430 ccr)))) ((= (length ccr) 2) (setq clrr (cdr (assoc 420 ccr)))) (t (setq clrr clr)))
 (if (/= clr nil)
 (progn
 (setq width (dimx_tile  "epc")
      height (dimy_tile "epc"))
(start_image "epc")
(fill_image 0 0 width height clr)   ;1 = AutoCAD red.
(end_image)
;(set_tile "epct" (cond ((= clr 4) "cyan")(t (rtos clr 2 0))))
(set_tile "epct" (cond ((= clrr 4) "cyan")(t (rtos clrr 2 0))))
(setq existingpipec clr)
 ))
 )
 ; =======================================================================================================
  (defun temp_change_color ( / clr)
 (setq clr (cdr (assoc 62 (acad_truecolordlg temporarypipec))))
 (if (/= clr nil)
 (progn
 (setq width (dimx_tile  "tpc")
      height (dimy_tile "tpc"))
(start_image "tpc")
(fill_image 0 0 width height clr)   ;1 = AutoCAD red.
(end_image)
(setq temporarypipec clr)
 ))
 )
 ; =======================================================================================================
  (defun prop_change_color ( / clr)
 (setq clr (cdr (assoc 62 (acad_truecolordlg relocatedpipec))))
 (if (/= clr nil)
 (progn
 (setq width (dimx_tile  "ppc")
      height (dimy_tile "ppc"))
(start_image "ppc")
(fill_image 0 0 width height clr)   ;1 = AutoCAD red.
(end_image)
(setq relocatedpipec clr)
 ))
 )
 ; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; WP Main Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:wp ()  
  (setq chn nil)
  (setq chtext nil)
;;;--- Main application
;(timeini) ;intitialize time count for time function
(inivar)  ;save initial setvars
  ;;;--- Get the autoCAD Version
  (setq veri (atoi(substr (getvar "acadver") 1 2)))
  (if(> veri 15)
     (setq prefx (strcat (getvar "mydocumentsprefix") "\\")) 
     (setq prefx (getvar "dwgprefix"))
  )
  ;;;--- Load the dialog box from file
 (setq dcl_id (load_dialog "WPD.dcl"))
(setq ffgd (findfile "WPD.dcl"))
;(princ ffgd)
  ;;;--- Load the dialog definition from the file
  (if (not (new_dialog "WPD" dcl_id))
    (progn
      (alert "Could not find the WPD.DCL file!")
      (exit)
    )
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      Load the defaults     ;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (if(findfile (strcat prefx "WPD.dat"))
    (progn
      (if(setq fil(open (strcat prefx "WPD.dat") "r"))
        (progn
		  (set_tile "ModeC" (read-line fil))
          (set_tile "ModeA" (read-line fil))
		  (set_tile "ModeM" (read-line fil))
		  (set_tile "ModeS" (read-line fil))
		  (set_tile "ModeI" (read-line fil))
		  (set_tile "vscale" (rtos(distof (read-line fil))))
		  (set_tile "hscale" (rtos(distof (read-line fil))))
		  ;(set_tile "existingpipec" (rtos(distof (read-line fil)) 2 0))
		  ;(set_tile "temporarypipec" (rtos(distof (read-line fil)) 2 0))
		  ;(set_tile "relocatedpipec" (rtos(distof (read-line fil)) 2 0))
		  (setq existingpipec (rtos(distof (read-line fil)) 2 0))
		  (setq temporarypipec (rtos(distof (read-line fil)) 2 0))
		  (setq relocatedpipec (rtos(distof (read-line fil)) 2 0))
		  (set_tile "diameter" (rtos(distof (read-line fil)) 2 0))
		  (set_tile "sstyle1" (read-line fil))
		  (set_tile "sstyle2" (read-line fil))
		  (set_tile "sstyle3" (read-line fil))
		  (set_tile "exportdata" (read-line fil))
		  (set_tile "allowchainage" (read-line fil)) 
		  (set_tile "allowchainageprofile" (read-line fil))
		  (set_tile "startchainage" (rtos(distof (read-line fil)) 2 2))
		  (set_tile "chainageinterval" (rtos(distof (read-line fil)) 2 2))
		  (set_tile "chtextsize" (rtos(distof (read-line fil)) 2 2))
		  (set_tile "chstyle1" (read-line fil))
		  (set_tile "chstyle2" (read-line fil))
		  (set_tile "chstyle3" (read-line fil))
		  (set_tile "auto_anno_b" (read-line fil))
		  (set_tile "angtol" (rtos(distof (read-line fil)) 2 2))
		  (set_tile "copycros" (read-line fil))
		  (set_tile "selpanno" (read-line fil))
		  (set_tile "npanno" (read-line fil))
		  (set_tile "exportdata" (read-line fil))
          (close fil)
        )
      )
    )
    (progn 
           

	       (set_tile "ModeC" (cond ((= ModeC 1) "1") (t "0")))
		   (set_tile "ModeA" (cond ((= ModeA 1) "1") (t "0")))
		   (set_tile "ModeM" (cond ((= ModeM 1) "1") (t "0")))
		   (set_tile "ModeI" (cond ((= ModeI 1) "1") (t "0")))
		   (set_tile "ModeS" (cond ((= ModeS 1) "1") (t "0")))
		   (if (and (= ModeC nil) (= ModeA nil) (= ModeM nil) (= ModeI nil) (= ModeS nil) )  (set_tile "ModeA" "1")  ) 
		   (set_tile "vscale"  (cond ( (/= vscale nil) (rtos vscale 2 0) )  (t "100") )  )
		   (set_tile "hscale"  (cond ( (/= hscale nil) (rtos hscale 2 0) )  (t "500") )  )
		   (setq width (dimx_tile  "epc")
                 height (dimy_tile "epc"))
           (start_image "epc")
           (fill_image 0 0 width height (cond ( (/= existingpipec nil) existingpipec  )  (t 4) ))   ;4 = AutoCAD cyan.
		   (if (= existingpipec nil) (setq existingpipec 4))
		   (end_image)
		   (set_tile "epct" (cond ( (/= existingpipec nil) (cond ((= existingpipec 4) "cyan")(t (rtos existingpipec 2 0))))  (t "4")))
		   (setq width (dimx_tile  "tpc")
                 height (dimy_tile "tpc"))
           (start_image "tpc")
           (fill_image 0 0 width height (cond ( (/= temporarypipec nil) temporarypipec )   (t 6) ))   ;6 = AutoCAD magenta.
		   (if (= temporarypipec nil) (setq temporarypipec 6))
           (end_image)
		   (setq width (dimx_tile  "ppc")
                 height (dimy_tile "ppc"))
           (start_image "ppc")
           (fill_image 0 0 width height (cond ( (/= relocatedpipec nil) relocatedpipec  )  (t 221) ))
		   (if (= relocatedpipec nil) (setq relocatedpipec 221))
           (end_image)
		   ;(set_tile "existingpipec"  (cond ( (/= existingpipec nil) (rtos existingpipec 2 0) )  (t "130") )  )
		   ;(setq epcolorlist (list "Red" "Yellow" "Green" "Cyan" "Blue" "White" "Select Color..."))
		   ;(start_list "epcolorlist" 3)
           ;(mapcar 'add_list epcolorlist)
           ;(end_list)
		   
		   ;(set_tile "temporarypipec"  (cond ( (/= temporarypipec nil) (rtos temporarypipec 2 0) )  (t "6") )  )
		  ; (set_tile "relocatedpipec"  (cond ( (/= relocatedpipec nil) (rtos relocatedpipec 2 0) )  (t "221") )  )
		   (set_tile "diameter"  (cond ( (/= diameter nil) (rtos diameter 2 0) )  (t "800") )  )
		   (set_tile "sstyle1" (cond ((= sstyle1 1) "1") (t "0")))
		   (set_tile "sstyle2" (cond ((= sstyle2 1) "1") (t "0")))
		   (set_tile "sstyle3" (cond ((= sstyle3 1) "1") (t "0")))
		   (if (and (= sstyle1 nil) (= sstyle2 nil) (= sstyle3 nil) )  (set_tile "sstyle1" "1")  )
		   (set_tile "allowchainage"  (cond ( (= allowchainage 1) "1" )  (t "0") )  )
		   (set_tile "allowchainageprofile"  (cond ( (= allowchainageprofile 0) "0" )  (t "1") )  );;;;;;;;;;;;;;;;;;;;;;;;;
		   (set_tile "startchainage"  (cond ( (/= startchainage nil) (rtos startchainage 2 0) )  (t "0") )  )
	       (set_tile "chainageinterval"  (cond ( (/= chainageinterval nil) (rtos chainageinterval 2 0) )  (t "50") )  )
		   (set_tile "chtextsize"  (cond ( (/= chtextsize nil) (rtos chtextsize 2 0) )  (t "2.5") )  )
		   (set_tile "chstyle1" (cond ((= chstyle1 1) "1") (t "0")))
		   (set_tile "chstyle2" (cond ((= chstyle2 1) "1") (t "0")))
		   (set_tile "chstyle3" (cond ((= chstyle3 1) "1") (t "0")))
		   (set_tile "auto_anno_b"  (cond ( (= auto_anno_b nil) "1" ) ( (= auto_anno_b 0) "0" )  (t "1") )  );;;;;;;;;;;;;;;;;;;;;;;;;
		   (set_tile "angtol"  (cond 
		   ((and (>= (get_tile "diameter") "1") (<= (get_tile "diameter") "160"))   (set_tile "angtol" "5"))
	       ((and (>= (get_tile "diameter") "200") (<= (get_tile "diameter") "300"))   (set_tile "angtol" "4"))
	       ((and (>= (get_tile "diameter") "350") (<= (get_tile "diameter") "600"))   (set_tile "angtol" "3"))
	       ((and (>= (get_tile "diameter") "700") (<= (get_tile "diameter") "800"))   (set_tile "angtol" "2"))
        	 ((and (>= (get_tile "diameter") "900") (<= (get_tile "diameter") "1100"))   (set_tile "angtol" "1.5"))
	       ((and (>= (get_tile "diameter") "1200") (<= (get_tile "diameter") "1800"))   (set_tile "angtol" "1.5"))
	     	( (/= angtol nil) (rtos angtol 2 2) )
            (t (set_tile "angtol" "1.5"))			
		   )  )
		   (set_tile "copycros" (cond ((= copycros 1) "1") (t "0")))
		   (set_tile "selpanno" (cond ((= selpanno 1) "1") (t "0")))
		   (set_tile "npanno" (cond ((= npanno 1) "1") (t "0")))
		   (if (and (= copycros nil) (= selpanno nil) (= npanno nil) )  (set_tile "npanno" "1")  )
		   
		   (set_tile "exportdata" (cond ((= exportdata 1) "1") (t "0")))
		   
		   
	 )
  )
  
  ;;;--- Set the override mode to be enabled or disabled
  (changemode)
  (changechainagemode)
  (changeautoanno)
  ;;;--- If an action event occurs, do this...
  (action_tile "mode" "(changemode) (changeautoanno)")
  (action_tile "allowchainage" "(changechainagemode)")
  (action_tile "allowchainageprofile" "(changechainagemode)")
  (action_tile "auto_anno_b" "(changeautoanno)")
  (action_tile "diameter" "(changeautoanno)")
  (action_tile "about" "(aboutfunc)")
  (action_tile "ok"  "(saveVars)(setq ddiag 1)(done_dialog)")
  (action_tile "cancel"  "(setq ddiag 2)(done_dialog)")
  (action_tile "help"  "(wp_help)")
  (action_tile "epc" "(exist_change_color)")
  (action_tile "tpc" "(temp_change_color)")
  (action_tile "ppc" "(prop_change_color)")
  ;;;--- Display the dialog box
  (start_dialog)

  ;;;-- Unload the dialog box
  (unload_dialog dcl_id)

  ;;;--- Start the routine
  (if(= ddiag 1)
    (progn
	
  ;(princ exportdata)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
 (command "._UNDO" "_Begin")          
 (vl-load-com)
 (setvar "DIMZIN" 8)
 (setq existpipecolor existingpipec)
 (setq temppipecolor temporarypipec)
 (setq relocpipecolor relocatedpipec)
 ; (setq existpipecolor 130)
 ; (setq temppipecolor 6)
 ; (setq relocpipecolor 221)
  (SETVAR "CMDECHO"   0)
 ; (setq VS (getint "\nEnter Profile Vertical Scale (Default 1:100) 1:"))
 ; (setq HS (getint "\nEnter Profile Horizontal Scale (Default 1:500) 1:"))
 (setq HS hscale)
 (setq VS vscale)
 
 (if (= VS nil) (setq VS 100))
 (if (= HS nil) (setq HS 500))
 

 (SETQ    HF (/ 1000.0 HS))
 (SETQ    VF (/ 1000.0 VS))
 (SETVAR "OSMODE" 16383)
 (COMMAND "COLOR" "BYLAYER")
 (COMMAND "LAYER" "M" "C-ANNO-CHNG" "C" "7"   "" "L" "CONTINUOUS" "" "")
 (COMMAND "LAYER" "M" "C-PRFL-TEXT" "C" "7"   "" "L" "CONTINUOUS" "" "")
 (COMMAND "LAYER" "M" "C-PRFL-BORD" "C" "7"   "" "L" "CONTINUOUS" "" "")
 (COMMAND "LAYER" "M" "C-PRFL-GRID" "C" "8"  "" "L" "CONTINUOUS" "" "")
 (COMMAND "LAYER" "M" "C-PRFL-CLIN"   "C" "8" "" "L" "CONTINUOUS" "" "")
 (COMMAND "LAYER" "M" "C-PRFL-MHOL"  "C" "191"   "" "L" "CONTINUOUS" "" "")
 (COMMAND "LAYER" "M" "C-PRFL-GRDE"  "C" "1"   "" "L" "CONTINUOUS" "" "")
 (COMMAND "LAYER" "M" "C-PRFL-PIPE"  "C" relocpipecolor   "" "L" "CONTINUOUS" "" "")  
 (SETQ VS  (RTOS VS 2 0) HS  (RTOS HS 2 0 )) 
 (SETQ SC  (STRCAT "( VER. SCALE = 1/"VS "     HOR. SCALE = 1/"HS " )")) 

(setq Z1 2)
(setq MODE "UU")
;;;;;;;;;;;;;;;;;;;;;;;;;
(if(= ModeC 1)
        (setq MODE "C")
 )	
(if(= ModeA 1)
        (setq MODE "A")
 )	
 (if(= ModeM 1)
        (setq MODE "M")
 )
 (if(= ModeS 1)
        (setq MODE "S")
 )
 (if(= ModeI 1)
        (setq MODE "I")
 )
;;;;;;;;;;;;;;;;;;;;;;;;;
(while (< Z1 1)
(setq MODE (getstring "\nChoose Mode: (C) to use contours, Mode: (A) to automatically get the pipe extremities , (M) to manually pick the points , (S) for Second Run mode or (I) to import Data:  " ))
(if (or (= MODE "c") (= MODE "C")(= MODE "a") (= MODE "A") (= MODE "m") (= MODE "M") (= MODE "s") (= MODE "S") (= MODE "i") (= MODE "I")) (setq Z1 2))
)
(if (= MODE "c") (setq MODE "C"))
(if (= MODE "a") (setq MODE "A"))
(if (= MODE "m") (setq MODE "M"))
(if (= MODE "s") (setq MODE "S"))
(if (= MODE "i") (setq MODE "I"))

(if (= MODE "C") (progn (contourmode) (ptinv) (timeini) ));;;;;;;;;;;;;;;;;;;;;;;;;;here
(if (= MODE "A") (progn (autogetpts) (ptgrinv) (timeini))) 
(if (= MODE "M") (progn (mangetpts) (timeini)))
(if (= MODE "S") (progn (secondrun) (timeini)))
(if (= MODE "I") (progn (import-data) (timeini)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (/= MODE "I") (progn
 (setq dia diameter)
(setq rdia (rtos dia 2 0))
))
(if  (= MODE "I")   (setq TITLE    TITLExls))
(if  (= MODE "M")   (progn (setq pipecolor relocpipecolor) (setq TITLE      (strcat "RELOCATED WATER LINE DN" rdia))))
(if  (or (= MODE "C") (= MODE "A") (= MODE "S")) (if (or (= pipecolor relocpipecolor) (= pipecolor temppipecolor) (= pipecolor existpipecolor))
                   (progn
                     (if (= pipecolor relocpipecolor) (setq TITLE      (strcat "RELOCATED WATER LINE DN" rdia)))  
                     (if (= pipecolor temppipecolor)   (setq TITLE      (strcat "TEMPORARY WATER LINE DN" rdia)))  
                     (if (= pipecolor existpipecolor) (setq TITLE      (strcat "EXISTING WATER LINE DN" rdia)))
                    ) (progn (setq pipecolor relocpipecolor) (setq TITLE      (strcat "RELOCATED WATER LINE DN" rdia))))
)
 
 (setq i 0)
 (setq MAXGE (car ptsg))
 (while (<= i (- (length ptsg) 1))
 (if (>= (nth i ptsg) MAXGE) (setq MAXGE (nth i ptsg)))
 (setq i (+ i 1))
 )
 (setq i 0)
  (while (<= i (- (length ptsi) 1))
 (if (/= (nth i ptsi) nil) 
 (if (>= (nth i ptsi) MAXGE) (setq MAXGE (nth i ptsi))))
 (setq i (+ i 1))
 )
 ;(princ MAXGE)
 ;(princ (length ptsg))
 
 (setq i 0)
 (setq YBASE (car ptsi))
 (while (<= i (- (length ptsi) 1))
 (if (/= (nth i ptsi) nil) 
 (if (<= (nth i ptsi) YBASE) (setq YBASE (nth i ptsi))))
 (setq i (+ i 1))
 )
 (setq i 0)
  (while (<= i (- (length ptsg) 1))
 (if (/= (nth i ptsg) nil) 
 (if (<= (nth i ptsg) YBASE) (setq YBASE (nth i ptsg))))
 (setq i (+ i 1))
 )
 ;(princ YBASE)
 (if (and (/= MODE "S") (/= MODE "I")) (progn
  (setq i 0)
  (setq pdt nil)
  (setq CD 0)
  (setq cdlistt nil)
  (while (<= i (- n 4))
  (setq xp1 (nth i pipecoords))
  (setq yp1 (nth (+ i 1) pipecoords))
  (setq xp2 (nth (+ i 2) pipecoords))
  (setq yp2 (nth (+ i 3) pipecoords))
  (setq p1 (list xp1 yp1))
  (setq p2 (list xp2 yp2))
  (setq d (distance p1 p2 ))
  (setq d1 (list d))
  (setq pdt (append pdt d1))
  (setq CD (+ CD d))
  (setq CD1 (list CD))
  (setq cdlistt (append cdlistt CD1))
  (setq i (+ i 2))
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;chainage on profile
   (if (= allowchainageprofile 1) (progn
   (setq pdt nil)
   ;(princ cdlist)
   (setq i 0)
   (while (< i  (- (length cdlist) 1))
   (setq pddt (- (nth (+ i 1) cdlist) (nth i cdlist) ))
   (setq pddt (list pddt))
   (setq pdt (append pdt pddt))
   (setq i (+ i 1))
   )
   ;(setq cdlist (RemoveNth 0 cdlist))
   )
   (progn
    (setq cdlist cdlistt)
	(setq cdlist (cons '0 cdlist))
	   )
   )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;chainage on profile
  )
  (setq cdlist (cons '0 cdlist))
  )
   
 ;(princ pdt)
 ;(princ CD)
 ;(princ cdlist)
 ;(princ)
 
 ;finds the one slope using start and end invert levels and append the invert levels accordingly
 (setq nflag 0)
 (setq ptsii nil)
 (setq i 1)
  (while (<= i (- (length ptsi) 2))
  (if (/= (nth i ptsi) nil) (setq  nflag 1))
  (setq i (+ i 1))
   )
 (if (= nflag 0) (progn
 (setq idiff (- (nth (- (length ptsi) 1) ptsi) (car ptsi)))
 ;(princ idiff)
 (setq dslp (/ idiff CD))
 (setq ptii (car ptsi))
 (setq ptii (list ptii))
 (setq ptsii (append ptsii ptii) )
 ;(princ ptsii)
 (setq i 1)
 (setq cdlist (RemoveNth 0 cdlist))
 (while (<= i (- (length cdlist) 1))
  (setq ptii (+ (car ptii) (* dslp (nth (- i 1) pdt))))
  (setq ptii (list ptii))
  (setq ptsii (append ptsii ptii) )
  (setq i (+ i 1))
  )
  ;(princ ptsii)
 (setq ptii (nth (- (length ptsi) 1) ptsi))
 (setq ptii (list ptii))
 (setq ptsii (append ptsii ptii) )
 (setq ptsi ptsii)
)
(progn;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (and (/= MODE "S") (/= MODE "I")) (progn
(setq ptsii nil)

(if (= MODE "C") (setq icdlistt  (cons '0 cdlist_for_mcm)) (setq icdlistt (cons '0 gcdlist)))
  (setq ptsi  (CALC_IL_SKIP ptsi icdlistt))
  (setq k 0)
  (setq i 0)
  (while (<= k (length cdlist)) 
  (setq i 0)
  (setq distinv (nth k cdlist))
  ;(princ cdlist)
  ;(terpri)
  ;(princ distinv)
  ;(terpri)
 	    (while (<= i (- (length ptsi) 1))
		;(princ (nth i ptsi) ) (princ (nth (+ i 1) ptsi))
		  (if (and (<= (nth i icdlistt)  distinv) (>= (nth (+ i 1) icdlistt) distinv)) 
		    (PROGN 
			  (setq xp1 (nth i icdlistt))
              (setq yp1 (nth i ptsi))
              (setq xp2 (nth (+ i 1) icdlistt))
              (setq yp2 (nth (+ i 1) ptsi))
              (setq slp (/ (- yp2 yp1) (- xp2 xp1) ))
              (setq b (- yp1 (* slp xp1) ))
              (setq invertlevel (+ (* slp  distinv) b))
			 ; (terpri)
              ;(princ invertlevel)
			  (setq invertlevel (list invertlevel))
              (setq ptsii (append ptsii invertlevel))
			  (setq i (length ptsi))
              ;(princ ptsii)			  
		     )
		  )		  
			  (setq i (+ i 1))
	     )
	(setq k (+ k 1))	 
  )
  ;(terpri)
  ;(princ ptsii)
  ;(terpri)
  (setq ptsi ptsii)
  (setq icdlistt (RemoveNth 0 icdlistt))
  (setq cdlist (RemoveNth 0 cdlist))
 ;(setq cdlist (RemoveNth 0 cdlist))
 )
 (setq cdlist (RemoveNth 0 cdlist))
 )
 )
)  
 
 
 
 
 (if (/= MODE "I") (progn 
  (setq i 0)
  (setq k 0)
  (setq slpl nil)
  (while (<= i (- (length ptsi) 2))
  (setq ilv1 (nth i ptsi))
  (setq ilv2 (nth (+ i 1) ptsi))
  (setq dist (nth k pdt))
  (setq slp (/ (- ilv2 ilv1) dist))
  (setq slp (list slp))
  (setq slpl (append slpl slp))
  (setq i (+ i 1))
  (setq k (+ k 1))
  )
  ))
  ;(princ slpl)
 
 
 
  
   

 (BRDR2)
 ;(command "circle" BASE 5)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq i 0)
 (setq CD 0)
 (setq cdlistft nil)
 (while (<= i (- (length pdt) 1))
 (setq CD (+ CD (Roundto(nth i pdt) 1)))
 (setq CD1 CD)
 (setq CD1 (list CD1))
 (setq cdlistft (append cdlistft CD1))
 (setq i (+ i 1))
 )
;(setq cdlistft (mapcar  '(lambda (x)            (Roundto x 1)           )           cdlist ))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(TEXTT)

 (draw_grade_pipe)
  (if (and (= MODE "S") (= copycros 1))(getpcr) )
 ;(princ slpl)
 
 
 (SETVAR "CMDECHO"   0)
 (SETVAR "PICKBOX"   4)
 (SETVAR "OSMODE" 16383)
 (COMMAND "LAYER" "SET" "0" "")
   
 (princ)
 ;-------------------------------------------------------------------------------
 ;(princ exportdata)
 (if (= exportdata 1) (setq option "E")(setq option "Q") )
 (if (/= MODE "I") (progn
; (setq Z14 0)
; (setq option "UU")
; (while (< Z14 1)
; (setq option (getstring "\nTo Export the Profile Data Enter (E) or Enter (Q) to Quit: " ))
; (if (or (= option "e") (= option "E") (= option "q") (= option "Q")) (setq Z14 5))
; )
; (if (= option "e") (setq option "E"))
; (if (= option "q") (setq option "Q"))


(if (= option "E") (export-data)) 
))
 
(princ)
  (timeend)
 )
 )
  
  (command "._UNDO" "_End")
  (recvar)
 (princ)

 )
 ; (prompt "\nWP (Pre-Release VER. 1.0 Date: 05-1-2017)" )
 ; (prompt "\nLisp to create Water profiles." )
 ; (prompt "\nDeveloped by Ibrahim El Shar." )
 ; (prompt "\n> Type WP to start." )
 (princ "\nWP (Pre-Release VER. 1.0 Date: 05-02-2017) \nAutoLisp to create Pressurized Lines Profiles \n\nDesigned and Created by Ibrahim El Shar \nContact: ibrahim.elshar@gmail.com \n\nCopyright \U+00A9 2017. All rights reserved. \nType WP to start. ")
