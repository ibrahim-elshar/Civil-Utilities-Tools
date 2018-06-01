;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Hydraulic Losses calculator by IJE                                                                          ;
;
;                                                                                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(setq kb_tot 0)
(setq nhb90 0) ;number of 90 elbows
(setq nhb45 0) ;number of 45 elbows
(setq nhb22 0) ;number of 22.5 elbows
(setq nhb11 0) ;number of 11.25 elbows
(setq nhbnsd 0) ;number of non-standard elbows
(setq bend_angles nil)
;(setq angtol 5) ;angle deviation tolerance
(while (<= i (- (length ang_list) 1))
(cond
((and (>= (atof (nth i ang_list)) 0) (<= (atof (nth i ang_list)) angtol)) (setq bend_angles (append bend_angles (list  nil))))
((and (>= (atof (nth i ang_list)) (- 11.25 angtol)) (<= (atof (nth i ang_list)) (+ 11.25 angtol))) (progn (setq bend_angles (append bend_angles (list "11.25%%dH.BEND")))   (setq nhb11 (+ nhb11 1))   (setq kb_tot (+ kb_tot k11de)) ))
((and (>= (atof (nth i ang_list)) (- 22.5 angtol)) (<= (atof (nth i ang_list)) (+ 22.5 angtol))) (progn (setq bend_angles (append bend_angles (list "22.5%%dH.BEND"))) (setq nhb22 (+ nhb22 1)) (setq kb_tot (+ kb_tot k22de)) ))
((and (>= (atof (nth i ang_list)) (- 45 angtol)) (<= (atof (nth i ang_list)) (+ 45 angtol))) (progn(setq bend_angles (append bend_angles (list "45%%dH.BEND"))) (setq nhb45 (+ nhb45 1)) (setq kb_tot (+ kb_tot k45de)) ))
((and (>= (atof (nth i ang_list)) (- 90 angtol)) (<= (atof (nth i ang_list)) (+ 90 angtol))) (progn(setq bend_angles (append bend_angles (list "90%%dH.BEND"))) (setq nhb90 (+ nhb90 1)) (setq kb_tot (+ kb_tot k90de)) ))
(t  (progn (setq bend_angles (append bend_angles (list "NON-STANDARD-BEND")))  (setq nhbnsd (+ nhbnsd 1))))
)
(setq i (+ i 1))
)
(princ)
;(setq bend_angles (cons nil bend_angles))
;(setq bend_angles (append bend_angles (list nil)))
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
;; Returns a list with the entered element number removed.
(defun RemoveNth ( n l )
    (if (and l (< 0 n))
        (cons (car l) (RemoveNth (1- n) (cdr l)))
        (cdr l)
    )
)
; =======================================================================================================
(defun lst_s_d (lst)
(setq i 0)
 (setq pairll nil)
 (while (<= i (- (length lst) 2))
 (setq pairl (list (nth i lst) (nth (+ i 1) lst)))
 (setq pairll (append pairll (list pairl)))
 (setq i (+ i 2))
 )
 (setq pairl1 pairll)
 )
; =======================================================================================================
(defun slpi (PT1 PT2 / DY DX SLOPE )
(SETQ DY (abs (float (- (CADR PT1) (CADR PT2)))))
(SETQ DX (abs (float (- (CAR PT1) (CAR PT2)))))
(SETQ SLOPE (/ (/ DY VF) (/ DX HF)) )
)
; =======================================================================================================
(defun find_v_angles ( / pipecoords i slps pangles slp ang fng )
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
(getha)
 (setq pipecoords
 (vlax-safearray->list
 (vlax-variant-value
 (vla-get-coordinates ha-object))))
 (setq pipecoords (lst_s_d pipecoords))
 (setq i 0)
 (setq slps nil)
 (setq pangles nil)
  (setq fangles nil)
  (setq vangles nil)
 (while ( <= i (- (length pipecoords) 2))
 (setq slp (slpi (nth i pipecoords) (nth (+ i 1) pipecoords)))
 (setq ang (atan slp))
 (setq slp (list slp))
 (setq slps (append slps slp))
 (setq ang (/ (* ang 180) Pi))
 (setq ang (list ang))
 (setq pangles (append pangles ang)) 
 (setq i (+ i 1) )
 )
 (setq i 0)
  (while (<=  i (- (length pangles) 2))
(setq fng (- (nth (+ i 1) pangles) (nth i pangles)))
 (setq fng (list (abs fng)))
 (setq vangles (append vangles fng)) 
 (setq i (+ i 1) )
  )
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq i 0)
(setq kvb_tot 0)
(setq nvb90 0) ;number of 90 v elbows
(setq nvb45 0) ;number of 45 v elbows
(setq nvb22 0) ;number of 22.5 v elbows
(setq nvb11 0) ;number of 11.25 v elbows
(setq nvbnsd 0) ;number of v non-standard elbows
(setq vbend_angles nil)
(while (<= i (- (length vangles) 1)) 
(cond
((and (>= (nth i vangles) 0) (<= (nth i vangles) angtol)) (setq vbend_angles (append vbend_angles (list  nil))))
((and (>=  (nth i vangles) (- 11.25 angtol)) (<=  (nth i vangles) (+ 11.25 angtol))) (progn (setq vbend_angles (append vbend_angles (list "11.25%%dV.BEND")))   (setq nvb11 (+ nvb11 1))   (setq kvb_tot (+ kvb_tot k11de)) ))
((and (>=  (nth i vangles) (- 22.5 angtol)) (<=  (nth i vangles) (+ 22.5 angtol))) (progn (setq vbend_angles (append vbend_angles (list "22.5%%dV.BEND"))) (setq nvb22 (+ nvb22 1)) (setq kvb_tot (+ kvb_tot k22de)) ))
((and (>=  (nth i vangles) (- 45 angtol)) (<=  (nth i vangles) (+ 45 angtol))) (progn(setq vbend_angles (append vbend_angles (list "45%%dV.BEND"))) (setq nvb45 (+ nvb45 1)) (setq kvb_tot (+ kvb_tot k45de)) ))
((and (>=  (nth i vangles) (- 90 angtol)) (<=  (nth i vangles) (+ 90 angtol))) (progn(setq vbend_angles (append vbend_angles (list "90%%dV.BEND"))) (setq nvb90 (+ nvb90 1)) (setq kvb_tot (+ kvb_tot k90de)) )) 
(t  (progn (setq vbend_angles (append vbend_angles (list "NON-STANDARD-BEND")))  (setq nvbnsd (+ nvbnsd 1)) ))
)
(setq i (+ i 1))
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
; =======================================================================================================
; =======================================================================================================
; =======================================================================================================
; =======================================================================================================
; =======================================================================================================
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
  ;(alert  "HR (VER. 1.0 Date: 10-July-2017) \nAutoLisp to calculate the hydraulic losses \n\nDesigned and Created by Ibrahim El Sharr \nContact: ibrahim.elshar@gmail.com \n\nCopyright © 2017. All rights reserved.")
  (LM:popup "About" "HR (VER. 1.0 Date: 10-July-2017) \nAutoLisp to calculate the hydraulic losses \n\nDesigned and Created by Ibrahim El Sharr \nContact: ibrahim.elshar@gmail.com \n\nCopyright © 2017. All rights reserved." (+ 0 ))

 )
 ; ======================================================================================================= 
 (defun saveVars()
    
	(setq diameter (distof(get_tile "diameter")))
	(setq flow_cumd (distof(get_tile "flow_cumd")))
   	(setq nbfv (atoi(get_tile "nbfv")))
	(setq ngv (atoi(get_tile "ngv")))
	(setq nglv (atoi(get_tile "nglv")))
	(setq nav (atoi(get_tile "nav")))
	(setq ntr (atoi(get_tile "ntr")))
	(setq ntb (atoi(get_tile "ntb")))
	(setq angtol (distof(get_tile "angtol")))
	(setq adkcoeff (distof(get_tile "adkcoeff")))

  )
  ; ======================================================================================================= 
 (defun saveKCcoeff()
    
	(setq k90de (distof(get_tile "k90de")))
	(setq k45de (distof(get_tile "k45de")))
	(setq k22de (distof(get_tile "k22de")))
	(setq k11de (distof(get_tile "k11de")))
	(setq kbfv (distof(get_tile "kbfv")))
	(setq kgv (distof(get_tile "kgv")))
	(setq kglv (distof(get_tile "kglv")))
	(setq kav (distof(get_tile "kav")))
	(setq ktr (distof(get_tile "ktr")))
	(setq ktb (distof(get_tile "ktb")))
	(setq cdin (distof(get_tile "cdin")))
	(setq cdio (distof(get_tile "cdio")))
	(setq cpvc (distof(get_tile "cpvc")))
	(setq chdpe (distof(get_tile "chdpe")))
	(setq ccs (distof(get_tile "ccs")))
	(setq cac (distof(get_tile "cac")))
	(setq cfrp (distof(get_tile "cfrp")))
	(setq cwi (distof(get_tile "cwi")))
	(setq cc (distof(get_tile "cc")))
	(setq cb (distof(get_tile "cb")))
	(setq cci (distof(get_tile "cci")))
	(setq cclp (distof(get_tile "cclp")))
  )
 ; =======================================================================================================
(defun calcu()
(set_tile "pipelength"  (cond ( (/= pl_length nil) (rtos pl_length 2 3) )  (t "0") )  )
(setq k_tot 0)
(setq i 0)
(setq kb_tot 0)
(setq bend_angles nil)
(setq nhb90 0) ;number of 90 elbows
(setq nhb45 0) ;number of 45 elbows
(setq nhb22 0) ;number of 22.5 elbows
(setq nhb11 0) ;number of 11.25 elbows
(setq nhbnsd 0) ;number of non-standard elbows
(if (/= ang_list nil)
(progn
(while (<= i (- (length ang_list) 1))
(cond
((and (>= (atof (nth i ang_list)) 0) (<= (atof (nth i ang_list)) angtol)) (setq bend_angles (append bend_angles (list  nil))))
((and (>= (atof (nth i ang_list)) (- 11.25 angtol)) (<= (atof (nth i ang_list)) (+ 11.25 angtol))) (progn (setq bend_angles (append bend_angles (list "11.25%%dH.BEND")))   (setq nhb11 (+ nhb11 1))   (setq kb_tot (+ kb_tot k11de)) ))
((and (>= (atof (nth i ang_list)) (- 22.5 angtol)) (<= (atof (nth i ang_list)) (+ 22.5 angtol))) (progn (setq bend_angles (append bend_angles (list "22.5%%dH.BEND"))) (setq nhb22 (+ nhb22 1)) (setq kb_tot (+ kb_tot k22de)) ))
((and (>= (atof (nth i ang_list)) (- 45 angtol)) (<= (atof (nth i ang_list)) (+ 45 angtol))) (progn(setq bend_angles (append bend_angles (list "45%%dH.BEND"))) (setq nhb45 (+ nhb45 1)) (setq kb_tot (+ kb_tot k45de)) ))
((and (>= (atof (nth i ang_list)) (- 90 angtol)) (<= (atof (nth i ang_list)) (+ 90 angtol))) (progn(setq bend_angles (append bend_angles (list "90%%dH.BEND"))) (setq nhb90 (+ nhb90 1)) (setq kb_tot (+ kb_tot k90de)) ))
(t  (progn (setq bend_angles (append bend_angles (list "NON-STANDARD-BEND")))  (setq nhbnsd (+ nhbnsd 1))))
)
(setq i (+ i 1))
)

)
(progn
      (set_tile "nb90"  (cond ( (/= nb90 nil) (rtos nb90 2 0) )  (t "0") )  )
	  (set_tile "nb45"  (cond ( (/= nb45 nil) (rtos nb45 2 0) )  (t "0") )  )
	  (set_tile "nb22"  (cond ( (/= nb22 nil) (rtos nb22 2 0) )  (t "0") )  )
	  (set_tile "nb11"  (cond ( (/= nb11 nil) (rtos nb11 2 0) )  (t "0") )  )
	  (set_tile "nbnsd"  (cond ( (/= nbnsd nil) (rtos nbnsd 2 0) )  (t "0") )  )

(setq kb_tot (+  (* nb90 k90de) (* nb45 k45de) (* nb22 k22de) (* nb11 k11de)))
)
)
(setq i 0)
(setq kvb_tot 0)
(setq vbend_angles nil)
(setq nvb90 0) ;number of 90 v elbows
(setq nvb45 0) ;number of 45 v elbows
(setq nvb22 0) ;number of 22.5 v elbows
(setq nvb11 0) ;number of 11.25 v elbows
(setq nvbnsd 0) ;number of v non-standard elbows
(if (/= vangles nil)
(progn
(while (<= i (- (length vangles) 1))
(cond
((and (>= (nth i vangles) 0) (<= (nth i vangles) angtol)) (setq vbend_angles (append vbend_angles (list  nil))))
((and (>=  (nth i vangles) (- 11.25 angtol)) (<=  (nth i vangles) (+ 11.25 angtol))) (progn (setq vbend_angles (append vbend_angles (list "11.25%%dV.BEND")))   (setq nvb11 (+ nvb11 1))   (setq kvb_tot (+ kvb_tot k11de)) ))
((and (>=  (nth i vangles) (- 22.5 angtol)) (<=  (nth i vangles) (+ 22.5 angtol))) (progn (setq vbend_angles (append vbend_angles (list "22.5%%dV.BEND"))) (setq nvb22 (+ nvb22 1)) (setq kvb_tot (+ kvb_tot k22de)) ))
((and (>=  (nth i vangles) (- 45 angtol)) (<=  (nth i vangles) (+ 45 angtol))) (progn(setq vbend_angles (append vbend_angles (list "45%%dV.BEND"))) (setq nvb45 (+ nvb45 1)) (setq kvb_tot (+ kvb_tot k45de)) ))
((and (>=  (nth i vangles) (- 90 angtol)) (<=  (nth i vangles) (+ 90 angtol))) (progn(setq vbend_angles (append vbend_angles (list "90%%dV.BEND"))) (setq nvb90 (+ nvb90 1)) (setq kvb_tot (+ kvb_tot k90de)) )) 
(t  (progn (setq vbend_angles (append vbend_angles (list "NON-STANDARD-BEND")))  (setq nvbnsd (+ nvbnsd 1)) ))
)
(setq i (+ i 1))
)

)
(progn
      (set_tile "nb90"  (cond ( (/= nb90 nil) (rtos nb90 2 0) )  (t "0") )  )
	  (set_tile "nb45"  (cond ( (/= nb45 nil) (rtos nb45 2 0) )  (t "0") )  )
	  (set_tile "nb22"  (cond ( (/= nb22 nil) (rtos nb22 2 0) )  (t "0") )  )
	  (set_tile "nb11"  (cond ( (/= nb11 nil) (rtos nb11 2 0) )  (t "0") )  )
	  (set_tile "nbnsd"  (cond ( (/= nbnsd nil) (rtos nbnsd 2 0) )  (t "0") )  )

(setq kb_tot (+  (* nb90 k90de) (* nb45 k45de) (* nb22 k22de) (* nb11 k11de)))
)
)

(setq k_valv_n_tee  (+  (* nbfv kbfv) (* ngv kgv) (* nglv kglv) (* nav kav) (* ntr ktr) (* ntb ktb)))

 (setq k_tot  (+ kb_tot kvb_tot k_valv_n_tee adkcoeff))
 (set_tile "disp"   (cond ( (/= k_tot nil)(rtos k_tot 2 4)  )  (t "0" )  ))
 (set_tile "flow_cumd"  (cond ( (/= flow_cumd nil) (rtos flow_cumd 2 3) )  (t "1527") )  )
 (set_tile "diameter"  (cond ( (/= diameter nil) (rtos diameter 2 3) )  (t "150") )  )
 
 (set_tile "flow_cumhr"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (/ (distof (get_tile "flow_cumd")) 24) 2 2)  )  (t "0" )  ))
 (set_tile "flow_gpm"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (* (distof (get_tile "flow_cumd")) 0.183715461) 2 2)  )  (t "0" )  ))
 (set_tile "flow_lps"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (* (distof (get_tile "flow_cumd")) 0.011574074) 2 2)  )  (t "0" )  ))
 (set_tile "velocity"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (/  (/ (* (distof (get_tile "flow_cumd")) 0.011574074) 1000) (/ (*  Pi (expt (/ (distof (get_tile "diameter")) 1000) 2)) 4)) 2 2)  )  (t "0" )  ))
   (start_list "material_selections")
       (mapcar 'add_list material_selections)
       (end_list)
       (if (setq pos (vl-position mst-last-sel material_selections))
         (set_tile "material_selections" (itoa pos))
         (set_tile "material_selections" "0")   ; default 
       )
	   (set_tile "ccoeff" 
	   (cond ((= (get_tile "material_selections") "0")(set_tile "ccoeff" (rtos cdin 2 2)))
	         ((= (get_tile "material_selections") "1")(set_tile "ccoeff" (rtos cdio 2 2)))
			 ((= (get_tile "material_selections") "2")(set_tile "ccoeff" (rtos cpvc 2 2)))
			 ((= (get_tile "material_selections") "3")(set_tile "ccoeff" (rtos chdpe 2 2)))
			 ((= (get_tile "material_selections") "4")(set_tile "ccoeff" (rtos ccs 2 2)))
			 ((= (get_tile "material_selections") "5")(set_tile "ccoeff" (rtos cac 2 2)))
			 ((= (get_tile "material_selections") "6")(set_tile "ccoeff" (rtos cfrp 2 2)))
			 ((= (get_tile "material_selections") "7")(set_tile "ccoeff" (rtos cwi 2 2)))
			 ((= (get_tile "material_selections") "8")(set_tile "ccoeff" (rtos cc 2 2)))
			 ((= (get_tile "material_selections") "9")(set_tile "ccoeff" (rtos cb 2 2)))
			 ((= (get_tile "material_selections") "10")(set_tile "ccoeff" (rtos cci 2 2)))
			 ((= (get_tile "material_selections") "11")(set_tile "ccoeff" (rtos cclp 2 2)))
			 
	   ))
		  (setq flow_lps (distof (get_tile "flow_lps")))
		  (setq velocity (distof (get_tile "velocity")))
		  (setq diameter (distof (get_tile "diameter")))
		  
		   (if (=  pl_length 0) (setq pl_length nil))
		  
		  (setq percent_loss (/ (* (expt velocity 1.85) 682.5) (* (expt (distof (get_tile "ccoeff")) 1.85) (expt (/ diameter 1000) 1.167) ) ))
		  (if (/=  pl_length nil)(setq tot_fric_loss (/ (* percent_loss pl_length) 100)) (setq tot_fric_loss 0))
		  (set_tile "tot_fric_loss" (rtos tot_fric_loss 2 3))
		  (setq tot_fitt_loss (/ (* (expt velocity 2) k_tot) (* 2 9.81)))
		  (set_tile "tot_fitt_loss" (rtos tot_fitt_loss 2 3))
		  (setq tot_loss (+ tot_fric_loss tot_fitt_loss))
		  (set_tile "tot_loss" (rtos tot_loss 2 3))
		  (if (/=  pl_length nil)(setq hyd_grad (* (/ tot_loss pl_length) 1000)) (setq hyd_grad 0))
		  (set_tile "hyd_grad" (rtos hyd_grad 2 3))
)
  ; =======================================================================================================
  (defun changeangtol()

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
 ; =======================================================================================================
; =======================================================================================================	
; =======================================================================================================
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
; =======================================================================================================
(defun reset_entries ()
(setq list_flow_cumd nil)
(setq list_pl_length nil)
(setq list_diameter nil)
(setq list_ccoeff nil)
(setq list_k_tot nil)
(setq no_of_entries 0)
(set_tile "nofsaves" "0")
;(set_tile "line_name" "Line 1")
(setq list_user_line_name nil)
)
; =======================================================================================================
(defun save_entry ()
(setq pl_length (distof (get_tile "pipelength")))
(setq ccoeff (distof (get_tile "ccoeff")))
(setq lflow_cumd (list flow_cumd))
(setq lpl_length (list pl_length))
(setq ldiameter (list diameter))
(setq lccoeff (list ccoeff))
(setq lk_tot (list k_tot))
(setq list_flow_cumd (append list_flow_cumd lflow_cumd))
(setq list_pl_length (append list_pl_length lpl_length))
(setq list_diameter (append list_diameter ldiameter))
(setq list_ccoeff (append list_ccoeff lccoeff))
(setq list_k_tot (append list_k_tot lk_tot))
(if (= no_of_entries nil) (progn (setq no_of_entries 0) (setq no_of_entries (+ no_of_entries 1))) (progn (setq no_of_entries (+ no_of_entries 1))))
(set_tile "nofsaves" (rtos no_of_entries 2 0))
(setq user_line_name (get_tile "line_name"))
(setq list_user_line_name (append list_user_line_name (list user_line_name)))

)
; =======================================================================================================
 (defun makelinen ( i / )
 ;(setq linen (list ((nth (- i 5) strcat list_user_line_name) (rtos (nth (- i 5) list_flow_cumd ) 2 2)  (strcat "=A" (rtos i 2 0) "/24") (strcat "=(B" (rtos i 2 0) "*1000/3.78)/60")  (strcat "=C" (rtos i 2 0) "*3.78/60") (rtos (nth (- i 5) list_pl_length ) 2 2) (rtos (nth (- i 5) list_diameter ) 2)  (strcat "=(D" (rtos i 2 0) "/1000)/(3.1416*(F" (rtos i 2 0) "/1000)^2/4)") (strcat "=(682.5*G" (rtos i 2 0) "^1.85)/((I" (rtos i 2 0) "^1.85)*(F" (rtos i 2 0) "/1000)^1.167)") (rtos (nth (- i 5) list_ccoeff) 2 2) (strcat "=E"(rtos i 2 0)"*H"(rtos i 2 0)"/100") (rtos (nth (- i 5) list_k_tot ) 2 4) (strcat "=K" (rtos i 2 0) "*G" (rtos i 2 0) "^2/(2*9.81)") (strcat "=J" (rtos i 2 0) "+L" (rtos i 2 0)) (strcat "=M" (rtos i 2 0) "/E" (rtos i 2 0) "*1000")))
 (setq linen (list  (strcat (nth (- i 5) list_user_line_name)) (rtos (nth (- i 5) list_flow_cumd ) 2 2)  (strcat "=B" (rtos i 2 0) "/24")
 (strcat "=(C" (rtos i 2 0) "*1000/3.78)/60")  (strcat "=D" (rtos i 2 0) "*3.78/60") (rtos (nth (- i 5) list_pl_length ) 2 2)
 (rtos (nth (- i 5) list_diameter ) 2)  (strcat "=(E" (rtos i 2 0) "/1000)/(3.1416*(G" (rtos i 2 0) "/1000)^2/4)")
 (strcat "=(682.5*H" (rtos i 2 0) "^1.85)/((J" (rtos i 2 0) "^1.85)*(G" (rtos i 2 0) "/1000)^1.167)") 
 (rtos (nth (- i 5) list_ccoeff) 2 2) (strcat "=F"(rtos i 2 0)"*I"(rtos i 2 0)"/100") (rtos (nth (- i 5) list_k_tot ) 2 4)
 (strcat "=L" (rtos i 2 0) "*H" (rtos i 2 0) "^2/(2*9.81)") (strcat "=K" (rtos i 2 0) "+M" (rtos i 2 0)) (strcat "=N" (rtos i 2 0) "/F" (rtos i 2 0) "*1000")))
 )
 ; =======================================================================================================
(defun HR-export-data ( / emsg )
(setq line1  (list "Hydraulic Losses Calculations"))
(setq line2  (list "Created by HLC v1.0"))
(setq line3  (list "Date:" (menucmd "M=$(edtime,$(getvar,date),DDDD\",\" D MONTH YYYY)")))
(setq line4  (list "Line" "cu. m/day" "cu. m/hr"  "gpm" "l/s"  "length (m)" "dia (mm)" "velocity (m/s)" "loss (%)" "Coeff c" "total fric loss (m)" "fitting Coeff K" "fitting loss (m)" "Total loss (m)" "Hyd Gradient ‰" ))

; (setq i 2)
; ;(princ i)
; (while (< i (+(length ptsi) 1))
; (setq ilet (Number2Alpha i))
; (setq ilett (list (strcat "=" ilet "5+(" ilet "6/100)*" ilet "7")))
; (setq ptsixls (append ptsixls  ilett))
; (setq i (+ i 1))
; )

	 (setq fname
       (getfiled "Save as"
          (strcat (getvar 'dwgprefix) (vl-filename-base (getvar 'dwgname)) )
           "xls"
          1
       )
   )
  
   (if (/= fname nil) (progn
   (setq emsg (strcat "\nSaving... " fname ))  
   (princ emsg)
   (OpenExcel nil nil nil)
   (PutCell "A1" line1)
   (PutCell "A2" line2)
   (PutCell "A3" line3)
   (PutCell "A4" line4)
   (setq j 1)
   (setq i 5)
  (while (<= j no_of_entries)
   (PutCell (strcat "A" (rtos i 2 0) ) (makelinen i))
   (repeat (length ptsgxls)(PutCell "A4" ptsgxls));Repeat as required
   (setq j (+ j 1))
   (setq i (+ i 1))
   )
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
; =======================================================================================================
(defun nest1 ()
;define the function
 
  (setq dcl_id1 (load_dialog "HLD.dcl"))
  ;load the DCL file
 
  (if (not (new_dialog "nest1" dcl_id1))
  ;load the nested dialogue box
 
    (exit)
    ;if not loaded exit
 
  )
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      Load the defaults     ;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (progn 
                  
		  (set_tile "k90de"  (cond ( (/= k90de nil) (rtos k90de 2 3) )  (t (rtos dk90de 2 3) )  ))	
		  (set_tile "k45de"  (cond ( (/= k45de nil) (rtos k45de 2 3) )  (t (rtos dk45de 2 3)) )  )	
		  (set_tile "k22de"  (cond ( (/= k22de nil) (rtos k22de 2 3) )  (t (rtos dk22de 2 3)) )  )
		  (set_tile "k11de"  (cond ( (/= k11de nil) (rtos k11de 2 3) )  (t (rtos dk11de 2 3)) )  )
		  (set_tile "kbfv"  (cond ( (/= kbfv nil) (rtos kbfv 2 3) )  (t (rtos dkbfv 2 3)) )  )
		  (set_tile "kgv"  (cond ( (/= kgv nil) (rtos kgv 2 3) )  (t (rtos dkgv 2 3)) )  )
		  (set_tile "kglv"  (cond ( (/= kglv nil) (rtos kglv 2 3) )  (t (rtos dkglv 2 3)) )  )
		  (set_tile "kav"  (cond ( (/= kav nil) (rtos kav 2 3) )  (t (rtos dkav 2 3)) )  )
		  (set_tile "ktr"  (cond ( (/= ktr nil) (rtos ktr 2 3) )  (t (rtos dktr 2 3)) )  )
		  (set_tile "ktb"  (cond ( (/= ktb nil) (rtos ktb 2 3) )  (t (rtos dktb 2 3)) )  )
		  (set_tile "cdin"  (cond ( (/= cdin nil) (rtos cdin 2 3) )  (t (rtos dcdin 2 3)) )  )	
		  (set_tile "cdio"  (cond ( (/= cdio nil) (rtos cdio 2 3) )  (t (rtos dcdio 2 3)) )  )
		  (set_tile "cpvc"  (cond ( (/= cpvc nil) (rtos cpvc 2 3) )  (t (rtos dcpvc 2 3)) )  )	
		  (set_tile "chdpe"  (cond ( (/= chdpe nil) (rtos chdpe 2 3) )  (t (rtos dchdpe 2 3)) )  )
		  (set_tile "ccs"  (cond ( (/= ccs nil) (rtos ccs 2 3) )  (t (rtos dccs 2 3)) )  )
		  (set_tile "cac"  (cond ( (/= cac nil) (rtos cac 2 3) )  (t (rtos dcac 2 3)) )  )
		  (set_tile "cfrp"  (cond ( (/= cfrp nil) (rtos cfrp 2 3) )  (t (rtos dcfrp 2 3)) )  )
		  (set_tile "cwi"  (cond ( (/= cwi nil) (rtos cwi 2 3) )  (t (rtos dcwi 2 3)) )  )
		  (set_tile "cc"  (cond ( (/= cc nil) (rtos cc 2 3) )  (t (rtos dcc 2 3)) )  )
		  (set_tile "cb"  (cond ( (/= cb nil) (rtos cb 2 3) )  (t (rtos dcb 2 3)) )  )
		  (set_tile "cci"  (cond ( (/= cci nil) (rtos cci 2 3) )  (t (rtos dcci 2 3)) )  )
		  (set_tile "cclp"  (cond ( (/= cclp nil) (rtos cclp 2 3) )  (t (rtos dcclp 2 3)) )  )
 	 )

  
   (action_tile  "resetdefaults"  "(resetdefaults)" )
   (action_tile  "setasdefaults"  "(setasdefaults)" )
 
   (action_tile  "cancell"  "(done_dialog)" )
  ;if cancel selected do this
 
  (action_tile  "OK2"  "(saveKCcoeff) (done_dialog 10)" )
  ;if OK selected do this
 
 
  (start_dialog)
  ;start the nested dialogue box
 
  (unload_dialog dcl_id1)
  ;unload the nested dialogue box
 
  (princ)
 
);defun
;========================================================================================================
(defun savegsvelocity()
(setq velocity (distof (get_tile "gsvelocity")))
(setq flow_cumd (/ (* velocity (/ (*  Pi 1000 (expt (/ diameter 1000) 2)) 4)) 0.011574074))
)
;========================================================================================================
(defun savegsvelocity_chang_dia()
(setq velocity (distof (get_tile "gsvelocityvd")))
(setq diameter (expt (/  (* 4 flow_cumd 11.574074) (* velocity  Pi )) 0.5))
)
;========================================================================================================
(defun nest2 ()
;define the function
 
  (setq dcl_id2 (load_dialog "HLD.dcl"))
  ;load the DCL file
 
  (if (not (new_dialog "nest2" dcl_id2))
  ;load the nested dialogue box
 
    (exit)
    ;if not loaded exit
	)
 (set_tile "gsvelocity"  (cond ( (/= velocity nil) (rtos velocity 2 3) )  (t "0") )  )
 
 (action_tile  "cancel2"  " (done_dialog)" )
  ;if cancel selected do this
 
 
  (action_tile  "OK22"  "(savegsvelocity) (done_dialog 5)" )
  ;if OK selected do this
  
 (start_dialog)
  ;start the nested dialogue box
 
  (unload_dialog dcl_id2)
  ;unload the nested dialogue box
 
  (princ)
  )
 ;========================================================================================================
 (defun nest5 ()
;define the function
 
  (setq dcl_id5 (load_dialog "HLD.dcl"))
  ;load the DCL file
 
  (if (not (new_dialog "nest5" dcl_id5))
  ;load the nested dialogue box
 
    (exit)
    ;if not loaded exit
	)
 (set_tile "gsvelocityvd"  (cond ( (/= velocity nil) (rtos velocity 2 3) )  (t "0") )  )
 
 (action_tile  "cancel5"  " (done_dialog)" )
  ;if cancel selected do this
 
 
  (action_tile  "OK5"  "(savegsvelocity_chang_dia) (done_dialog 5)" )
  ;if OK selected do this
  
 (start_dialog)
  ;start the nested dialogue box
 
  (unload_dialog dcl_id5)
  ;unload the nested dialogue box
 
  (princ)
  )
;========================================================================================================
(defun saveovdbends()
(setq nb90 (distof (get_tile "onb90")))
(setq nb45 (distof (get_tile "onb45")))
(setq nb22 (distof (get_tile "onb22")))
(setq nb11 (distof (get_tile "onb11")))
(setq nbnsd 0)
)
;========================================================================================================
(defun obresetdefaults()
(set_tile "onb90"  "0"  )
 (set_tile "onb45"  "0" )
 (set_tile "onb22"   "0" )
 (set_tile "onb11" "0" )
)
;========================================================================================================
;========================================================================================================
(defun nest3 ()
;define the function
 
  (setq dcl_id3 (load_dialog "HLD.dcl"))
  ;load the DCL file
 
  (if (not (new_dialog "nest3" dcl_id3))
  ;load the nested dialogue box
 
    (exit)
    ;if not loaded exit
	)
 (set_tile "onb90"  (cond ( (/= nb90 nil) (rtos nb90 2 0) )  (t "0") )  )
 (set_tile "onb45"  (cond ( (/= nb45 nil) (rtos nb45 2 0) )  (t "0") )  )
 (set_tile "onb22"  (cond ( (/= nb22 nil) (rtos nb22 2 0) )  (t "0") )  )
 (set_tile "onb11"  (cond ( (/= nb11 nil) (rtos nb11 2 0) )  (t "0") )  )

 (setq ang_list nil)
 (setq vangles nil)
  
 (action_tile  "oreset"  "(obresetdefaults) " )
  
 (action_tile  "cancel3"  " (done_dialog)" )
  ;if cancel selected do this
 
 (action_tile  "OK3"  "(saveovdbends) (done_dialog 3)" )
  ;if OK selected do this
  
 (start_dialog)
  ;start the nested dialogue box
 
  (unload_dialog dcl_id3)
  ;unload the nested dialogue box
 
  (princ)
  )
    
;========================================================================================================
(defun setasdefaults()
	(setq k90de (distof(get_tile "k90de")))
	(setq k45de (distof(get_tile "k45de")))
	(setq k22de (distof(get_tile "k22de")))
	(setq k11de (distof(get_tile "k11de")))
	(setq kbfv (distof(get_tile "kbfv")))
	(setq kgv (distof(get_tile "kgv")))
	(setq kglv (distof(get_tile "kglv")))
	(setq kav (distof(get_tile "kav")))
	(setq ktr (distof(get_tile "ktr")))
	(setq ktb (distof(get_tile "ktb")))
	(setq cdin (distof(get_tile "cdin")))
	(setq cdio (distof(get_tile "cdio")))
	(setq cpvc (distof(get_tile "cpvc")))
	(setq chdpe (distof(get_tile "chdpe")))
	(setq ccs (distof(get_tile "ccs")))
	(setq cac (distof(get_tile "cac")))
	(setq cfrp (distof(get_tile "cfrp")))
	(setq cwi (distof(get_tile "cwi")))
	(setq cc (distof(get_tile "cc")))
	(setq cb (distof(get_tile "cb")))
	(setq cci (distof(get_tile "cci")))
	(setq cclp (distof(get_tile "cclp")))
	
	(setq file (open (strcat prefx "HLCD.dat") "w"))
(write-line (rtos k90de 2 4)  file)
(write-line (rtos k45de 2 4)  file)
(write-line (rtos k22de 2 4)  file)
(write-line (rtos k11de 2 4)  file)
(write-line (rtos kbfv 2 4)  file)
(write-line (rtos kgv 2 4)  file)
(write-line (rtos kglv 2 4)  file)
(write-line (rtos kav 2 4)  file)
(write-line (rtos ktr 2 4)  file)
(write-line (rtos ktb 2 4)  file)
(write-line (rtos cdin 2 4)  file)
(write-line (rtos cdio 2 4)  file)
(write-line (rtos cpvc 2 4)  file)
(write-line (rtos chdpe 2 4)  file)
(write-line (rtos ccs 2 4)  file)
(write-line (rtos cac 2 4)  file)
(write-line (rtos cfrp 2 4)  file)
(write-line (rtos cwi 2 4)  file)
(write-line (rtos cc 2 4)  file)
(write-line (rtos cb 2 4)  file)
(write-line (rtos cci 2 4)  file)
(write-line (rtos cclp 2 4)  file)


(close file)
 )
;===================================================================================================
(defun resetdefaults()
	      (setq  dk90de 0.9)
          (setq  dk45de 0.45)
          (setq  dk22de 0.22)
          (setq  dk11de 0.11)
          (setq  dkbfv 1.2)
          (setq  dkgv 0.39)  
          (setq  dkglv 10)
          (setq  dkav 4.3)
          (setq  dktr 0.6)
          (setq  dktb 1.8)
		  (setq dcdin 140) ; c coefficient of ductile iron new pipe
		  (setq dcdio 120) ; c coefficient of ductile iron old pipe
		  (setq dcpvc 150) ; c coefficient of pvc
		  (setq dchdpe 150) ; c coefficient of hdpe
		  (setq dccs 140) ; c coefficient of carbon steel pipes
		  (setq dcac 140) ; c coefficient of asbestos cement pipes
		  (setq dcfrp 150) ; c coefficient of frp pipes
		  (setq dcwi 100) ; c coefficient of wrought iron pipes
		  (setq dcc 130) ; c coefficient of copper pipes
		  (setq dcb 130) ; c coefficient of brass pipes
		  (setq dcci 130) ; c coefficient of cast-iron pipes
		  (setq dcclp 140) ; c coefficient of cement lined pipes
		  
		  (setq  k90de dk90de)
          (setq  k45de dk45de)
          (setq  k22de dk22de)
          (setq  k11de dk11de)
          (setq  kbfv dkbfv)
          (setq  kgv dkgv)  
          (setq  kglv dkglv)
          (setq  kav dkav)
          (setq  ktr dktr)
          (setq  ktb dktb)
		  (setq cdin dcdin) ; c coefficient of ductile iron new pipe
		  (setq cdio dcdio) ; c coefficient of ductile iron old pipe
          (setq cpvc dcpvc) ; c coefficient of pvc
          (setq chdpe dchdpe) ; c coefficient of hdpe
          (setq ccs dccs) ; c coefficient of carbon steel pipes
		  (setq cac dcac) ; c coefficient of asbestos cement pipes
		  (setq cfrp  dcfrp) ; c coefficient of frp pipes
		  (setq  cwi dcwi) ; c coefficient of wrought iron pipes
		  (setq  cc dcc) ; c coefficient of copper pipes
		  (setq  cb dcb) ; c coefficient of brass pipes
		  (setq  cci dcci) ; c coefficient of cast-iron pipes
		  (setq  cclp dcclp) ; c coefficient of cement lined pipes
		  
		  (set_tile "k90de"   (rtos dk90de 2 3 ) )
		  (set_tile "k45de"   (rtos dk45de 2 3 ) ) 	
		  (set_tile "k22de"   (rtos dk22de 2 3 ) )
		  (set_tile "k11de"   (rtos dk11de 2 3 ) )
		  (set_tile "kbfv"    (rtos dkbfv  2 3 ) )
		  (set_tile "kgv"     (rtos dkgv   2 3 ) )
		  (set_tile "kglv"    (rtos dkglv  2 3 ) )
		  (set_tile "kav"     (rtos dkav   2 3 ) )
		  (set_tile "ktr"     (rtos dktr   2 3 ) )
		  (set_tile "ktb"     (rtos dktb   2 3 ) )
          (set_tile "cdin"    (rtos dcdin   2 3 ) )
		  (set_tile "cdio"    (rtos dcdio   2 3 ) )
		  (set_tile "cpvc"    (rtos dcpvc  2 3 ) )
		  (set_tile "chdpe"   (rtos dchdpe 2 3 ) )
		  (set_tile "ccs"     (rtos dccs   2 3 ) )		  
          (set_tile "cac"     (rtos dcac   2 3 ) )
		  (set_tile "cfrp"     (rtos dcfrp   2 3 ) )
		  (set_tile "cwi"     (rtos dcwi   2 3 ) )
		  (set_tile "cc"     (rtos dcc   2 3 ) )
		  (set_tile "cb"     (rtos dcb   2 3 ) )
		  (set_tile "cci"     (rtos dcci   2 3 ) )
		  (set_tile "cclp"     (rtos dcclp   2 3 ) )
		  
 )
; =======================================================================================================
(defun change_material()
 (set_tile "ccoeff" 
	 (cond ((= (get_tile "material_selections") "0")(set_tile "ccoeff" (rtos cdin 2 2)))
	         ((= (get_tile "material_selections") "1")(set_tile "ccoeff" (rtos cdio 2 2)))
			 ((= (get_tile "material_selections") "2")(set_tile "ccoeff" (rtos cpvc 2 2)))
			 ((= (get_tile "material_selections") "3")(set_tile "ccoeff" (rtos chdpe 2 2)))
			 ((= (get_tile "material_selections") "4")(set_tile "ccoeff" (rtos ccs 2 2)))
			 ((= (get_tile "material_selections") "5")(set_tile "ccoeff" (rtos cac 2 2)))
			 ((= (get_tile "material_selections") "6")(set_tile "ccoeff" (rtos cfrp 2 2)))
			 ((= (get_tile "material_selections") "7")(set_tile "ccoeff" (rtos cwi 2 2)))
			 ((= (get_tile "material_selections") "8")(set_tile "ccoeff" (rtos cc 2 2)))
			 ((= (get_tile "material_selections") "9")(set_tile "ccoeff" (rtos cb 2 2)))
			 ((= (get_tile "material_selections") "10")(set_tile "ccoeff" (rtos cci 2 2)))
			 ((= (get_tile "material_selections") "11")(set_tile "ccoeff" (rtos cclp 2 2)))
	   ))
 )
 ;========================================================================================================
(defun saveplo()
(setq pl_length (distof (get_tile "plo")))

)
 ; =======================================================================================================
 (defun nest4 ()
;define the function
 
  (setq dcl_id4 (load_dialog "HLD.dcl"))
  ;load the DCL file
 
  (if (not (new_dialog "nest4" dcl_id4))
  ;load the nested dialogue box
 
    (exit)
    ;if not loaded exit
	)
 (set_tile "plo"  (cond ( (/= pl_length nil) (rtos pl_length 2 3) )  (t "0") )  )
 
 (action_tile  "cancel4"  " (done_dialog)" )
  ;if cancel selected do this
 
 
  (action_tile  "OK4"  "(saveplo) (done_dialog 5)" )
  ;if OK selected do this
  
 (start_dialog)
  ;start the nested dialogue box
 
  (unload_dialog dcl_id4)
  ;unload the nested dialogue box
 
  (princ)
  )
; =======================================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HLC Main Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:hlc () 
 (vl-load-com)
;;;--- Main application
(inivar)  ;save initial setvars
  ;;;--- Get the autoCAD Version
  (setq veri (atoi(substr (getvar "acadver") 1 2)))
  (if(> veri 15)
     (setq prefx (strcat (getvar "mydocumentsprefix") "\\")) 
     (setq prefx (getvar "dwgprefix"))
  )
 ; (setq list_flow_cumd nil)
; (setq list_pl_length nil)
; (setq list_diameter nil)
; (setq list_ccoeff nil)
; (setq list_k_tot nil)
; (setq no_of_entries 0)


(setq ang_list nil)
(setq vangles nil)
(setq k_tot  0)
(setq kb_tot  0)
(setq kvb_tot 0)
(setq adkcoeff 0)
(setq pl_length nil)
(setq material_selections '("Ductile Iron (new pipe)" "Ductile Iron (old pipe)" "PVC" "HDPE" "Carbon Steel" "Asbestos Cement" "FRP" "Wrought Iron" "Copper" "Brass" "Cast-Iron" "Cement Lined Pipes" ))

(setq nbfv 0)
(setq ngv 0)
(setq nglv 0)
(setq nav 0)
(setq ntr 0)
(setq ntb 0)

(setq nvb90 0) ;number of 90 v elbows
(setq nvb45 0) ;number of 45 v elbows
(setq nvb22 0) ;number of 22.5 v elbows
(setq nvb11 0) ;number of 11.25 v elbows
(setq nvbnsd 0) ;number of v non-standard elbows

(setq nhb90 0) ;number of 90 h elbows
(setq nhb45 0) ;number of 45 h elbows
(setq nhb22 0) ;number of 22.5 h elbows
(setq nhb11 0) ;number of 11.25 h elbows
(setq nhbnsd 0) ;number of h non-standard elbows

(setq nb90 (+ nhb90 nvb90)) ;number of 90 h+v elbows
(setq nb45 (+ nhb45 nvb45)) ;number of 45 h+v elbows
(setq nb22 (+ nhb22 nvb22)) ;number of 22.5 h+v elbows
(setq nb11 (+ nhb11 nvb11)) ;number of 11.25 h+v elbows
(setq nbnsd (+ nhbnsd nvbnsd)) ;number of h+v non-standard elbows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if(findfile (strcat prefx "HLCD.dat"))
    (progn
      (if(setq fil(open (strcat prefx "HLCD.dat") "r"))
        (progn
		  (setq dk90de (distof (read-line fil)) )
		  (setq dk45de (distof (read-line fil)) )
		  (setq dk22de (distof (read-line fil)) )
		  (setq dk11de (distof (read-line fil)) )
		  (setq dkbfv (distof (read-line fil)) )
		  (setq dkgv (distof (read-line fil)) )
		  (setq dkglv (distof (read-line fil)) )
	      (setq dkav (distof (read-line fil)) )
	      (setq dktr (distof (read-line fil)) )
		  (setq dktb (distof (read-line fil)) )
		  (setq dcdin (distof (read-line fil))) ; c coefficient of ductile iron new
		  (setq dcdio (distof (read-line fil))) ; c coefficient of ductile iron old
		  (setq dcpvc (distof (read-line fil))) ; c coefficient of pvc
		  (setq dchdpe (distof (read-line fil))) ; c coefficient of hdpe
		  (setq dccs (distof (read-line fil))) ; c coefficient of carbon steel pipes
		  (setq dcac (distof (read-line fil))) ; c coefficient of asbestos cement pipes
		  (setq dcfrp (distof (read-line fil))) ; c coefficient of frp pipes
		  (setq dcwi (distof (read-line fil))) ; c coefficient of wrought iron pipes
		  (setq dcc (distof (read-line fil))) ; c coefficient of copper pipes
		  (setq dcb (distof (read-line fil))) ; c coefficient of brass pipes
		  (setq dcci (distof (read-line fil))) ; c coefficient of cast-iron pipes
		  (setq dcclp (distof (read-line fil))) ; c coefficient of cement lined pipes
          (close fil)
        )
      )
	  )
    
    (progn 
                  
		  (setq  dk90de 0.9)
          (setq  dk45de 0.45)
          (setq  dk22de 0.22)
          (setq  dk11de 0.11)
          (setq  dkbfv 1.2)
          (setq  dkgv 0.39)  
          (setq  dkglv 10)
          (setq  dkav 4.3)
          (setq  dktr 0.6)
          (setq  dktb 1.8)
		  (setq dcdin 140) ; c coefficient of ductile iron new pipe
		  (setq dcdio 120) ; c coefficient of ductile iron old pipe
		  (setq dcpvc 150) ; c coefficient of pvc
		  (setq dchdpe 150) ; c coefficient of hdpe
		  (setq dccs 140) ; c coefficient of carbon steel pipes
		  (setq dcac 140) ; c coefficient of asbestos cement pipes
		  (setq dcfrp 150) ; c coefficient of frp pipes
		  (setq dcwi 100) ; c coefficient of wrought iron pipes
		  (setq dcc 130) ; c coefficient of copper pipes
		  (setq dcb 130) ; c coefficient of brass pipes
		  (setq dcci 130) ; c coefficient of cast-iron pipes
		  (setq dcclp 140) ; c coefficient of cement lined pipes
		  
 	 )
  )

(setq  k90de dk90de)
(setq  k45de dk45de)
(setq  k22de dk22de)
(setq  k11de dk11de)
(setq  kbfv dkbfv)
(setq  kgv dkgv)  
(setq  kglv dkglv)
(setq  kav dkav)
(setq  ktr dktr)
(setq  ktb dktb)
(setq cdin dcdin) ; c coefficient of ductile iron new
(setq cdio dcdio) ; c coefficient of ductile iron old
(setq cpvc dcpvc) ; c coefficient of pvc
(setq chdpe dchdpe) ; c coefficient of hdpe
(setq ccs dccs) ; c coefficient of carbon steel pipes
(setq cac dcac) ; c coefficient of asbestos cement pipes
(setq cfrp  dcfrp) ; c coefficient of frp pipes
(setq  cwi dcwi) ; c coefficient of wrought iron pipes
(setq  cc dcc) ; c coefficient of copper pipes
(setq  cb dcb) ; c coefficient of brass pipes
(setq  cci dcci) ; c coefficient of cast-iron pipes
(setq  cclp dcclp) ; c coefficient of cement lined pipes

 (setq flag 4)
 (while (> flag 2)
  ;;;--- Load the dialog box from file
 (setq dcl_id (load_dialog "HLD.dcl"))
(setq ffgd (findfile "HLD.dcl"))
;(princ ffgd)
  ;;;--- Load the dialog definition from the file
  (if (not (new_dialog "HLD" dcl_id))
    (progn
      (alert "Could not find the HLD.DCL file!")
      (exit)
    )
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      Load the defaults     ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
		  (set_tile "diameter"  (cond ( (/= diameter nil) (rtos diameter 2 3) )  (t "150") )  )
		  (set_tile "adkcoeff"  (cond ( (/= adkcoeff nil) (rtos adkcoeff 2 3) )  (t "0") )  )
		  (set_tile "nbfv"  (cond ( (/= nbfv nil) (rtos nbfv 2 0) )  (t "0") )  )
		  (set_tile "ngv"  (cond ( (/= ngv nil) (rtos ngv 2 0) )  (t "0") )  )
		  (set_tile "nglv"  (cond ( (/= nglv nil) (rtos nglv 2 0) )  (t "0") )  )
		  (set_tile "nav"  (cond ( (/= nav nil) (rtos nav 2 0) )  (t "0") )  )
		  (set_tile "ntr"  (cond ( (/= ntr nil) (rtos ntr 2 0) )  (t "0") )  )
		  (set_tile "ntb"  (cond ( (/= ntb nil) (rtos ntb 2 0) )  (t "0") )  )
		  
		  (set_tile "nofsaves" (cond ( (/= no_of_entries nil) (rtos no_of_entries 2 0) )  (t "0") )  )
		  
		  (setq nb90 (+ nhb90 nvb90)) ;number of 90 h+v elbows
          (setq nb45 (+ nhb45 nvb45)) ;number of 45 h+v elbows
          (setq nb22 (+ nhb22 nvb22)) ;number of 22.5 h+v elbows
          (setq nb11 (+ nhb11 nvb11)) ;number of 11.25 h+v elbows
          (setq nbnsd (+ nhbnsd nvbnsd)) ;number of h+v non-standard elbows
		  
		  (set_tile "nb90"  (cond ( (/= nb90 nil) (rtos nb90 2 0) )  (t "0") )  )
		  (set_tile "nb45"  (cond ( (/= nb45 nil) (rtos nb45 2 0) )  (t "0") )  )
		  (set_tile "nb22"  (cond ( (/= nb22 nil) (rtos nb22 2 0) )  (t "0") )  )
		  (set_tile "nb11"  (cond ( (/= nb11 nil) (rtos nb11 2 0) )  (t "0") )  )
		  (set_tile "nbnsd"  (cond ( (/= nbnsd nil) (rtos nbnsd 2 0) )  (t "0") )  )
		  
		  (set_tile "disp"   (cond ( (/= k_tot nil)(rtos k_tot 2 3)  )  (t "0" )  ))
		  (set_tile "flow_cumd"  (cond ( (/= flow_cumd nil) (rtos flow_cumd 2 3) )  (t "1527") )  )
		  (set_tile "flow_cumhr"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (/ (distof (get_tile "flow_cumd")) 24) 2 2)  )  (t "0" )  ))
		  (set_tile "flow_gpm"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (* (distof (get_tile "flow_cumd")) 0.183715461) 2 2)  )  (t "0" )  ))
		  (set_tile "flow_lps"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (* (distof (get_tile "flow_cumd")) 0.011574074) 2 2)  )  (t "0" )  ))
		  (set_tile "velocity"   (cond ( (/=  (distof (get_tile "flow_cumd")) nil)(rtos (/  (/ (* (distof (get_tile "flow_cumd")) 0.011574074) 1000) (/ (*  Pi (expt (/ (distof (get_tile "diameter")) 1000) 2)) 4)) 2 2)  )  (t "0" )  ))
		  
	  (start_list "material_selections")
       (mapcar 'add_list material_selections)
       (end_list)
       (if (setq pos (vl-position mst-last-sel material_selections))
         (set_tile "material_selections" (itoa pos))
         (set_tile "material_selections" "0")   ; default 
       )
	   (set_tile "ccoeff" 
	   (cond ((= (get_tile "material_selections") "0")(set_tile "ccoeff" (rtos cdin 2 2)))
	         ((= (get_tile "material_selections") "1")(set_tile "ccoeff" (rtos cdio 2 2)))
			 ((= (get_tile "material_selections") "2")(set_tile "ccoeff" (rtos cpvc 2 2)))
			 ((= (get_tile "material_selections") "3")(set_tile "ccoeff" (rtos chdpe 2 2)))
			 ((= (get_tile "material_selections") "4")(set_tile "ccoeff" (rtos ccs 2 2)))
			 ((= (get_tile "material_selections") "5")(set_tile "ccoeff" (rtos cac 2 2)))
			 ((= (get_tile "material_selections") "6")(set_tile "ccoeff" (rtos cfrp 2 2)))
			 ((= (get_tile "material_selections") "7")(set_tile "ccoeff" (rtos cwi 2 2)))
			 ((= (get_tile "material_selections") "8")(set_tile "ccoeff" (rtos cc 2 2)))
			 ((= (get_tile "material_selections") "9")(set_tile "ccoeff" (rtos cb 2 2)))
			 ((= (get_tile "material_selections") "10")(set_tile "ccoeff" (rtos cci 2 2)))
			 ((= (get_tile "material_selections") "11")(set_tile "ccoeff" (rtos cclp 2 2)))
	   ))
		  (setq flow_lps (distof (get_tile "flow_lps")))
		  (setq velocity (distof (get_tile "velocity")))
		  (setq diameter (distof (get_tile "diameter")))
		  (setq percent_loss (/ (* (expt velocity 1.85) 682.5) (* (expt (distof (get_tile "ccoeff")) 1.85) (expt (/ diameter 1000) 1.167) ) ))
		  
		  (set_tile "pipelength"  (cond ( (/= pl_length nil) (rtos pl_length 2 3) )  (t "0") )  )
		  
		  (if (/=  pl_length nil)(setq tot_fric_loss (/ (* percent_loss pl_length) 100)) (setq tot_fric_loss 0))
		  (set_tile "tot_fric_loss" (rtos tot_fric_loss 2 3))
		  (setq tot_fitt_loss (/ (* (expt velocity 2) k_tot) (* 2 9.81)))
		  (set_tile "tot_fitt_loss" (rtos tot_fitt_loss 2 3))
		  (setq tot_loss (+ tot_fric_loss tot_fitt_loss))
		  (set_tile "tot_loss" (rtos tot_loss 2 3))
		  (if (/=  pl_length nil)(setq hyd_grad (* (/ tot_loss pl_length) 1000)) (setq hyd_grad 0))
		  (set_tile "hyd_grad" (rtos hyd_grad 2 3))
		  
   		  (set_tile "angtol"  (if (= angtol nil) (cond 
		   ((and (>= (get_tile "diameter") "1") (<= (get_tile "diameter") "160"))   (set_tile "angtol" "5"))
	       ((and (>= (get_tile "diameter") "200") (<= (get_tile "diameter") "300"))   (set_tile "angtol" "4"))
	       ((and (>= (get_tile "diameter") "350") (<= (get_tile "diameter") "600"))   (set_tile "angtol" "3"))
	       ((and (>= (get_tile "diameter") "700") (<= (get_tile "diameter") "800"))   (set_tile "angtol" "2"))
        	 ((and (>= (get_tile "diameter") "900") (<= (get_tile "diameter") "1100"))   (set_tile "angtol" "1.5"))
	       ((and (>= (get_tile "diameter") "1200") (<= (get_tile "diameter") "1800"))   (set_tile "angtol" "1.5"))
	     	( (/= angtol nil) (rtos angtol 2 2) )
            (t (set_tile "angtol" "1.5"))			
		   ) (set_tile "angtol" (rtos angtol 2 2)) ))
		   
		   		   
		   
		   
		  
  
  ; ;;--- Set the override mode to be enabled or disabled
  ; (changemode)
  ; (changechainagemode)
  ; (changeautoanno)
  ;;--- If an action event occurs, do this...
  ; (action_tile "mode" "(changemode) (changeautoanno)")
  ; (action_tile "allowchainage" "(changechainagemode)")
  ; (action_tile "allowchainageprofile" "(changechainagemode)")
  ; (action_tile "auto_anno_b" "(changeautoanno)")
  ; (action_tile "diameter" "(changeautoanno)")
  ; (action_tile "about" "(aboutfunc)")
  (saveVars)
  (calcu)
  (mode_tile "disp" 1)
  (mode_tile "flow_cumhr" 1)
  (mode_tile "flow_gpm" 1)
  (mode_tile "flow_lps" 1)
  (mode_tile "velocity" 1)
  (mode_tile "ccoeff" 1)
  (mode_tile "tot_fric_loss" 1)
  (mode_tile "tot_fitt_loss" 1)
  (mode_tile "tot_loss" 1)
  (mode_tile "hyd_grad" 1)
  (mode_tile "nb90" 1)
  (mode_tile "nb45" 1)
  (mode_tile "nb22" 1)
  (mode_tile "nb11" 1)
  (mode_tile "nbnsd" 1)
  (mode_tile "pipelength" 1)
  (mode_tile "nofsaves" 1)
  (action_tile "material_selections"  "(saveVars)(change_material)(setq mst-last-sel (nth (atoi $value) material_selections)) (calcu)")
  ;(action_tile  "diameter"   " (changeangtol)")
  (action_tile  "Q_unit1"   "(saveVars)(calcu)")
  (action_tile  "kccoeff"   "(saveVars) (nest1)  (change_material) (calcu)")
  (action_tile  "goalseek"   "(saveVars)(nest2) (calcu)")
  (action_tile  "goalseekvd"   "(saveVars)(nest5) (calcu)")
  (action_tile  "pipelengthoverride"   "(saveVars)(nest4) (calcu)")
  (action_tile  "nelbowoverride"   "(saveVars)(nest3) (calcu)")
  (action_tile "SHA"  "(saveVars)  (done_dialog 4)")
  (action_tile "SVA"  "(saveVars)  (done_dialog 5)")
  (action_tile "savecalc"  "(saveVars) (calcu) (save_entry)")
  (action_tile "export"  "(saveVars) (calcu) (HR-export-data)")
  (action_tile "reset_entries" "(reset_entries)")
   
  (action_tile "calc"  "(saveVars)(calcu)")
  (action_tile "about" "(aboutfunc)")
  (action_tile "cancel"  "(done_dialog 0)")
  ; (action_tile "help"  "(wp_help)")
  ;;--- Display the dialog box
 (setq flag (start_dialog))


  
  ;;;--- Start the routine
  (if(= flag 4)
    (progn
	(getha)
	(id_pline_ang ha-ename)
	(setq pl_length (vla-get-length (vlax-ename->vla-object ha-ename)))
 
 
 (setq k_valv_n_tee  (+  (* nbfv kbfv) (* ngv kgv) (* nglv kglv) (* nav kav) (* ntr ktr) (* ntb ktb)))
 (setq k_tot  (+ kb_tot kvb_tot k_valv_n_tee adkcoeff)) 

 
 (princ)

 )

 )
 (if(= flag 5)
    (progn
	(find_v_angles)
    (setq k_valv_n_tee  (+  (* nbfv kbfv) (* ngv kgv) (* nglv kglv) (* nav kav) (* ntr ktr) (* ntb ktb)))
    (setq k_tot  (+ kb_tot kvb_tot k_valv_n_tee adkcoeff))
   
	)
  )


 )
   ;;;-- Unload the dialog box
   (unload_dialog dcl_id)
   (recvar)
   (princ)
 )
 ; (prompt "\nWP (Pre-Release VER. 1.0 Date: 05-1-2017)" )
 ; (prompt "\nLisp to create Water profiles." )
 ; (prompt "\nDeveloped by Ibrahim El Shar." )
 ; (prompt "\n> Type WP to start." )
 ;(princ "\nWP (Pre-Release VER. 1.0 Date: 05-02-2017) \nAutoLisp to create Pressurized Lines Profiles \n\nDesigned and Created by Ibrahim El Shar \nContact: ibrahim.elshar@gmail.com \n\nCopyright \U+00A9 2017. All rights reserved. \nType WP to start. ")
