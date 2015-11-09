; docformat = 'rst'
;
; NAME:
;       ep_analyze_runest
;
;+
;   Analyze the results of RunEst.
;
;   NOTES:
;       `ID`, `ID1`, and `ID2` are returned by ep_prep_runest.pro
;       All other positional input parameters are returned by RunEst.pro
;
; :Params:
;       ID:             in, required, type=integer/intarr
;                       Concatenation of ID1 and ID2.
;       ID1:            in, required, type=intarr
;                       Indices for GD21 that match the following criteria::
;                           beam time is not a fill value, beam quality is GE
;                           pre-defined minimum, MAXCHAN = 7.
;       ID2:            in, required, type=intarr
;                       Indices for GD12 that match the following criteria::
;                           beam time is not a fill value, beam quality is GE
;                           pre-defined minimum, MAXCHAN = 7.
;       HAVOUT:         in, required, type=struct
;                       Structure returned by RunEst.pro
;       TOF:            in, required, type=fltarr
;                       Time of flight in micro-seconds
;       STIME:          in, required, type=dblarr
;                       Beam time in seconds since 1970-01-01
;       CTYPE:          in, required, type=bytarr
;                       Correlator type (0=short)
;       NC:             in, required, type=bytarr
;                       Correlator n (1,2,4,8,16,32,64)
;       MC:             in, required, type=bytarr
;                       Correlator m (16,8,4,2 )
;       GDU:            in, required, type=bytarr
;                       Gun from which data originated (1=GDU1, 2=GDU2)
;       RUNSTRUCT1:     in, out, required, type=structarr
;                       A structure array for GD21 quantities with one element per beam
;                           that is initialized externally, but filled internally. Values
;                           from `HAVOUT` are re-ordered and copyied into their
;                           corresponding fields, which include::
;                               'RUNORDER' - Scalar, Runner order
;                               'TG'       - Scalar, Gyrotime
;                               'PROB'     - Array, Probability of each runner order being true
;                               'FLAG'     - Array, 0 = bad, 1 = PROB > P_LIM
;                               'ESTOF'    - Array, Estimated single runner time of flight
;       RUNSTRUCT2:     in, out, required, type=structarr
;                       A structure array for GD12 quantities with one element per beam
;                           that is initialized externally, but filled internally. Values
;                           from `HAVOUT` are re-ordered and copyied into their
;                           corresponding fields, which include::
;                               'RUNORDER' - Scalar, Runner order
;                               'TG'       - Scalar, Gyrotime
;                               'PROB'     - Array, Probability of each runner order being true
;                               'FLAG'     - Array, 0 = bad, 1 = PROB > P_LIM
;                               'ESTOF'    - Array, Estimated single runner time of flight
;       STAT:           out, required, type=integer
;                       Status flag: 0=fail, 1=success
;
; :Common Blocks:
;   EDI_PISO_ONEHOUR_COMMON
;   EP_ENVAR_CON_CB
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015-10-07  -   Written by Matthew Argall. Adapted from ep_analyze_runest written
;                           Cluster's bestarg.
;-
pro ep_analyze_runest, id, id1, id2, havout, $
                       tof, st, ctype, nc, mc, gdu, $
                       runstruc1, runstruc2, stat=stat

	common edi_piso_onehour_common, btime1,btime2,atime0,atime1,t1,t1m2,VAX1,VAY1,VAX2,VAY2,I1,I2,SQ1,SQ2,E_INDEX,NNN,mm,code_type,sob,limhi,limlo,cnum,inmodstr,mxch1,mxch2

	common ep_envar_con_cb
	; --------------------------------------------------------------
	
	;Assume success. Get total number of elements.
	stat = 1
	ntot = n_elements(btime1)

	;Make sure RUNEST found all of the data
	if ( (n_elements(id) ne n_elements(havout.data.sortindex)) or $
		 (n_elements(st) ne n_elements(havout.data.sortindex)) ) then begin
		stat = 0
		return
	endif

	;Sort by time
	;   - Separate GD12 from GD21. GD12 values have indices greater than GD21.
	id      = id[havout.data.sortindex]
	id1_loc = where(id lt ntot)
	id2_loc = where(id ge ntot)

	;GD21 - CHECK
	if (id1[0] ne -1) then begin
		;Did we find all values?
		if (n_elements(id1) ne n_elements(id1_loc)) then begin
			stat = 0
			return
		endif
		
		;Times must match exactly
		tot = total(btime1[id1] - st[id1_loc])
		if (tot ne 0.d0) then begin
			stat = 0
			return
		endif
	endif
	
	;GD12 - CHECK
	if (id2(0) ne -1) then begin
		;Did we find all values?
		if (n_elements(id2) ne n_elements(id2_loc)) then begin
			stat = 0
			return
		endif
		;Times must match exactly
		tot = total(btime2[id2] - st[id2_loc])
		if (tot ne 0.d0) then begin
			stat = 0
			return
		endif
	endif

	;GD21 - Sort and copy data
	if (id1[0] ne -1) then begin
		runstruc1[id1].runorder = havout.data[id1_loc].runorder ; DET1
		runstruc1[id1].tg       = havout.data[id1_loc].tg       ; DET1
		runstruc1[id1].flag[*]  = havout.data[id1_loc].flag[*]  ; DET1
		runstruc1[id1].prob[*]  = havout.data[id1_loc].prob[*]  ; DET1
		runstruc1[id1].estof[*] = havout.data[id1_loc].estof[*] ; DET1
	endif

	;GD12 - Sort and copy data
	if (id2[0] ne -1) then begin
		runstruc2[id2].runorder = havout.data[id2_loc].runorder  ; DET2
		runstruc2[id2].tg       = havout.data[id2_loc].tg        ; DET2
		runstruc2[id2].flag[*]  = havout.data[id2_loc].flag[*]   ; DET2
		runstruc2[id2].prob[*]  = havout.data[id2_loc].prob[*]   ; DET2
		runstruc2[id2].estof[*] = havout.data[id2_loc].estof[*]  ; DET2
	endif
end