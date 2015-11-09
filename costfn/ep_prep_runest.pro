; docformat = 'rst'
;
; NAME:
;       ep_prep_runest
;
;+
;   Prepare data for RunEst. Data from GDU1 and GDU2 are created from
;   quantities in EDI_PISO_ONEHOUR_COMMON and combined into a single array.
;   GD12 data is stored before GD21 data. Data selected are::
;       1) not fill values (PP_RFILL)
;       2) meet the minimum quality requirements (PP_QUAL_MIN)
;       3) have MAXCHAN = 7
;
; :Params:
;       TOF:            out, required, type=fltarr
;                       Time of flight in micro-seconds.
;       ST:             out, optional, type=double
;                       Beam time in seconds since 1970-01-01.
;       CTYPE:          out, required, type=integer/intarr
;                       Correlator code type (0=short, 1=long).
;       NC:             out, required, type=intarr
;                       Correlator n (1,2,4,8,16,32,64).
;       MC:             out, required, type=intarr
;                       Correlator m (16,8,4,2 ).
;       GDU:            out, required, type=intarr
;                       Gun/Detector number.
;       QUAL:           out, required, type=integer/intarr
;                       Beam quality (1, 2, or 3).
;       ID:             out, required, type=integer/intarr
;                       Concatenation of ID1 and ID2
;       ID1:            out, required, type=intarr
;                       Indices for GD21 that match the following criteria::
;                           beam time is not a fill value, beam quality is GE
;                           pre-defined minimum, MAXCHAN = 7.
;       ID2:            out, required, type=intarr
;                       Indices for GD12 that match the following criteria::
;                           beam time is not a fill value, beam quality is GE
;                           pre-defined minimum, MAXCHAN = 7.
;       STATUS:         out, required, type=integer
;                       Status flag: 0=fail, 1=pass
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
;       2015-10-07  -   Written by Matthew Argall. Adapted from ep_prep_runest written
;                           Cluster's bestarg.
;-
pro ep_prep_runest, tof, st, ctype, nc, mc, gdu, qual, id, id1, id2, status

	common edi_piso_onehour_common, btime1,btime2,atime0,atime1,t1,t1m2,VAX1,VAY1,VAX2,VAY2,I1,I2,SQ1,SQ2,E_INDEX,NNN,mm,code_type,sob,limhi,limlo,cnum,inmodstr,mxch1,mxch2

	common ep_envar_con_cb

	ntot      = n_elements(btime1)
	status    = 0                    ; Fail
	tof_scale = 1.e3/59.6046         ; convert (by dividing) tof in 59.6-ns to tof in mics (NOS)
	
	;Filter DET1 Quanitities
	id1 = where(btime1 ne pp_rfill    and $ ; Filter on DET1 quantities
	            sq1    ge pp_qual_min and $ ; NOTE: qual=1 is OK here! Hans' routines can handle it.
	            mxch1  eq 7)
	
	;Filter DET2 Quanitities
	id2 = where(btime2 ne pp_rfill    and $ ; Filter on DET2 quantities
	            sq2    ge pp_qual_min and $ ; NOTE: qual=1 is OK here! Hans' routines can handle it.
	            mxch2  eq 7)
	
	;Define values so that we can concatenate without a lot
	;of checkings.
	tof   = 0.
	st    = 0.d0
	ctype = 0
	nc    = 0
	mc    = 0
	gdu   = 0
	qual  = 0
	id    = -1L

	;Concatenate GD12 values
	if (id2[0] ne -1) then begin     ; Det2/Gun1
		id2 = id2[sort(btime2[id2])] ; This MUST be sorted here,
		                             ;  or ep_analyze_runest.pro will be
		                             ;  screwed up...
		tof   = [tof,   (t1[id2] - t1m2[id2]) / tof_scale]
		st    = [st,    btime2[id2]]
		ctype = [ctype, code_type[id2]]
		nc    = [nc,    nnn[id2]]
		mc    = [mc,    mm[id2]]
		gdu   = [gdu,   make_array(n_elements(id2), /INT, VALUE=1)]
		qual  = [qual,  sq2(id2)]
		id    = [id,    id2 + ntot]          ; To ensure uniqueness, add ntot
		                                     ; Handled correctly in ep_analyze_runest.pro
	endif

	;Concatenate GD21 values
	if (id1[0] ne -1) then begin    ; Det1/Gun2
		id1   = id1[sort(btime1[id1])]
		tof   = [tof,   t1[id1] / tof_scale]
		st    = [st,    btime1[id1]]
		ctype = [ctype, code_type[id1]]
		nc    = [nc,    nnn[id1]]
		mc    = [mc,    mm[id1]]
		gdu   = [gdu,   make_array(n_elements(id1),/int,value=2)]
		qual  = [qual,  sq1[id1]]
		id    = [id,    id1]
	endif

	;Trim off the dummy element that was used for convenience in concatentation.
	n = n_elements(tof)
	if (n gt 1) then begin
		status = 1                  ; Success
		tof    = tof[1:n-1]
		st     = st[1:n-1]
		ctype  = ctype[1:n-1]
		nc     = nc[1:n-1]
		mc     = mc[1:n-1]
		gdu    = gdu[1:n-1]
		qual   = qual[1:n-1]
		id     = id[1:n-1]
	endif
end
