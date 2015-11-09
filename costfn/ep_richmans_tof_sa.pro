; docformat = 'rst'
;
; NAME:
;       ep_richmans_tof_sa
;
;+
;   Determine if we are in the time of flight regime or triangulation regime.
;
; :Params:
;       BEAM:           in, required, type=struct
;                       Structure with information from all beams.
;       IDUSE:          in, required, type=intarr
;                       Indices into `BEAM` indicating the beams to use for this analysis.
;                           Must have previously been checked to meet the minimum number
;                           of beams requirement.
;       PHITO:          in, required, type=fltarr
;                       Mean firing angle (radians) toward the virtual source point
;                           for `IDUSE` beams.
;       STDEV:          in, required, type=fltarr
;                       Standard deviation of beams angles (radians) from `PHITO`.
;       MSG_RMT:        out, required, type=string
;                       Status message taken from PP_PSTAT.
;       EDI6_RMT:       out, required, type=6x1 fltarr
;                       Results. Elements are::
;                           0 = drift step magnitude, meters
;                           1 = drift step magnitude error, meters
;                           2 = drift step azimuthal angle, radians
;                           3 = drift step azimuthal angle error, radians
;                           4 = gyrotime, micro-seconds
;                           5 = gyrotime error, micro-seconds
;       OUT_RMT:        out, required, type=intarr
;                       Flag array with the following values:
;                           0 = Used for this round of RMT
;                           2 = Triangulation outliers
;                           3 = noClassC outliers
;                           6 = Fill value (i.e. value not yet assigned)
;       BESTORD_RMT:    out, required, type=intarr
;                       Runner order for the beam set corresponding to `IDUSE`.
;                           Depends a somehow on beam class.
;       CLASSA_TOWARD:  out, required, type=integer
;                       Number of class A beams fired toward the target.
;       NONA_TOWARD:    out, required, type=integer
;                       Number of non-class A beams fired toward the target.
;       CLASSA_AWAY:    out, required, type=integer
;                       Number of class A beams fired away from the target.
;       NONA_AWAY:      out, required, type=integer
;                       Number of non-class A beams fired away from the target.
;
; :Returns:
;       TF_TRI:         Returns 1 of triangulation should be used, 0 if not.
;
; :Common Blocks:
;   ep_envar_con_cb
;       PP_BFILL          - Scalar integer fill value.
;       PP_RFILL          - Fill value used for EDI6_RMT[5]
;       PP_PSTAT          - Array of pre-defined status messages.
;       PP_RMT_DTOF_ERROR - Confidence limit on drift step magnitude to use.
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
;       2015-09-14  -   Written by Matthew Argall. Adapted from ep_richmans_tof_sa
;                           from Cluster's bestarg.
;-
function ep_richmans_tof_sa, beam,           iduse,        phito,       stdev, $
                             msg_rmt,        edi6_rmt,     out_rmt,     bestord_rmt, $
                             classA_towards, nonA_towards, classA_away, nonA_away
	compile_opt idl2
	common ep_envar_con_cb

;---------------------------------------------------------------------
; Select Data of Interest ////////////////////////////////////////////
;---------------------------------------------------------------------
	; Pre-define some output numbers
	classA_towards = pp_bfill
	nonA_towards   = pp_bfill
	classA_away    = pp_bfill
	nonA_away      = pp_bfill

	;================================================
	; Extract only the 'good' beams which will be used.
	; The indice array "iduse" has already been checked to meet the
	; minimum beam number requirement for each of the to/aw classes.
	id    = iduse                ;Not changed. No need to copy. Rename input instead
	nbeam = n_elements(id)

	; RMT-specific requirement:
	;     Use lowest permissible order for "B" beams
	order_lp   = reform(beam.posord[0,id]) ; Same as "order" for A-beams, lowest permissible order for B-beams, "1" for C-beams
	alpha      = beam.alpha[id]            ; radians
	estof      = beam.runest_estof[*,id]   ; DIM=[pp_maxorder_runest,nbeam], microsec, Estimated single-runner time-of-flight for each possible order assignment
	estg       = beam.runest_estg[id]      ; DIM=[nbeam], microsec, Estimated gyrotime
	btime      = beam.btime[id]            ; DIM=[nbeam]
	tof_in     = beam.tof[id]              ; DIM=[nbeam]
	class      = beam.class[id]            ; DIM=[nbeam]
	class_init = beam.class_init[id]       ; DIM=[nbeam]
	maxchan    = beam.maxchan[id]          ; DIM=[nbeam]
	toaw_init  = beam.toaw_init[id]        ; DIM=[nbeam]

	; Define the estimated single-runner ToFs
;	tof = estof[order_lp-1, *]
	tof = fltarr(nbeam)
	for i=0,nbeam-1 do tof[i] = estof[order_lp[i]-1,i]

	;diag if (min(btime) ge 7.*3600.+4.*60.+13.) then begin
	;diag     print, ep_time_handler(min(btime),'ssm','vax_string',date='20010922')
	;diag     print, order_lp
	;diag     stop
	;diag endif

;---------------------------------------------------------------------
; Decifer Toward and Away Beams //////////////////////////////////////
;---------------------------------------------------------------------

	;Best guess of toward direction (editable)
	;   - Extract the toward and away beams
	phi_best = phito
	ito = where(toaw_init eq  1, nto)
	iaw = where(toaw_init eq -1, naw)

	;=================================================================
	; THESE NEXT TWO CHECKS SHOULDN'T BE NECESSARY, BUT I'M JUST BEING
	; CAUTIOUS HERE
	if ((nto+naw) ne nbeam) then begin
		irmt = 28
		msg_rmt = pp_pstat[irmt]
		return, irmt
	endif

	;Make sure there are enough toward and away beams to use RMT
	if nto eq 0                     or $
	   naw eq 0                     or $
	   nto lt pp_tofclass_nbeam_min or $
	   naw lt pp_tofclass_nbeam_min $
	then begin
		irmt = 26
		msg_rmt = pp_pstat[irmt]
		return, irmt
	endif
	;=================================================================

	;
	; Construct the towards and away times-of-flight RELATIVE TO THE
	; GYROTIME TREND, in the hopes that this detrending of the data will
	; improve our attempts at calculating the true dtof
	;
	; Toward: ToF > Tg
	; Away:   ToF < Tg
	;
	tof_to_rel = tof(ito) - estg(ito) ; These values will be mostly positive if towards/away assignment is correct
	tof_aw_rel = tof(iaw) - estg(iaw) ; These values will be mostly negative if towards/away assignment is correct

	; If mean(tof_to_rel) < mean(tof_aw_rel) then we need to flip the assignment
	tf_switch = 0                     ; switch flag
	to_final  = ito                   ; Indicies for the [nbeam]-sized arrays
	aw_final  = iaw                   ; Indicies for the [nbeam]-sized arrays
	if mean(tof_to_rel) lt mean(tof_aw_rel) then begin
		tf_switch  = 1
		to_final   = iaw               ; Indicies for the [nbeam]-sized arrays
		aw_final   = ito               ; Indicies for the [nbeam]-sized arrays
		tof_to_rel = tof[to_final] - estg[to_final]
		tof_aw_rel = tof[aw_final] - estg[aw_final]
		phi_best   = phi_best + !pi
	endif

	;Used as a shortcut later instead of beam.toaway_final
	toaw           =  intarr(nbeam)
	toaw(to_final) =  1
	toaw(aw_final) = -1

	; Propagate out this final towards/away assignment
	beam.toaw_final[id[to_final]] =  1
	beam.toaw_final[id[aw_final]] = -1

;---------------------------------------------------------------------
; ToF Error //////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	; Use Hans' TofError.pro routine to assess whether or not the
	; difference between the mean towards and away ToFs is significant.
	tof_estruc = TofError(tof_to_rel, tof_aw_rel) ;tof_estruc.dtof=avg(tof_to_rel)-avg(tof_aw_rel)

	;============================================================
	;tof_estruc   = { status:0, $
	;                 msg:'', $
	;                 dtof:   0.0, $  ; difference of ToF averages
	;                 err_t68:0.0, $  ; t-scaled, 68% confidence
	;                 err_t90:0.0, $  ; t-scaled, 90% confidence
	;                 err_t95:0.0, $  ; t-scaled, 95% confidence
	;                 err_nr: 0.0 $   ; Numerical Recipes Formula
	;               }

	;If there was an error calculating the ToF Error
	if (tof_estruc.status ne 0) then begin
		irmt = 27                   ;21
		msg_rmt = pp_pstat[irmt] + ': ' + tof_estruc.msg
		return, irmt
	endif

	; Select which error on dtof you want to use to define the error on
	; the drift step magnitude
;	h     = 'tof_estruc.err_'
;	s     = ['t68','t90','t95','nr']
;	t     = s[pp_rmt_dtof_error]
;	istat = execute('dtof_error = '+h+t)
	case pp_rmt_dtof_error of
		0:    dtof_error = tof_estruct.err_t68
		1:    dtof_error = tof_estruct.err_t90
		2:    dtof_error = tof_estruct.err_t95
		3:    dtof_error = tof_estruct.err_nr
		;else: Should not be possible
	endcase

;---------------------------------------------------------------------
; Output Quantities //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Output
	;   - Drift step magnitude
	;   - Drift step magnitude error
	;   - Drift step azimuthal angle
	;   - Drift step azimuthal angle error
	;   - Average gyro-time
	;   - Average gyro-time error (PP_RFILL)
	dmag_best       = tof_estruc.dtof * beam.vb / 2.    ; meters
	dmag_sigma_best = dtof_error      * beam.vb / 2.    ; meters
	phi_best        = atan(sin(phi_best),cos(phi_best)) ; [-pi,pi], rad
	phi_sigma_best  = stdev                             ; rad
	tgyro_rmt       = mean(estg)

	;Output array of results
	edi6_rmt = [dmag_best, dmag_sigma_best, phi_best * !radeg, phi_sigma_best * !radeg, tgyro_rmt, pp_rfill]

	;Statistics -- Number of class (not) A toward/away beams
	idum = where(toaw eq  1 and class eq 'A', classA_towards, NCOMPLEMENT=nonA_towards)
	idum = where(toaw eq -1 and class eq 'A', classA_away,    NCOMPLEMENT=nonA_away)

;---------------------------------------------------------------------
; Housekeeping ///////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;
	; Return the 'out' array so that we keep a paper trail of which beams
	; were used in this RMT analysis
	;

	; Initialize all to "6"
	out_rmt = make_array(n_elements(beam.out), /INTEGER, VALUE=6)

	; Assign "0" to those actually used for this RMT calculation
	out_rmt[id] = 0

	; Put back the "2"s (triangulation outliers) and "3"s (noClassC
	; outliers) if necessary (probably not at this point in the code, but
	; we're just being safe.)
	i2 = where(beam.out eq 2, n2)
	i3 = where(beam.out eq 3, n3)
	if n2 gt 0 then out_rmt[i2] = 2
	if n3 gt 0 then out_rmt[i3] = 3

	; Return the 'bestord' array so that we know which orders were used
	; for this analysis
	bestord_rmt     = make_array(n_elements(beam.bestord), /INTEGER, VALUE=0)
	bestord_rmt[id] = order_lp

	;
	; Successful Return
	irmt    = 3
	msg_rmt = pp_pstat[irmt]
	return, irmt
end
