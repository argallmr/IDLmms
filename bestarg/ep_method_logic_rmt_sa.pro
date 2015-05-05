function ep_method_logic_rmt_sa, $
	checkpar=checkpar, beam, $ ; IN
	mean_angle_out, stdev_angle_out, $ ; OUT
	out_out, bestord_out, $ ; OUT
	status_out, msg_out, method_out, $ ; OUT
	edi6, ambig_180, $ ; OUT
	classA_towards, nonA_towards, $ ; OUT
	classA_away, nonA_away, $ ; OUT
	beam_used, perr3
	common ep_envar_con_cb

	checkpar = keyword_set(checkpar)

	;******************************************************************
	; Perform the to/aw sorting, phito/stdev calculation
	;******************************************************************
	; 1) Which beams do we use for the to/aw sorting and phito/stdev
	;    calculation? THIS MUST BE A SUPERSET OF OR IDENTICAL TO IDUSE_RMT
	;    DEFINED BELOW!
	;    Because for every beam in iduse_rmt, the towards/away assignment
	;    must have been made before ep_richmans_tof_sa.pro is called.
	iduse = lindgen (n_elements (beam.maxchan))
	ep_toaw_sort_sa, $
		beam, iduse, $ ; IN
		phito, stdev                  ; OUT
	mean_angle_out = phito
	stdev_angle_out = stdev

	;******************************************************************
	; Branch to TRI if we're not in Forced RMT (i.e. checkpar=1),
	; and beams fail parallelism test
	;******************************************************************
	if ( checkpar and (stdev gt pp_para_anglim_rad) ) then $
		goto, tri_continue

	;******************************************************************
	; We are HERE because of one of the following:
	; 1) Forced RMT desired, parallelism calculated, but not checked
	; 2) Parallelism test passed
	; Regardless of which one, there will be no branching to TRI below
	; Begin Prep-work for RMT
	; Which beams do we use for Richman's ToF? HAV uses:
	;    1) MaxChan = 7
	;    2) Qual >= 2
	;    3) Short code only
	;    4) Class "A" and "B" only (exclude "C", "D", "G", "H")
	;    5) Include those beams which may have been designated TRI
	;       outliers (don't check 'out' variable)
	;******************************************************************
	iduse_rmt = where ( $
		beam.maxchan eq 7 and $
		beam.qual ge 2 and $
		beam.code_type eq 0 and $
		(beam.class_init eq 'A' or $
		 beam.class_init eq 'B'), nrmt)

	; Check: Enough beams for RMT
	NO_RMT_25 = 0                   ; Enough to begin with?
	NO_RMT_26 = 0                   ; Enough in each to/away class?
	if (nrmt eq 0) then begin
		NO_RMT_25 = 1
	endif $
	else begin
		if (nrmt lt 2*pp_tofclass_nbeam_min) then begin
			NO_RMT_25 = 1
		endif $
		else begin
			ito = where (beam.toaw_init (iduse_rmt) eq  1, nto)
			iaw = where (beam.toaw_init (iduse_rmt) eq -1, naw)
			if (nto lt pp_tofclass_nbeam_min or naw lt pp_tofclass_nbeam_min) then $
				NO_RMT_26 = 1
		endelse
	endelse

	; NO_RMT_25 = 1 means there are not enought beams to start with
	; NO_RMT_26 = 1 means that there are not enough beams in each to/aw class.
	; In either case, RMT is not possible at all.

	if (NO_RMT_25) then begin
		if (checkpar) then begin
			; pp_method=7 (TRI/RMT Logic)
			; Parallel beams (see earlier check)
			; RMT not possible
			; Beams too parallel for TRI
			status_out = 17
		endif $
		else begin
			; pp_method=8 or 9 (Forced RMT)
			; Parallelness of beams unchecked
			; RMT not possible
			 status_out = 25
		endelse

		msg_out = pp_pstat(status_out)
		return, 0                   ; 0 = Do not continue with TRI
	endif $
	else $
		if (NO_RMT_26) then begin
		if (checkpar) then begin
			; pp_method=7 (TRI/RMT Logic)
			; Parallel beams (see earlier check)
			; RMT not possible
			; Beams too parallel for TRI
			status_out = 14
		endif $
		else begin
			; pp_method=8 or 9 (Forced RMT)
			; Parallelness of beams unchecked
			; RMT not possible
			status_out = 26
		endelse

		msg_out = pp_pstat (status_out)
		return, 0                   ; 0 = Do not continue with TRI
	endif

	;================================================================
	; RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT
	method_out = 3
	irmt = ep_richmans_tof_sa ( $
		beam, iduse_rmt, $ ; IN
		phito, stdev, $ ; IN
		msg_rmt, edi6_rmt, out_rmt, bestord_rmt, $
		classA_towards, nonA_towards, $ ; OUT
		classA_away, nonA_away) ; OUT

	if (irmt eq 3) then begin       ; RMT Success
		edi6 = edi6_rmt
		if (edi6_rmt(1)/edi6_rmt(0) gt 1.) then $
			ambig_180 = 1 $
			else $
				ambig_180 = 0
		out_out = out_rmt

		beam_used = make_array (size=size (out_out),value=0)
		ii = where (out_out eq 0)
		if (ii(0) ne -1) then $
			beam_used(ii) = 1

		bestord_out = bestord_rmt
		status_out = 3              ; RMT Success
		msg_out = pp_pstat (status_out)

		ep_recalc_aerror, $
			edi6_rmt(0), edi6_rmt(2), $
			beam_used, bestord_out, beam.GDU_locX, beam.GDU_locY, beam.alpha, perr3

	endif $
	else begin
		status_out = irmt
		msg_out = msg_rmt
	endelse

	return, 0                       ; 0 = Do not continue with TRI
	; RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT RMT
	;================================================================

tri_continue:
	return, 1                       ; 1 = Continue with TRI
end
