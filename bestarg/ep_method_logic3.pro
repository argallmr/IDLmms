function ep_method_logic3, $
	xd,yd,tg,beam,bmag,NO_TRI, $ ; IN
	mean_angle_out, stdev_angle_out, $ ; OUT
	out_out, $ ; OUT
	status_out, msg_out, method_out, $ ; OUT
	edi6, classA_towards, nonA_towards, $ ; OUT
	classA_away, nonA_away, $ ; OUT
	ambig_180

	common ep_envar_con_cb

	;******************************************************************
	; Perform "The" Parallelism Test using ALL worthy beams
	; Even the "C" beams could be used here because the angular info of
	; the "C" beams isn't suspect, only their ToFs are.
	;******************************************************************
	id_tri = where(beam.tri_ok eq 1) ; Should be at least pp_nbeam_min
	alpha = beam.alpha(id_tri)
	ipar = ep_parallel_test ( $
		xd, yd, alpha, $ ; IN
		mean_angle, stdev_angle, ifin=ifin) ; OUT
	if (ifin) then begin
		mean_angle_out = mean_angle
		stdev_angle_out = stdev_angle
	endif

	;******************************************************************
	; Perform Poorman's ToF if ipar = 1, else continue with TRI
	;******************************************************************
	if (ipar and ifin) then begin
		method_out = 1
		ipmt = ep_poormans_tof ( $
			beam, $ ; IN
			mean_angle, stdev_angle, $ ; IN
			msg_pmt, edi4_pmt, out_pmt, $ ; OUT
			cat=classA_towards, nat=nonA_towards, $
			caa=classA_away, naa=nonA_away, $
			ismt=ismt, edi4_smt=edi4_smt, out_smt=out_smt, $
			msg_smt=msg_smt) ; OUT

		if (ismt eq 2) then begin
			status_out = 2          ; SMT Success
			msg_out = pp_pstat(status_out)
			out_out = out_smt
			method_out = 2
			edi6 = [edi4_smt,tg,pp_rfill]
			if (edi4_smt(1)/edi4_smt(0) gt 1.) then $
				ambig_180 = 1 $
			else $
				ambig_180 = 0
		endif $
		else $
			if (ipmt eq 1) then begin
				status_out = 1          ; PMT Success
				msg_out = pp_pstat(status_out)
				out_out = out_pmt
				method_out = 1
				edi6 = [edi4_pmt,tg,pp_rfill]
				if (edi4_smt(1)/edi4_smt(0) gt 1.) then $
					ambig_180 = 1 $
				else $
					ambig_180 = 0
			endif $
			else begin
				status_out = 32
				msg_out = pp_pstat(status_out)+': PMT Reason:'+msg_pmt+', SMT Reason:'+msg_smt
			endelse

		return, 0                   ; 0 = Do not continue with TRI
	endif $
	else $
		if (not ipar and ifin) then begin
			if (NO_TRI) then begin
				status_out = 10
				msg_out = pp_pstat(status_out)
				return, 0               ; 0 = Do not continue with TRI
			endif $
			else begin
				goto, tri_continue
			endelse
		endif $
		else begin
			status_out = 53
			msg_out = pp_pstat(status_out)
			return, 0                   ; 0 = Do not continue with TRI
		endelse

tri_continue:
	return, 1                       ; 1 = Continue with TRI
end
