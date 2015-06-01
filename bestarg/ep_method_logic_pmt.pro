function ep_method_logic_pmt, $
	xd,yd,tg,beam,bmag,NO_TRI, $ ; IN
	mean_angle_out, stdev_angle_out, $ ; OUT
	out_out, $ ; OUT
	status_out, msg_out, method_out, $ ; OUT
	edi6, classA_towards, nonA_towards, $ ;OUT
	classA_away, nonA_away, $ ; OUT
	ambig_180

	common ep_envar_con_cb

	method_out = 1                  ; PMT

	;******************************************************************
	; Perform "The" Parallelism Test using ALL worthy beams
	; Even the "C" beams could be used here because the angular info of
	; the "C" beams isn't suspect, only their ToFs are.
	;******************************************************************
	id_tri = where (beam.tri_ok eq 1) ; Should be at least pp_nbeam_min
	alpha = beam.alpha (id_tri)
	ipar = ep_parallel_test ( $
		xd, yd, alpha, $ ; IN
		mean_angle, stdev_angle, ifin=ifin) ; OUT

	; Carry on even if the parallelism test failed...This is forced PMT.
	if (ifin) then begin
		mean_angle_out = mean_angle
		stdev_angle_out = stdev_angle
		ipmt = ep_poormans_tof ( $
			beam, $ ; IN
			mean_angle, stdev_angle, $ ; IN
			msg_pmt, edi4_pmt, out_pmt, $ ; OUT
			cat=classA_towards, nat=nonA_towards, $
			caa=classA_away, naa=nonA_away)
		if (ipmt eq 1) then begin                 ; PMT Success
				edi6 = [edi4_pmt, tg,pp_rfill]
				if (edi4_pmt(1) / edi4_pmt(0) gt 1.) then $
					ambig_180 = 1 $
				else $
					ambig_180 = 0
				out_out = out_pmt
				status_out = 1          ; PMT Success
				msg_out = pp_pstat (status_out)
		endif $
		else begin
			status_out = ipmt
			msg_out = msg_pmt
		endelse
	endif $
	else begin
		status_out = 53
		msg_out = pp_pstat(status_out)
	endelse

	return, 0                       ; 0 = Do not continue w/ TRI
end
