function ep_method_logic1, $
	xd,yd,tg,beam,bmag,NO_TRI, $ ; IN
	mean_angle_out, stdev_angle_out, $ ; OUT
	out_out, $ ; OUT
	status_out, msg_out, method_out, $ ; OUT
	edi6, classA_towards, nonA_towards, $ ; OUT
	classA_away, nonA_away, $ ; OUT
	ambig_180
	common ep_envar_con_cb
	
	;******************************************************************
	; Perform the ToF Regime Test using only ToF-worthy beams
	;******************************************************************
	; itof = 0    Test not performed (too few ToF-worthy beams)
	;             output variables not meaningful
	; itof = 1    Test performed, success, output variables meaningful
	; itof = 2    Test performed, failure, output variables meaningful
	; itof = 3    Test not performed (Mean/Stdev Angle Nan Error)
	;             output variables not meaningful
	itof = ep_tof_regime_test ( $
		xd, yd, beam, bmag, $ ; IN
		mean_angle, stdev_angle, msg_out) ; OUT
	if (itof eq 1 or itof eq 2) then begin
		mean_angle_out = mean_angle
		stdev_angle_out = stdev_angle
	endif

	;******************************************************************
	; Perform Poorman's ToF if itof = 1, continue with TRI if itof=0 or
	; itof=2, abort if itof=3
	;******************************************************************
	if (itof eq 1) then begin
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
				if (edi4_pmt(1)/edi4_pmt(0) gt 1.) then $
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
		if (itof eq 0 or itof eq 2) then begin
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
	return, 1
end




