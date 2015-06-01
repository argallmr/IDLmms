function ep_tof_regime_test, $
	xd, yd, beam, bmag, $ ; IN
	mean_angle_out, stdev_angle_out, msg_out ; OUT

	common ep_envar_con_cb

	; Define the PMT-worthy points
	id_pmt = where(beam.pmt_ok eq 1)

	; Check the number of beams that meet this, because our previous check
	; after outlier removal was more lenient, namely we checked that the
	; number of out le 1 beams was >= pp_nbeam_min.  This above is a bit
	; more restrictive.
	if (n_elements(id_pmt) lt 2*pp_tofclass_nbeam_min) then begin
		msg_out = 'ep_tof_regime_test: Not enough beams to start with'
		return, 0
	endif

	; Parallelism Test
	alpha = beam.alpha(id_pmt)
	ipar = ep_parallel_test ( $
		xd, yd, alpha, $ ; IN
		mean_angle, stdev_angle, ifin=ifin) ; OUT

	if (not ifin) then begin
		msg_out = 'ep_tof_regime_test: Mean/Stdev angle = NaN problem'
		return, 3
	endif $
	else begin
		mean_angle_out = mean_angle
		stdev_angle_out = stdev_angle
	endelse

	itof = 2
	msg_out = 'ep_tof_regime_test: Failed test'
	if (ipar and bmag le pp_bmag_toflim) then begin
		itof = 1
		msg_out = 'ep_tof_regime_test: Passed test'
	endif

	return, itof
end
