function ep_simultan_tof, $
	btime_to, btime_aw, tof_to, tof_aw, $ ; IN
	tg_to, tg_aw, idpt_to, idpt_aw, $ ; IN
	phi_best_in, phi_stdev_in, beam, $ ; IN
	out_smt, edi4_smt, msg_smt ; OUT

	common ep_envar_con_cb

	edi4_smt = make_array(4,/float,value=pp_rfill)

	; Create the two identity arrays
	id_to = make_array(n_elements(btime_to),/int,value=1)
	id_aw = make_array(n_elements(btime_aw),/int,value=0)

	; Merge the classes
	btime = [btime_to,btime_aw]
	tof = [tof_to,tof_aw]
	id = [id_to,id_aw]
	tg = [tg_to,tg_aw]
	idpt = [idpt_to,idpt_aw]        ; ID papertrail
	tof_dtrend = [tof_to-tg_to,tof_aw-tg_aw]

	; Sort these arrays
	isort = sort(btime)
	btime = btime(isort)
	tof = tof(isort)
	id = id(isort)
	tg = tg(isort)
	idpt = idpt(isort)
	tof_dtrend = tof_dtrend(isort)

	; Difference these arrays
	n = n_elements(btime)
	dt = btime(1:n-1) - btime(0:n-2)
	dtof = tof(1:n-1) - tof(0:n-2)
	did = id(1:n-1) - id(0:n-2)
	dtg = tg(1:n-1) - tg(0:n-2)
	idpt_r = idpt(1:n-1)            ; Right, where differences are Right-Left
	idpt_l = idpt(0:n-2)            ; Left
	tof_dtrend_r = tof_dtrend(1:n-1)
	tof_dtrend_l = tof_dtrend(0:n-2)

	; Do we have enough simultaneous pairs?
	ipairs = where(abs(dt) le pp_dt_smt_sec and did ne 0)
	npair = n_elements(ipairs)
	if (npair lt pp_npair_min_smt or ipairs(0) eq -1) then begin
		ismt = 41
		msg_smt = pp_pstat(ismt)
		return, ismt
	endif

	; Only work with the valid pairs
	dtof = dtof(ipairs)
	did = did(ipairs)
	dtg = dtg(ipairs)
	tof_dtrend_r_save = tof_dtrend_r(ipairs)
	tof_dtrend_l_save = tof_dtrend_l(ipairs)

	;================================================================
	; Keep track of those beams used in out_smt
	idpt_r = idpt_r(ipairs)
	idpt_l = idpt_l(ipairs)
	idpt_used = [idpt_r,idpt_l]
	; Initialize all to "5"
	out_smt = make_array(n_elements(beam.out),/int,value=5)
	; Assign "0" to those actually used for this SMT calculation
	out_smt(idpt_used) = 0
	; Put back the "2"s (triangulation outliers) and "3"s (noClassC outliers)
	i2 = where(beam.out eq 2)
	if (i2(0) ne -1) then out_smt(i2) = 2
	i3 = where(beam.out eq 3)
	if (i3(0) ne -1) then out_smt(i3) = 3

	;================================================================
	; Make sure that the dtof's and dtg's are "towards minus away", that
	; tof_dtrend_r are all towards, and tof_dtrend_l are all away.

	tof_dtrend_r = tof_dtrend_r_save
	tof_dtrend_l = tof_dtrend_l_save
	wrong_way = where(did lt 0)
	if (wrong_way(0) ne -1) then begin
		dtof(wrong_way) = -1.*dtof(wrong_way)
		dtg(wrong_way) = -1.*dtg(wrong_way)
		tof_dtrend_r(wrong_way) = tof_dtrend_l_save(wrong_way)
		tof_dtrend_l(wrong_way) = tof_dtrend_r_save(wrong_way)
	endif

	; Define dToF = (ToF_to - ToF_aw) - (Tg_to - Tg_aw)
	; to account for the fact that the gyro time could vary between the
	; towards point of the pair and the away point.
	dtof = dtof - dtg

	; Find the average of the dtofs
	dtof_avg = total(dtof)/float(npair)

	; If the average is less than zero, then our original
	; towards and away assignments were wrong.  We need to do
	; a flip here.
	tof_dtrend_r_save = tof_dtrend_r
	tof_dtrend_l_save = tof_dtrend_l
	phi_best = phi_best_in
	if (dtof_avg lt 0) then begin
		dtof = -1.*dtof
		dtof_avg = total(dtof)/float(npair)
		phi_best = phi_best_in + !pi
		tof_dtrend_r = tof_dtrend_l_save
		tof_dtrend_l = tof_dtrend_r_save
	endif

	; Check that dtof and dtof_check are the same!  They're supposed to be!
	; check dtof_check = tof_dtrend_r - tof_dtrend_l
	; check stop

	; Use Hans' TofError.pro routine to find an error for dtof_avg
	; 14.03.2002: Hans changed the interface
	;     tof_estruc = TofError(dtof)
	tof_estruc = TofError(tof_dtrend_r, tof_dtrend_l)

	if (tof_estruc.status ne 0) then begin
		ismt = 42
		msg_smt = pp_pstat(ismt)+': '+tof_estruc.msg
		return, ismt
	endif

	; Select which error on dtof you want to use to define the error on
	; the drift step magnitude
	h = 'tof_estruc.err_'
	s = ['t68','t90','t95','nr']
	t = s(pp_smt_dtof_error)
	istat = execute('dtof_error = '+h+t)

	; Output
	dmag_best = dtof_avg*beam.vb/2. ; meters
	dmag_sigma_best = dtof_error*beam.vb/2. ; meters
	phi_best = atan (sin(phi_best), cos(phi_best)) ; [-pi,pi], rad

	edi4_smt = [dmag_best, dmag_sigma_best, phi_best * !radeg, phi_stdev_in * !radeg]

	ismt = 2
	msg_smt = pp_pstat(ismt)
	return, ismt
end
