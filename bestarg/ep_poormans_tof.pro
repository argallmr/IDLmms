function ep_poormans_tof, $
	beam, $ ; IN
	mean_angle, stdev_angle, $ ; IN
	msg_pmt, edi4_pmt, out_pmt, $  ; OUT
	cat=classA_towards, nat=nonA_towards, $
	caa=classA_away, naa=nonA_away, $
	ismt=ismt, edi4_smt=edi4_smt, out_smt=out_smt, $
	msg_smt=msg_smt

	;+
	; INPUT:
	; beam         = structure of beam quantities
	; mean_angle   = Mean angle from parallelism test, radians
	; stdev_angle  = Standard deviation of beam angles from parallelism test, radians
	;
	; OUTPUT:
	; edi4_pmt(0)  = drift step magnitude, meters
	; edi4_pmt(1)  = drift step magnitude error, meters
	; edi4_pmt(2)  = drift step azimuthal angle, degrees
	; edi4_pmt(3)  = drift step azimuthal angle error, degrees
	;-

	common ep_envar_con_cb

	; Pre-define some output numbers
	classA_towards = pp_bfill
	nonA_towards = pp_bfill
	classA_away = pp_bfill
	nonA_away = pp_bfill

	;================================================
	; Extract the 'good' beams which will be used
	id = where(beam.pmt_ok eq 1)
	if (n_elements(id) lt 2*pp_tofclass_nbeam_min) then begin
		ipmt = 30
		msg_pmt = pp_pstat(ipmt)
		ismt = ipmt
		msg_smt = msg_pmt
		return, ipmt
	endif
	nbeam = n_elements(id)
	order = beam.bestord(id)
	alpha = beam.alpha(id)          ; radians
	estof = beam.runest_estof(*,id) ; microsec
	estg = beam.runest_estg(id)     ; microsec
	btime = beam.btime(id)
	tof_in = beam.tof(id)
	class = beam.class(id)
	class_init = beam.class_init(id)
	maxchan = beam.maxchan(id)
	phi_best = mean_angle           ; radians

	;================================================
	; Separate beams into towards and away classes:
	; Calculate towards and away AS IF THE BEAMS ARE FIRED FROM THE
	; SPACECRAFT CENTER, NOT THE GUN LOCATION!!!  IMPORTANT DIFFERENCE
	; FROM THE REAL TOWARDS/AWAY DISTINCTION!!!  Just look at the dot
	; product between the gun firing direction and the unit vector along
	; the mean phi angle reported by the parallelism test.  IF the
	; average of the towards times-of-flight is not greater than the
	; average of the aways, then just flip the assignments since this 180
	; degree ambiguity is inherent at this point.

	that = [cos(phi_best),sin(phi_best)] ; [2]
	tmag = sqrt(total(that^2))      ; scalar
	that = that/tmag                ; [2], Unit length
	that = make_array(nbeam,/float,value=1.)#that ; [nbeam,2], Unit length
	ghat = [[cos(alpha)],[sin(alpha)]] ; [nbeam,2], Unit length
	dot = total(that*ghat,2)        ; [nbeam], dot>0 towards, dot<0 away

	to = where(dot ge 0)
	aw = where(dot lt 0)

	if (to(0) eq -1 or aw(0) eq -1 or            $
		n_elements(to) lt pp_tofclass_nbeam_min or $
		n_elements(aw) lt pp_tofclass_nbeam_min)   then begin
		ipmt = 31
		msg_pmt = pp_pstat(ipmt)
		ismt = ipmt
		msg_smt = msg_pmt
		return, ipmt
	endif

	; Define the single-runner ToFs of these two populations
	tof = fltarr(nbeam)
	for i=0,nbeam-1 do tof(i) = estof(order(i)-1,i)
	tof_aw = tof(aw)                ; Average should be the smaller one
	tof_to = tof(to)                ; Average should be the larger one
	btime_to = btime(to)
	btime_aw = btime(aw)
	tg_to = estg(to)
	tg_aw = estg(aw)

	id_to = id(to)                  ; Keeping papertrail of who's used
	id_aw = id(aw)

	; Using the above information, branch to ep_simultan_tof.pro
	ismt = ep_simultan_tof ( $
		btime_to, btime_aw, tof_to, tof_aw, $ ; IN, not changed
		tg_to, tg_aw, id_to, id_aw, $ ; IN, not changed
		phi_best, stdev_angle, beam, $ ; IN
		out_smt, edi4_smt, msg_smt) ; OUT
	if (pp_method eq 5) then begin
		ipmt = 22
		msg_pmt = pp_pstat(ipmt)
		return, ipmt
	endif

	; If mean(tof_to) < mean(tof_aw) then we need to flip the assignment
	mean_tof_to = mean(tof_to)
	mean_tof_aw = mean(tof_aw)
	swi = 0
	if (mean_tof_to lt mean_tof_aw) then begin
		swi= 1
		tof_to = tof(aw)
		tof_aw = tof(to)
		mean_tof_to = mean(tof_to)
		mean_tof_aw = mean(tof_aw)
		phi_best = phi_best + !pi
	endif

	; Use Hans' TofError.pro routine to assess whether or not the
	; difference between the mean towards and away ToFs is significant.
	tof_estruc = TofError(tof_to, tof_aw) ;tof_estruc.dtof=avg(tof_to)-avg(tof_aw)

	;============================================================
	; tof_estruc   = { status:0, $
	;                  msg:'', $
	;                  dtof:   0.0, $  ; difference of ToF averages
	;                  err_t68:0.0, $  ; t-scaled, 68% confidence
	;                  err_t95:0.0, $  ; t-scaled, 95% confidence
	;                  err_nr: 0.0 $   ; Numerical Recipes Formula
	;                }

	if (tof_estruc.status ne 0) then begin
		ipmt = 21
		msg_pmt = pp_pstat(ipmt)+': '+tof_estruc.msg
		return, ipmt
	endif

	; Select which error on dtof you want to use to define the error on
	; the drift step magnitude
	h = 'tof_estruc.err_'
	s = ['t68','t90','t95','nr']
	t = s(pp_pmt_dtof_error)
	istat = execute('dtof_error = '+h+t)

	; Output
	dmag_best = tof_estruc.dtof*beam.vb/2. ; meters
	dmag_sigma_best = dtof_error*beam.vb/2. ; meters
	phi_best = atan(sin(phi_best),cos(phi_best)) ; [-pi,pi], rad
	phi_sigma_best = stdev_angle    ; rad

	edi4_pmt = [dmag_best, dmag_sigma_best, phi_best * !radeg, phi_sigma_best * !radeg]

	; Statistics===================================================
	if (not swi) then begin
		to1 = where(dot ge 0 and class eq 'A')
		if (to1(0) ne -1) then $
			classA_towards = n_elements(to1) $
		else $
			classA_towards = 0
		aw1 = where(dot lt 0 and class eq 'A')
		if (aw1(0) ne -1) then $
			classA_away = n_elements(aw1) $
		else $
			classA_away = 0
		to1 = where(dot ge 0 and class ne 'A')
		if (to1(0) ne -1) then $
			nonA_towards = n_elements(to1) $
		else $
			nonA_towards = 0
		aw1 = where(dot lt 0 and class ne 'A')
		if (aw1(0) ne -1) then $
			nonA_away = n_elements(aw1) $
		else $
			nonA_away = 0
	endif $
	else begin
		to1 = where(dot lt 0 and class eq 'A')
		if (to1(0) ne -1) then $
			classA_towards = n_elements(to1) $
		else $
			classA_towards = 0
		aw1 = where(dot ge 0 and class eq 'A')
		if (aw1(0) ne -1) then $
			classA_away = n_elements(aw1) $
		else $
			classA_away = 0
		to1 = where(dot lt 0 and class ne 'A')
		if (to1(0) ne -1) then $
			nonA_towards = n_elements(to1) $
		else $
			nonA_towards = 0
		aw1 = where(dot ge 0 and class ne 'A')
		if (aw1(0) ne -1) then $
			nonA_away = n_elements(aw1) $
		else $
			nonA_away = 0
	endelse
	;===============================================================

	;================================================================
	; Return the 'out' array so that we keep a paper trail of which beams
	; were used in this PMT analysis

	; Initialize all to "4"
	out_pmt = make_array(n_elements(beam.out),/int,value=4)

	; Assign "0" to those actually used for this PMT calculation
	out_pmt(id) = 0

	; Put back the "2"s (triangulation outliers) and "3"s (noClassC outliers)
	i2 = where(beam.out eq 2)
	if (i2(0) ne -1) then $
		out_pmt(i2) = 2
	i3 = where(beam.out eq 3)
	if (i3(0) ne -1) then $
		out_pmt(i3) = 3

	;================================================================
	ipmt = 1
	msg_pmt = pp_pstat(ipmt)
	return, ipmt
end
