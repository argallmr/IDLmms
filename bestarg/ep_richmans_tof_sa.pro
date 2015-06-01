function ep_richmans_tof_sa, $
	beam, iduse, $ ; IN
	phito, stdev, $ ; IN
	msg_rmt, edi6_rmt, out_rmt, bestord_rmt, $ ; OUT
	classA_towards, nonA_towards, $ ; OUT
	classA_away, nonA_away ; OUT
	;+
	; INPUT:
	; beam        = structure of beam quantities
	; iduse       = Indices (to be used for the 'beam' structure variables)
	;               which indicate which beams to use in the analysis.
	; phito       = Mean angle as initial guess for towards direction, radians
	; stdev       = Standard deviation of beam angles from to/aw sorting algorithm, radians
	;
	; OUTPUT:
	; edi6_rmt(0) = drift step magnitude, meters
	; edi6_rmt(1) = drift step magnitude error, meters
	; edi6_rmt(2) = drift step azimuthal angle, degrees
	; edi6_rmt(3) = drift step azimuthal angle error, deg.
	; edi6_rmt(4) = gyrotime [microsec]
	; edi6_rmt(5) = gyrotime error [microsec]
	;-

	common ep_envar_con_cb

	; Pre-define some output numbers
	classA_towards = pp_bfill
	nonA_towards = pp_bfill
	classA_away = pp_bfill
	nonA_away = pp_bfill

	;================================================
	; Extract only the 'good' beams which will be used.
	; The indice array "iduse" has already been checked to meet the
	; minimum beam number requirement for each of the to/aw classes.
	id = iduse
	nbeam = n_elements(id)

	; RMT-specific requirement:
	;     Use lowest permissible order for "B" beams
	order_lp = reform(beam.posord(0,id)) ; Same as "order" for A-beams, lowest permissible order for B-beams, "1" for C-beams

	alpha = beam.alpha(id)          ; radians
	estof = beam.runest_estof(*,id) ; DIM=[pp_maxorder_runest,nbeam], microsec, Estimated single-runner time-of-flight for each possible order assignment
	estg = beam.runest_estg(id)     ; DIM=[nbeam], microsec, Estimated gyrotime

	; Output the average of all of the gyrotimes used for this calculation
	tgyro_rmt = mean(estg)

	; Define the estimated single-runner ToFs
	tof = fltarr(nbeam)
	for i=0,nbeam-1 do tof(i) = estof(order_lp(i)-1,i)

	btime = beam.btime(id)          ; DIM=[nbeam]

	;diag if (min(btime) ge 7.*3600.+4.*60.+13.) then begin
	;diag     print, ep_time_handler(min(btime),'ssm','vax_string',date='20010922')
	;diag     print, order_lp
	;diag     stop
	;diag endif

	tof_in = beam.tof(id)            ; DIM=[nbeam]
	class = beam.class(id)           ; DIM=[nbeam]
	class_init = beam.class_init(id) ; DIM=[nbeam]
	maxchan = beam.maxchan(id)       ; DIM=[nbeam]
	toaw_init = beam.toaw_init(id)   ; DIM=[nbeam]

	phi_best = phito
	to = where(toaw_init eq 1, icto)
	aw = where(toaw_init eq -1, icaw)

	;=================================================================
	; THESE NEXT TWO CHECKS SHOULDN'T BE NECESSARY, BUT I'M JUST BEING
	; CAUTIOUS HERE
	if ((icto+icaw) ne nbeam) then begin
		irmt = 28
		msg_rmt = pp_pstat(irmt)
		return, irmt
	endif

	if ((to(0) eq -1) or (aw(0) eq -1) or             $
		(n_elements(to) lt pp_tofclass_nbeam_min) or    $
		(n_elements(aw) lt pp_tofclass_nbeam_min)) then begin
		irmt = 26
		msg_rmt = pp_pstat(irmt)
		return, irmt
	endif
	;=================================================================


	; Construct the towards and away times-of-flight RELATIVE TO THE
	; GYROTIME TREND, in the hopes that this detrending of the data will
	; improve our attempts at calculating the true dtof
	tof_to_rel = tof(to) - estg(to) ; These values will be mostly positive with an average greater than tof_aw_rel if towards/away assignment is correct

	tof_aw_rel = tof(aw) - estg(aw) ; These values will be mostly negative with an average less than tof_to_rel if towards/away assignment is correct

	; If mean(tof_to_rel) < mean(tof_aw_rel) then we need to flip the
	; assignment
	swi = 0
	to_final = to                   ; Indicies for the [nbeam]-sized arrays
	aw_final = aw                   ; Indicies for the [nbeam]-sized arrays

	if (mean(tof_to_rel) lt mean(tof_aw_rel)) then begin
		swi = 1
		to_final = aw               ; Indicies for the [nbeam]-sized arrays
		aw_final = to               ; Indicies for the [nbeam]-sized arrays
		tof_to_rel = tof(to_final) - estg(to_final)
		tof_aw_rel = tof(aw_final) - estg(aw_final)
		phi_best = phi_best + !pi
	endif

	toaw = intarr(nbeam)
	toaw(to_final) = 1
	toaw(aw_final) = -1

	; Propagate out this final towards/away assignment
	beam.toaw_final(id(to_final)) = 1
	beam.toaw_final(id(aw_final)) = -1

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

	if (tof_estruc.status ne 0) then begin
		irmt = 27                   ;21
		msg_rmt = pp_pstat(irmt)+': '+tof_estruc.msg
		return, irmt
	endif

	; Select which error on dtof you want to use to define the error on
	; the drift step magnitude
	h = 'tof_estruc.err_'
	s = ['t68','t90','t95','nr']
	t = s(pp_rmt_dtof_error)
	istat = execute('dtof_error = '+h+t)

	; Output
	dmag_best = tof_estruc.dtof*beam.vb/2. ; meters
	dmag_sigma_best = dtof_error*beam.vb/2. ; meters
	phi_best = atan(sin(phi_best),cos(phi_best)) ; [-pi,pi], rad
	phi_sigma_best = stdev          ; rad

	edi6_rmt = [dmag_best, dmag_sigma_best, phi_best * !radeg, phi_sigma_best * !radeg, tgyro_rmt, pp_rfill]

	; Statistics===================================================
	idum = where(toaw eq 1 and class eq 'A', classA_towards)
	idum = where(toaw eq -1 and class eq 'A', classA_away)
	idum = where(toaw eq 1 and class ne 'A', nonA_towards)
	idum = where(toaw eq -1 and class ne 'A', nonA_away)
	;===============================================================

	goto, skip_hans_diag

	if (min(btime) ge 18.*3600.+7.*60.+27.) then begin
		openw, unit, '15-Feb-2001_hans_out.txt', /get_lun
		printf, unit, 'tof_estruc.dtof [microsec]: ', tof_estruc.dtof
		printf, unit, 'vb [m/microsec]: ', beam.vb
		printf, unit, 'Tgyro from <tgyros> [microsec]: ', tgyro_rmt
		printf, unit, 'dmag = tof_estruc.dtof*vb/2. [m]: ', dmag_best
		printf, unit, 'dmag_sigma_best [m]: ', dmag_sigma_best
		printf, unit, 'dvec [m]: ', dmag_best*cos(phi_best), dmag_best*sin(phi_best), 0.

		printf, unit, '==========================================================='
		printf, unit, '#','O','G','ToF-Mea','Beam-Time','MC','C','IC','LPO','ToF-Est','Tgyro','T/A',format='(a2,1x,a1,1x,a1,1x,a7,1x,a9,1x,a2,1x,a2,1x,a2,1x,a3,1x,a7,1x,a7,1x,a3)'
		printf, unit, '--','-','-','-------','---------','--','--','--','---','-------','-------','---',format='(a2,1x,a1,1x,a1,1x,a7,1x,a9,1x,a2,1x,a2,1x,a2,1x,a3,1x,a7,1x,a7,1x,a3)'
		for i=0,nbeam-1 do begin
			printf, unit, $
			i, beam.out(id(i)), beam.gunid(id(i)), tof_in(i), btime(i), maxchan(i), class(i), class_init(i), order_lp(i),tof(i), estg(i), toaw(i), $
			format='(i2,1x,i1,1x,i1,1x,d7.3,1x,d9.3,1x,i2,1x,a2,1x,a2,1x,i3,1x,d7.3,1x,d7.3,1x,i3)'
		endfor

		free_lun, unit
	endif

skip_hans_diag:

	;================================================================
	; Return the 'out' array so that we keep a paper trail of which beams
	; were used in this RMT analysis

	; Initialize all to "6"
	out_rmt = make_array(n_elements(beam.out),/int,value=6)

	; Assign "0" to those actually used for this RMT calculation
	out_rmt(id) = 0

	; Put back the "2"s (triangulation outliers) and "3"s (noClassC
	; outliers) if necessary (probably not at this point in the code, but
	; we're just being safe.)
	i2 = where(beam.out eq 2)
	if (i2(0) ne -1) then $
		out_rmt(i2) = 2
	i3 = where(beam.out eq 3)
	if (i3(0) ne -1) $
		then out_rmt(i3) = 3

	;================================================================
	; Return the 'bestord' array so that we know which orders were used
	; for this analysis
	bestord_rmt = make_array(n_elements(beam.bestord),/int,value=0)
	bestord_rmt(id) = order_lp

	;================================================================
	; Successful Return
	irmt = 3
	msg_rmt = pp_pstat(irmt)
	return, irmt
end
