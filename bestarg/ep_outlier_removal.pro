pro ep_outlier_removal, $
	status, beam, $
	fdoe_nopen, sigratio_nopen

	common ep_envar_con_cb

	; Identify outliers, first for triangulation, then for ToF:
	; Consider the contribution fdoe of a beam
	; to the chi^2 of triangulation
	; evaluated at the best target of the robust estimation
	; as the square of a Gaussian random variable.
	; Determine maxd, the largest value of fdoe.
	; Calculate the probability, ptail, for fdoe >= maxd
	; Now consider the number, ntail, of beams with fdoe >= maxd
	; as another random variable.
	; The observed value of ntail is 1, since there is exactly one beam
	; with fdoe = maxd >= maxd.
	; The theoretical mean and standard deviation of ntail can be derived
	; from ptail using binomial statistics.
	; Identify the beam with fdoe = maxd as outlier if the observed ntail=1
	; is higher than the sum of its theoretical mean and standard deviation.
	; If this is the case the identification as outlier is done
	; with a confidence of more than 84%.
	; If the beam with fdoe = maxd has been sorted out then repeat
	; the procedure with the remaining set of beams.
	; If beam im is a triangulation outlier (out(im) = 2) then it must not be used
	; for ToF analysis. A ToF outlier (out(im) = 1) may be used for triangulation.
	;******************************************************************

	status = 0
	if (not pp_outlier_removal) then $
		goto, skip_or_1

	;=========Determine traingulation outliers=====================
	z = where (beam.tri_ok eq 1)     ; Beams OK for TRI analysis
	if z(0) ne -1 then $
		mm_le1 = n_elements(z) $
	else $
		message, 'This should not happen...mm_le1 should be at least pp_nbeam_min'

	; # of points allowed to be removed
	; Must meet the following two conditions:
	;    n_rem  le  long(pp_outlier_maxperc/100.*mm_le1)
	;    n_rem  le  mm_le1-pp_nbeam_min
	n_rem_allowed = min ([long (pp_outlier_maxperc / 100.*mm_le1), mm_le1-pp_nbeam_min])
	goon = 1
	mm1 = mm_le1
	n_rem = 0

	while (goon and (n_rem lt n_rem_allowed)) do begin

		; NO!    maxd = max(fdoe,immax)
		; fdoe_nopen was only calculated where
		; beam.tri_ok eq 1 (otherwise fdoe=0)
		; so we don't have to
		; check the value of beam.tri_ok here
		; Now using fdoe_nopen, 20010810

		maxd = max (fdoe_nopen, immax)

		ptail = 2 * gaussint (-sqrt (maxd * (mm1-1) / total (fdoe_nopen)))
		if 1 gt mm1*ptail+sqrt(mm1*ptail*(1-ptail)) then begin
			beam.out(immax) = 2     ; This beam won't be used for anything
			; NO!        sigratio(immax) = 0.    ; Zero-ing contribution from this outlier
			sigratio_nopen(immax) = 0. ; Zero-ing
			; NO!        fdoe(immax) = 0.        ; Zero-ing
			fdoe_nopen(immax) = 0.  ; Zero-ing
			mm1 = mm1 - 1           ; Beam counter for 'good' tri beams
			n_rem = n_rem + 1
		endif $
		else begin
				goon = 0
		endelse
	endwhile

	if (mm1 lt pp_nbeam_min) then begin
		status = 0                  ; Failure
		return
	endif

skip_or_1:
	status = 1
	return
end
