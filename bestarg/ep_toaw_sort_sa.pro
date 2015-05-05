pro ep_toaw_sort_sa, $
	beam, iduse, $ ; IN
	phito, stdev ; OUT

	; The indice array "iduse" has already been checked to meet the
	; requirment that n_elements(iduse) >= pp_nbeam_min
	alpha = beam.alpha(iduse)

	; Sort the beams into towards/away using just the phase angles and
	; Hans' iterative approach.  At this point, the towards/away assigment
	; is to within 180 degrees.  To know which class is really towards and
	; which one is really away, you have to examine the estimated
	; single-runner times of flight, which is done outside of this routine
	; (see e.g. ep_richmans_tof_sa.pro).  Here we just want indices for
	; the two classes.

	; NaN Hunt Noch einmal==========
	; s = ''
	; read, s, prompt='NaN Hunt'
	; i = where(finite(alpha) eq 1)
	; if (i(0) eq -1) then $
	;   stop
	; NaN Hunt Noch einmal==========

	ep_hav_sorter, $
		alpha*!radeg, idto, idaw

	; Either idto or idaw could be [-1], but not both.
	if (idto(0) eq -1) then $
		nto = 0 $
	else $
		nto = n_elements(idto)
	if (idaw(0) eq -1) then $
		naw = 0 $
	else $
		naw = n_elements(idaw)

	if (nto ge 1 and naw ge 1) then begin ; At least one in each class

		; These initial assignments need to be on the same indice footing
		; as all of the other beam structure arrays
		beam.toaw_init (iduse (idto)) =  1
		beam.toaw_init (iduse (idaw)) = -1

		; Calculate the mean vector for each class
		xto = mean(cos(alpha(idto)))
		yto = mean(sin(alpha(idto)))

		xaw = mean(cos(alpha(idaw)))
		yaw = mean(sin(alpha(idaw)))

		; Rotate the away class average vector by 180 degrees to have the same
		; sense as the towards class average vector.  This is done (as opposed to
		; rotating the towards class one) so that idto and idaw correspond in the
		; correct way to phito, since phito is initially treated as
		; the towards direction in later codes.
		xaw = -1.*xaw
		yaw = -1.*yaw

		; Average these two average vectors together
		xavg = (xto+xaw)/2.
		yavg = (yto+yaw)/2.

		; Average phase angle associated with the towards direction
		phito = atan(yavg,xavg)           ; [-pi,pi], rad

		; Compute stdev with respect to this phito, after having put all of
		; the vectors in the same hemisphere
		xs = [cos(alpha(idto)),-1.*cos(alpha(idaw))]
		ys = [sin(alpha(idto)),-1.*sin(alpha(idaw))]

		dots = (xs*xavg + ys*yavg)
		dots = dots/sqrt(xavg^2+yavg^2)
		idum = where(dots gt 1.0 or dots lt -1.0)
		if (idum(0) ne -1) then dots(idum) = dots(idum)/abs(dots(idum))*1.0
		adiff = acos(dots)          ; [0-!pi]

		stdev = sqrt(total(adiff^2)/float(n_elements(xs)-1))

		; NaN Hunt===========
		; 20010813, SC3
		;    if (mean(beam.btime(iduse)) ge 6.*3600.+5.*60.+5.) then stop
		; NaN Hunt===========
	endif $
	else begin
		if (nto ne 0 and naw eq 0) then begin ; Towards only
			beam.toaw_init(iduse(idto)) = 1
			xto = mean(cos(alpha(idto)))
			yto = mean(sin(alpha(idto)))
			xavg = xto
			yavg = yto
			phito = atan(yavg,xavg) ; [-pi,pi], rad
			xs = [cos(alpha(idto))]
			ys = [sin(alpha(idto))]

			dots = (xs*xavg + ys*yavg)
			dots = dots/sqrt(xavg^2+yavg^2)
			idum = where(dots gt 1.0 or dots lt -1.0)
			if (idum(0) ne -1) then $
				dots(idum) = dots(idum)/abs(dots(idum))*1.0
			adiff = acos(dots)      ; [0-!pi]

			stdev = sqrt(total(adiff^2)/float(n_elements(xs)-1))
		endif $
		else begin            ; Away only
			beam.toaw_init(iduse(idaw)) = -1
			xaw = mean(cos(alpha(idaw)))
			yaw = mean(sin(alpha(idaw)))
			xaw = -1.*xaw
			yaw = -1.*yaw
			xavg = xaw
			yavg = yaw
			phito = atan(yavg,xavg) ; [-pi,pi], rad
			xs = [-1.*cos(alpha(idaw))]
			ys = [-1.*sin(alpha(idaw))]

			dots = (xs*xavg + ys*yavg)
			dots = dots/sqrt(xavg^2+yavg^2)
			idum = where(dots gt 1.0 or dots lt -1.0)
			if (idum(0) ne -1) then $
				dots(idum) = dots(idum)/abs(dots(idum))*1.0
			adiff = acos(dots)      ; [0-!pi]

			stdev = sqrt(total(adiff^2)/float(n_elements(xs)-1))
		endelse
	endelse

	return
end
