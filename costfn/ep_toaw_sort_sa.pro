; docformat = 'rst'
;
; NAME:
;       ep_toaw_sort_sa
;
;+
;   Determine the mean firing direction.
;
;   SIDE-EFFECTS:
;       BEAM.TOAW_INIT is set to +/-1 for toward/away beams.
;
; :Params:
;       BEAM:           in, required, type=struct
;                       Structure with beam information.
;       IDUSE:          in, required, type=struct
;                       Indices into `BEAM` indicating the beams to use.
;       PHITO:          out, required, type=fltarr
;                       Angle of the mean firing direction.
;       STDEV:          out, required, type=struct
;                       Standard deviation of the mean firing direction.
;
; :Common Blocks:
;   ep_envar_con_cb
;       PP_BFILL              - Scalar integer fill value
;       PP_TOFCLASS_NBEAM_MIN - Minimum number of beams for a single gun that can be used for ToF
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015-09-14  -   Written by Matthew Argall. Adapted from ep_toaw_sort_sa
;                           from Cluster's bestarg.
;-
pro ep_toaw_sort_sa, beam, iduse, phito, stdev
	compile_opt idl2

	; The indice array "iduse" has already been checked to meet the
	; requirment that n_elements(iduse) >= pp_nbeam_min
	
	;Beam firing angles projected into BPP
	alpha = beam.alpha[iduse]

	;
	; Sort the beams into towards/away using just the phase angles and
	; Hans' iterative approach.  At this point, the towards/away assigment
	; is to within 180 degrees.  To know which class is really towards and
	; which one is really away, you have to examine the estimated
	; single-runner times of flight, which is done outside of this routine
	; (see e.g. ep_richmans_tof_sa.pro).  Here we just want indices for
	; the two classes.
	;

	;Determine which beams are fired towards IDTO and away IDAW
	ep_hav_sorter, alpha*!radeg, idto, idaw
	beam = MrStruct_AddTags(beam, 'toaw', 0US)
	if n_elements(idto) eq 0 then idto = -1
	if n_elements(idaw) eq 0 then idaw = -1

	;Either IDTO or IDAW could be [-1], but not both.
	;   - This is because at least one beam is given, and all beams
	;     are either toward or away.
	if (idto[0] eq -1) $
		then nto = 0 $
		else nto = n_elements(idto)
	if (idaw[0] eq -1) $
		then naw = 0 $
		else naw = n_elements(idaw)

	;At least one beam in each class
	if (nto gt 0 && naw gt 0) then begin

		; These initial assignments need to be on the same indice footing
		; as all of the other beam structure arrays
		beam.toaw[iduse[idto]] =  1
		beam.toaw[iduse[idaw]] = -1

		; Calculate the mean vector for each class
		xto = mean(cos(alpha[idto]))
		yto = mean(sin(alpha[idto]))

		xaw = mean(cos(alpha[idaw]))
		yaw = mean(sin(alpha[idaw]))

		; Rotate the away class average vector by 180 degrees to have the same
		; sense as the towards class average vector.  This is done (as opposed to
		; rotating the towards class one) so that idto and idaw correspond in the
		; correct way to phito, since phito is initially treated as
		; the towards direction in later codes.
		xaw = -1.0 * xaw
		yaw = -1.0 * yaw

		; Average these two average vectors together
		xavg = (xto+xaw)/2.0
		yavg = (yto+yaw)/2.0

		; Average phase angle associated with the towards direction
		phito = atan(yavg, xavg)           ; [-pi,pi], rad

		; Compute stdev with respect to this phito, after having put all of
		; the vectors in the same hemisphere
		xs = [cos(alpha[idto]), -1.0 * cos(alpha[idaw])]
		ys = [sin(alpha[idto]), -1.0 * sin(alpha[idaw])]

		;Obtain the angle between the firing vectors "v" and the mean firing direction "V"
		;   -  v is already a unit vector
		;   - When finished, force rounding errors to +/-1 by dividing by abs().
		;   1) Dot v into V
		;         v dot V_hat = v dot V/|V|
		;                     = (vx*Vx + vy*Vy) / sqrt(Vx^2 + Vy^2)
		;   2) Find the angle
		;         theta = acos(v dot V)
		dots = (xs*xavg + ys*yavg)
		dots = dots / sqrt(xavg^2 + yavg^2)
		idum = where(dots gt 1.0 or dots lt -1.0)
		if (idum[0] ne -1) then dots[idum] = dots[idum] / abs(dots[idum])
		adiff = acos(dots)          ; [0-!pi]

		;Standard deviation of the angle between v and V
		;   - ADIFF is the difference between each firing angle and the mean firing angle
		stdev = sqrt( total(adiff^2) / float(n_elements(xs)-1) )

	;TOWARD
	endif else if nto gt 0 then begin
		;Rotate away beams by 180 degrees and average
		beam.toaw[iduse[idto]] = 1
		xto = mean(cos(alpha[idto]))
		yto = mean(sin(alpha[idto]))
		xavg = xto
		yavg = yto
		
		;Components of each firing vector
		phito = atan(yavg,xavg) ; [-pi,pi], rad
		xs = [cos(alpha[idto])]
		ys = [sin(alpha[idto])]

		;Dot firing vectors into mean firing direction to get angular difference
		dots = (xs*xavg + ys*yavg)
		dots = dots/sqrt(xavg^2+yavg^2)
		idum = where(dots gt 1.0 or dots lt -1.0)
		if (idum[0] ne -1) then $
			dots[idum] = dots[idum]/abs(dots[idum])*1.0
		adiff = acos(dots)      ; [0-!pi]

		;Compute standard deviation
		stdev = sqrt(total(adiff^2) / float(n_elements(xs)-1))
		
	;AWAY
	endif else begin
		;Rotate away beams by 180 degrees and average
		beam.toaw[iduse[idaw]] = -1
		xaw = mean(cos(alpha[idaw]))
		yaw = mean(sin(alpha[idaw]))
		xaw = -1.*xaw
		yaw = -1.*yaw
		xavg = xaw
		yavg = yaw
		
		;Components of each firing vector
		phito = atan(yavg,xavg) ; [-pi,pi], rad
		xs = [-1.*cos(alpha[idaw])]
		ys = [-1.*sin(alpha[idaw])]

		;Dot firing vectors into mean firing direction to get angular difference
		dots = (xs*xavg + ys*yavg)
		dots = dots/sqrt(xavg^2+yavg^2)
		idum = where(dots gt 1.0 or dots lt -1.0)
		if (idum[0] ne -1) then $
			dots[idum] = dots[idum]/abs(dots[idum])*1.0
		adiff = acos(dots)      ; [0-!pi]

		;Compute standard deviation
		stdev = sqrt(total(adiff^2) / float(n_elements(xs)-1))
	endelse
end
