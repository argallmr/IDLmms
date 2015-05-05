function ep_parallel_test, $
	xd, yd, alpha, $ ; IN
	mean_angle, stdev_angle, ifin=ifin ; OUT

	common ep_envar_con_cb

	; xd and yd are scalars: X and Y of the target, in meters
	; alpha is a 1-D array:
	;    alpha(i) = firing angle of gun for beam i, in radians

	; Calculate towards and away AS IF THE BEAMS ARE FIRED FROM THE
	; SPACECRAFT CENTER, NOT THE GUN LOCATION!!!  IMPORTANT DIFFERENCE
	; FROM THE REAL TOWARDS/AWAY DISTINCTION!!!
	nbeam = n_elements(alpha)
	that = [xd,yd]                  ; [2]
	tmag = sqrt(total(that^2))      ; scalar
	that = that/tmag                ; [2], Unit length
	that = make_array(nbeam,/float,value=1.)#that ; [nbeam,2], Unit length
	ghat = [[cos(alpha)],[sin(alpha)]] ; [nbeam,2], Unit length
	dot = total(that*ghat,2)        ; [nbeam], dot>0 towards, dot<0 away

	to = where(dot ge 0)
	aw = where(dot lt 0)

	; Start with the original firing directions
	alp = alpha

	; Flip all of the away beams
	if (aw(0) ne -1) then alp(aw) = alp(aw) + !pi

	; All of the beams should now be within 180 degrees of each other, but the
	; edge of this 180 degree fan can have any numerical value.
	; Need to rotate this fan until all of the angular values are between
	; 0 and 180 so that an average can be performed on the angles.  This
	; rotation is one that puts the target vector at 90 degrees.
	tangle = atan(yd,xd)            ; [-180,180]
	rotangle = !pi/2. - tangle
	alp = alp + rotangle

	; Could at this point have some angles greater than 360 (say alp is
	; 340 and rotangle is 50, for instance).  So make sure they're all
	; between 0 and 180.
	x = cos(alp)
	y = sin(alp)
	alp = atan(y,x)

	; OK, NOW all beams are within 180 degrees of each other, AND they
	; have numerical values ranging from 0 to 180.  We can now take the
	; average and standard deviation:

	mean_angle = mean(alp) - rotangle
	stdev_angle = stddev(alp)

	; For whatever reason, had some trouble with infinity here
	if (finite(mean_angle) and finite(stdev_angle)) then begin
		ifin = 1
		if (stdev_angle le pp_para_anglim_rad) then $
			iret = 1 $
		else $
			iret = 0
	endif $
	else begin
		ifin = 0
		iret = 0
	endelse

	return, iret
end
