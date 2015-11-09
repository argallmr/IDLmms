; docformat = 'rst'
;
; NAME:
;       mms_edi_tri_rmax
;
;+
;   Determine the maximum radius to be used for triangulation grids.
;
;   The triangulation point of a pair of beams has a relative error in distance
;   of 1/3 (1/9) if the angle between both beams equals 4.2 (13.) times acfir.
;   Triangulation is considered as well bahaved if there is at least one beam pair
;   with an angle difference exceeding 4.2*acfir and the average angle difference
;   exceeds 2.1*acfir.
;
;
; :Params:
;       XG:             in, required, type=fltarr
;                       X position (meters) of the guns in BPP
;       YG:             in, required, type=fltarr
;                       Y position (meters) of the guns in BPP
;       ALPHA:          in, required, type=fltarr
;                       Firing angle (degrees) in BPP.
;       BASE:           in, optional, type=double
;                       Diameter of spacecraft, meters
;
; :Keywords:
;       ACFIR:          in, optional, type=float, default=1.0
;                       The error of the beam firing angle (degrees).
;       THRESH1:        in, optional, type=float, default=4.2*`ACFIR`
;                       At least one beam pair must have an angle difference exceeding
;                           `THRESH1` in order for triangulation to be well-behaved.  This
;                           ensures that, at least for this pair, the triangulation point
;                           has a relative error in distance of 1/3. (radians)
;       THRESH2:        in, optional, type=float, default=2.1*`ACFIR`
;                       In addition to `THRESH1`, the average angle difference of all of
;                           the beams must exceed `THRESH2` in order for triangulation to
;                           be well-behaved. (radians)
;
; :Returns:
;       RMAX:           out, required, type=long
;                       Maximum radius (meters) to be used for the grid method. If beams
;                           pass a case-by-case threshold test with `THRESH`,
;                           and a mean test against `THRESH2`, then
;                           RMAX = `BASE` + beam baseline. Otherwise, RMAX = `BASE`.
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
;       2015-10-10  -   Written by Matthew Argall. Adapted from whatech_new written
;                           for Cluster's bestarg.
;-
function mms_edi_tri_rmax, xg, yg, alpha, base, $
ACFIR=acfir, $
THRESH1=thresh1, $
THRESH2=thresh2
	compile_opt idl2
	on_error, 2
	
	;Convert degrees to radians
	deg2rad = !dpi / 180.0D
	
	;Defaults
	mm = n_elements(xg)
	if n_elements(acfir)   eq 0 then acfir   = 1.0
	if n_elements(thresh1) eq 0 then thresh1 = 4.2 * acfir * deg2rad
	if n_elements(thresh2) eq 0 then thresh2 = 2.1 * acfir * deg2rad

	;Compute the baseline for each beam with respect to all other beams.
;	difalp = fltarr(mm,mm)
;	tribas = fltarr(mm,mm)

	;
	;tribas(j,k) is the baseline (perpendicular to beam j) of beam pair j,k.
	;   - If you rotate the coordinate system by ALPHA, the x-axis will lie
	;     along the beam, and the y-axis will be perpendicular to it.
	;   - Rotating the baseline by the same amount, its x- and y-components
	;     the length of the baseline parallel and perpendicular to the beam.
	;   - We are interested in the perpendicular projection.
	;
	;        |x'|  = |  cos(alpha)  sin(alpha) | |(x1-x0)|
	;        |y'|    | -sin(alpha)  cos(alpha) | |(y1-y0)|
	;
	;        y' = -(x1-x0)*sin(alpha) + (y1-y0)*cos(alpha)
	;
	;   - The extra minus sign comes from the fact that the beam is fired
	;     away from the spacecraft while the separation vector points into
	;     the spacecraft. It does not matter, though, because we are
	;     interested in the absolute perpendicular distance.
	;
	_x      = rebin(xg, mm, mm)
	_y      = rebin(yg, mm, mm)
	_alpha  = rebin(alpha*deg2rad, mm, mm)
	difalp  = abs( alpha - transpose(alpha) )
	tribase = abs(_x - transpose(_x))*sin(_alpha) - abs(_y - transpose(_y))*cos(_alpha)

;	for im = 0, mm-1 do begin
;		;DIFALP will be in the range [0, 369]
;		difalp[*,im] = abs(alpha - alpha[im])
;		tribas[*,im] = abs((xg - xg[im]) * sin(alpha) - (yg - yg[im]) * cos(alpha))
;		;tribas(j,k) is the baseline (perpendicular to beam j) of beam pair j,k.
;	endfor
	
	;Force DIFALP into the range [0,90]
	difalp = difalp*(difalp lt !pi)   + (2*!pi-difalp)*(difalp ge !pi)
	difalp = difalp*(difalp lt !pi/2) +   (!pi-difalp)*(difalp ge !pi/2)
	
	;Compare DIFALP to triangulation thresholds
	;   - Must meet a raw angular threshold on a case-by-case basis
	;   - Must meet an average threshold
	;       The angular difference between a beam and itself is zero, so does not
	;       contribute to the average. Thus, averaging over the first dimension
	;       involves MM-1 points. From there, we average over the second dimension,
	;       which involves the average deviation of all MM beams.
	tripair = where(difalp gt thresh1)
	goodtri = ( total(difalp)/mm/(mm-1) gt thresh2 ) and (tripair[0] ne -1)

	;Radius of grid for triangulation method.
	rmax = base
	if goodtri then begin
		tribas = tribas[tripair] / tan(difalp[tripair]+1e-4)
		;tribas(j,k) is roughly the distance to the triangulation point of pair j,k.
		rmax = rmax + max(tribas)
	endif
	
	return, rmax
end
