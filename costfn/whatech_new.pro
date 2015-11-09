; docformat = 'rst'
;
; NAME:
;       whatech_new
;
;+
;   Determine the maximum radius to be used for triangulation grids.
;
;   SIDE EFFECTS:
;      `DATA1`[1,*] is redefined to be::
;          0.0 = quality >= 2
;          1.0 = quality <  2
;
; :Params:
;       DATA1:          in, required, type=7xN fltarr
;                       Array returned by EDI_PISO_ONECHUNK.
;                           data1[0,k] = gun ID (1 or 2) for beam k
;                           data1[1,k] = quality of beam k, as determined by on-board software
;                           data1[2,k] = gun X position for beam k
;                           data1[3,k] = gun Y position for beam k
;                           data1[4,k] = firing angle of beam k
;                           data1[5,k] = time of flight of beam k
;                           data1[6,k] = order of beam k (single runner or double, triple, ...)
;       ERG:            in, optional, type=double
;                       Not used. Beam energy in keV. Retrieved from common variable pp_eindex2keV
;       NMCHIP:         in, required, type=long
;                       Not used. Chip length -- the product of correlator n and m.
;       BASE:           in, required, type=float
;                       Diameter of spacecraft (meters)
;       RMAX:           out, required, type=long
;                       Maximum radius (meters) to be used for the grid method. If beams
;                           pass a case-by-case threshold test with pp_tri_ang_thresh_1,
;                           and a mean test against pp_tri_ang_thresh_2, then
;                           RMAX = `BASE` + beam baseline. Otherwise, RMAX = `BASE`.
;
; :Common Blocks:
;   EP_ENVAR_CON_CB
;       pp_deg2rad          - Convert degrees to radians.
;       pp_tri_ang_thresh_1 - Beam angle separation threshold for using triangulation
;       pp_tri_ang_thresh_2 - Mean beam angle separation threshopl for using triangulation
;       pp_control          - Referenced in commented sections
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
;       2015-10-07  -   Written by Matthew Argall. Adapted from whatech_new written
;                           for Cluster's bestarg.
;-
pro whatech_new, data1, erg, nmchip, base, rmax

common ep_envar_con_cb

	;Extract data
	mm         = n_elements(data1[0,*])
	qual       = fix(reform(data1[1,*]))
	qual       = 0.0 * (qual ge 2) + 1.0 * (qual lt 2)
	data1[1,*] = qual
	xg         = reform(data1[2,*])
	yg         = reform(data1[3,*])
	alpha      = reform(data1[4,*]) * pp_deg2rad
	;pp_control  alpha=reform(data1(4,*))*!pi/180

	;
	; The error of the beam firing angle is assumed to be 1 degree:
	; Now controlled by the pp_control.pro include file
	; pp_control  acfir= 1.*!pi/180
	; acfir= pp_acfir_rad
	;

	;
	; The triangulation point of a pair of beams has a relative error in distance
	; of 1/3 (1/9) if the angle between both beams equals 4.2 (13.) times acfir.
	; Triangulation is considered as well bahaved if there is at least one beam pair
	; with an angle difference exceeding 4.2*acfir and the average angle difference
	; exceeds 2.1*acfir.
	;

	;Compute the baseline for each beam with respect to all other beams.
	difalp = fltarr(mm,mm)
	tribas = fltarr(mm,mm)
	for im = 0, mm-1 do begin
		;DIFALP will be in the range [0, 369]
		difalp[*,im] = abs(alpha - alpha[im])
		tribas[*,im] = abs((xg - xg[im]) * sin(alpha) - (yg - yg[im]) * cos(alpha))
		;tribas(j,k) is the baseline (perpendicular to beam j) of beam pair j,k.
	endfor
	
	;Force DIFALP into the range [0,90]
	difalp = difalp*(difalp lt !pi)+(2*!pi-difalp)*(difalp ge !pi)
	difalp = difalp*(difalp lt !pi/2)+(!pi-difalp)*(difalp ge !pi/2)

	;pp_control  tripair = where(difalp gt 4.2*acfir)
	;pp_control  goodtri = total(difalp)/mm/(mm-1) gt 2.1*acfir and tripair(0) ne -1
	
	;Compare DIFALP to triangulation thresholds
	;   - Must meet a raw angular threshold on a case-by-case basis
	;   - Must meet an average threshold
	;       The angular difference between a beam and itself is zero, so does not
	;       contribute to the average. Thus, averaging over the first dimension
	;       involves MM-1 points. From there, we average over the second dimension,
	;       which involves the average deviation of all MM beams.
	tripair = where(difalp gt pp_tri_ang_thresh_1)
	goodtri = ( total(difalp)/mm/(mm-1) gt pp_tri_ang_thresh_2 ) and (tripair[0] ne -1)

	;Radius of grid for triangulation method.
	rmax = base
	if goodtri then begin
		tribas = tribas[tripair] / tan(difalp[tripair]+1e-4)
		;tribas(j,k) is roughly the distance to the triangulation point of pair j,k.
		rmax = rmax + max(tribas)
	endif
end
